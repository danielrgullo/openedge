{c:/work/desenv/edi/edi-ccs-api.p} // <- persistent ?
{c:/work/desenv/edi/edi-ccs-util.p}

DEF STREAM st-ed.

DEF VAR h-acomp AS HANDLE NO-UNDO.

DEF TEMP-TABLE tt-codigos NO-UNDO
    FIELD nr-pedcli LIKE ped-venda.nr-pedcli
    FIELD codigo-refer LIKE ITEM.codigo-refer
    FIELD it-codigo LIKE ITEM.it-codigo
    INDEX id nr-pedcli codigo-refer.

DEF TEMP-TABLE tt-prog-ant NO-UNDO 
   FIELD nr-pedcli    AS CHAR 
   FIELD codigo-refer AS CHAR
   FIELD it-codigo    AS CHAR
   FIELD id-programa  LIKE edi-ped-vendas.id-programa
   FIELD dt-entrega   AS DATE
   INDEX id nr-pedcli codigo-refer id-programa.

FUNCTION fi-ignorar-pedidos     RETURNS LOG  (nr-pedcli AS CHAR) FORWARD.
FUNCTION fi-valida-pedido       RETURNS CHAR (nr-pedcli AS CHAR) FORWARD.
FUNCTION fi-corrige-nr-pedcli   RETURNS CHAR (nome-abrev AS CHAR, nr-pedcli AS CHAR) FORWARD.
FUNCTION fi-valida-item         RETURNS CHAR (codigo-refer AS CHAR) FORWARD.
FUNCTION fi-verifica-preco-item RETURNS CHAR (cod-estabel AS CHAR, nr-pedcli AS CHAR, it-codigo AS CHAR, qtd-pedida AS DECIMAL) FORWARD.

/**
 *
 *******************************************************************************************************/
PROCEDURE processa-arquivo:
   DEF INPUT PARAM c-arquivo AS CHAR NO-UNDO.
   DEF OUTPUT PARAM TABLE FOR tt-edi.
   DEF OUTPUT PARAM TABLE FOR tt-prog-ant.
// TODO: OUTPUT TT de ERROS ???

    IF c-arquivo = '' THEN 
        RETURN 'Arquivo n∆o pode ser vazio!':u.

    FILE-INFO:FILE-NAME = c-arquivo.
    IF FILE-INFO:FULL-PATHNAME = ? THEN 
        RETURN 'Arquivo n∆o existe ou n∆o tens acesso a esse arquivo!':u.

    RUN utp/ut-acomp.p PERSISTENT SET h-acomp.
    {utp/ut-liter.i Acompanhamento...}
    RUN pi-inicializar IN h-acomp (INPUT RETURN-VALUE).

   // n∆o tem forma melhor de fazer isso?
   IF INDEX(c-arquivo, "CNH") > 0 THEN          
       RUN pi-leArquivoCNH (INPUT c-arquivo).
    ELSE IF INDEX(c-arquivo, "JACTO") > 0 THEN       
       RUN pi-leArquivoRND012 (INPUT c-arquivo).
    ELSE
       RUN pi-leArquivoRND001 (INPUT c-arquivo).

    IF RETURN-VALUE = "NOK":U THEN DO:
        RUN pi-finalizar IN h-acomp.
        RETURN 'Erro ao ler arquivo !!!':u.
    END.

    // -> temp-tables para tables
    RUN pi-processa-arquivo-lido.
    IF RETURN-VALUE <> 'OK':u THEN DO:
        RUN pi-finalizar IN h-acomp.
        RETURN RETURN-VALUE.
    END.

    // -> verificaá‰es contra os pedidos
    RUN pi-compara-pedidos-e-prepara-importacao.

    RUN pi-finalizar IN h-acomp.

    RETURN 'OK':u.

END PROCEDURE.

/**
 *
 *******************************************************************************************************/
PROCEDURE pi-processa-arquivo-lido:

    DEF VAR nr-pedcli AS CHAR NO-UNDO.

    EMPTY TEMP-TABLE tt-codigos.
    EMPTY TEMP-TABLE tt-prog-ant.
    
    FIND FIRST tt-edi NO-LOCK NO-ERROR.
    IF NOT AVAIL tt-edi THEN
        RETURN "Erro lendo arquivo?".

    FIND FIRST emitente NO-LOCK
         WHERE emitente.cod-emitente = tt-edi.cod-emitente NO-ERROR.
    IF NOT AVAIL emitente THEN
        RETURN REPLACE("Emitente $1 n∆o encontrado. Erro lendo arquivo.", '$1', STRING(tt-edi.cod-emitente)).

    FIND FIRST ccs-edi-conf NO-LOCK
         WHERE ccs-edi-conf.cod-emitente = tt-edi.cod-emitente NO-ERROR.
    IF NOT AVAIL ccs-edi-conf THEN
        RETURN REPLACE("Configuraá∆o do emitente $1 n∆o encontrada. Erro lendo arquivo.", '$1', STRING(tt-edi.cod-emitente)).

      
    // TODO:
    // verificar se Ç o mesmo arquivo.
    // perguntar se quer substituir ou n∆o.
    FIND FIRST edi-ped-vendas NO-LOCK 
        WHERE edi-ped-vendas.cod-emitente = tt-edi.cod-emitente
          AND edi-ped-vendas.dt-arquivo = tt-edi.dt-movto NO-ERROR.
    IF AVAIL edi-ped-venda THEN DO:

        // j† foi lido o mesmo arquivo antes

        FOR EACH edi-ped-vendas NO-LOCK 
                WHERE edi-ped-vendas.cod-emitente = tt-edi.cod-emitente
                  AND edi-ped-vendas.dt-arquivo = tt-edi.dt-movto,
            EACH edi-ped-ent NO-LOCK 
                WHERE edi-ped-ent.cod-emitente = edi-ped-vendas.cod-emitente
                  AND edi-ped-ent.dt-arquivo   < edi-ped-vendas.dt-arquivo
                  AND edi-ped-ent.nr-pedcli    = edi-ped-vendas.nr-pedcli
                  AND edi-ped-ent.id-programa  = edi-ped-vendas.id-prog-ant:
    
            RUN pi-acompanhar IN h-acomp ("Relendo posiá∆o anterior...").
    
            CREATE tt-prog-ant.
            ASSIGN tt-prog-ant.nr-pedcli    = edi-ped-ent.nr-pedcli
                   tt-prog-ant.codigo-refer = edi-ped-ent.codigo-refer
                   tt-prog-ant.it-codigo    = edi-ped-ent.it-codigo
                   tt-prog-ant.id-programa  = edi-ped-ent.id-programa
                   tt-prog-ant.dt-entrega   = edi-ped-ent.dt-janela.
        END.

    END. ELSE DO:
    
        FOR EACH edi-ped-vendas NO-LOCK 
                WHERE edi-ped-vendas.cod-emitente = tt-edi.cod-emitente,
            EACH edi-ped-ent NO-LOCK 
                WHERE edi-ped-ent.cod-emitente = edi-ped-vendas.cod-emitente
                  AND edi-ped-ent.dt-arquivo   < edi-ped-vendas.dt-arquivo
                  AND edi-ped-ent.nr-pedcli    = edi-ped-vendas.nr-pedcli
                  AND edi-ped-ent.id-programa  = edi-ped-vendas.id-programa:
    
            RUN pi-acompanhar IN h-acomp ("Salvando posiá∆o anterior...").
    
            CREATE tt-prog-ant.
            ASSIGN tt-prog-ant.nr-pedcli    = edi-ped-ent.nr-pedcli
                   tt-prog-ant.codigo-refer = edi-ped-ent.codigo-refer
                   tt-prog-ant.it-codigo    = edi-ped-ent.it-codigo
                   tt-prog-ant.id-programa  = edi-ped-ent.id-programa
                   tt-prog-ant.dt-entrega   = edi-ped-ent.dt-janela.
        END.
    END.

    // ->
    FOR EACH tt-ped-edi NO-LOCK:

        IF fi-ignorar-pedidos(tt-ped-edi.nr-pedcli) THEN 
            NEXT.

        ASSIGN nr-pedcli = fi-corrige-nr-pedcli (INPUT emitente.nome-abrev,
                                                 INPUT tt-ped-edi.nr-pedcli).
        FIND FIRST edi-ped-vendas 
             WHERE edi-ped-vendas.cod-emitente = tt-edi.cod-emitente 
               AND edi-ped-vendas.dt-arquivo   = tt-edi.dt-movto 
               AND edi-ped-vendas.nr-pedcli    = nr-pedcli                 
               AND edi-ped-vendas.id-programa  = tt-ped-edi.id-programa
               AND edi-ped-vendas.codigo-refer = tt-ped-edi.codigo-refer    
               NO-ERROR.
        IF NOT AVAIL edi-ped-vendas THEN DO:

            RUN pi-acompanhar IN h-acomp ("Salvando programa : " + nr-pedcli).

            FIND FIRST edi-ped-vendas 
                WHERE edi-ped-vendas.cod-emitente = tt-edi.cod-emitente                                        
                  AND edi-ped-vendas.nr-pedcli    = nr-pedcli
                  //AND edi-ped-vendas.cod-estabel  = tt-pedido.cod-estabel
                  AND edi-ped-vendas.codigo-refer = tt-ped-edi.codigo-refer NO-ERROR.
            IF NOT AVAIL edi-ped-vendas THEN DO:
                CREATE edi-ped-vendas.
                ASSIGN edi-ped-vendas.cod-emitente = tt-edi.cod-emitente
                       edi-ped-vendas.dt-arquivo   = tt-edi.dt-movto
                       edi-ped-vendas.nr-pedcli    = nr-pedcli
                       edi-ped-vendas.codigo-refer = tt-ped-edi.codigo-refer
                       //  ? edi-ped-vendas.cod-estabel  = tt-pedido.cod-estabel
                       .
            END.
            IF edi-ped-vendas.id-programa  <> tt-ped-edi.id-programa THEN DO:
                IF edi-ped-vendas.id-prog-ant <> edi-ped-vendas.id-programa THEN
                    ASSIGN edi-ped-vendas.id-prog-ant = edi-ped-vendas.id-programa.

                ASSIGN edi-ped-vendas.dt-arquivo    = tt-edi.dt-movto                   
                       edi-ped-vendas.id-programa   = tt-ped-edi.id-programa
                       //edi-ped-vendas.cod-qualifica = tt-pedido.c-estado     /* getQualificacao() */
                       //edi-ped-vendas.cod-tipoforn  = tt-pedido.c-tipoforn   /* getTipoFornecimento() */
                       edi-ped-vendas.ult-nf        = tt-ped-edi.ult-nf
                       edi-ped-vendas.qtd-acum      = tt-ped-edi.qt-acumul
                       edi-ped-vendas.narrativa     = tt-ped-edi.narrativa.
            END.
        END.  // not avail edi-ped-vendas 


        // ****** Entregas ******
        FOR EACH tt-ped-edi-ent NO-LOCK
                WHERE tt-ped-edi-ent.cod-emitente = tt-ped-edi.cod-emitente
                  AND tt-ped-edi-ent.nr-pedcli = tt-ped-edi.nr-pedcli
                  AND tt-ped-edi-ent.codigo-refer = tt-ped-edi.codigo-refer:

            RUN pi-acompanhar IN h-acomp ("Salvando programa do item : " + tt-ped-edi-ent.it-codigo).

            FIND FIRST edi-ped-ent 
                WHERE edi-ped-ent.cod-emitente = tt-edi.cod-emitente
                  AND edi-ped-ent.nr-pedcli    = nr-pedcli
                  AND edi-ped-ent.id-programa  = tt-ped-edi.id-programa
                  AND edi-ped-ent.codigo-refer = tt-ped-edi.codigo-refer
                  AND edi-ped-ent.dt-entrega   = tt-ped-edi-ent.dt-entrega
                  AND edi-ped-ent.tipo-entr    = tt-ped-edi-ent.situacao NO-ERROR.
            IF NOT AVAIL edi-ped-ent THEN DO:
               CREATE edi-ped-ent.
               ASSIGN edi-ped-ent.cod-emitente  = tt-edi.cod-emitente
                      edi-ped-ent.dt-arquivo    = tt-edi.dt-movto
                      edi-ped-ent.nr-pedcli     = nr-pedcli
                      edi-ped-ent.id-programa   = tt-ped-edi.id-programa
                      edi-ped-ent.it-codigo     = tt-ped-edi-ent.it-codigo
                      edi-ped-ent.codigo-refer  = tt-ped-edi-ent.codigo-refer
                      edi-ped-ent.dt-entrega    = tt-ped-edi-ent.dt-entrega
                      edi-ped-ent.dt-janela     = tt-ped-edi-ent.dt-janela
                      edi-ped-ent.alt-tec       = tt-ped-edi-ent.alt-tec
                      edi-ped-ent.tipo-entr     = tt-ped-edi-ent.situacao
                      edi-ped-ent.qtd-pedida    = 0
                      // edi-ped-ent.cod-situacao  = ?
                      // edi-ped-ent.cod-destino   = tt-ped-edi-ent.cod-destino
                      .
            END. ELSE DO:

               IF tt-edi.cod-emitente   = 10000326 AND
                  edi-ped-ent.it-codigo = "" THEN  
                  ASSIGN edi-ped-ent.it-codigo = tt-ped-edi-ent.it-codigo.

               IF edi-ped-ent.dt-arquivo <> tt-edi.dt-movto THEN DO:
                   ASSIGN edi-ped-ent.dt-arquivo   = tt-edi.dt-movto
                          edi-ped-ent.tipo-entr    = tt-ped-edi-ent.situacao
                          edi-ped-ent.qtd-pedida   = 0
                          // ?? edi-ped-ent.cod-situacao = 
                          .
               END.
            END.
            /* sempre fica a ultima qtd, caso repita o item/entrega/situaá∆o */
            ASSIGN edi-ped-ent.qtd-pedida = tt-ped-edi-ent.qt-entrega.
        END.
    END.
    RUN pi-acompanhar IN h-acomp ("Aguarde ...").

    RETURN 'OK'.

END PROCEDURE.

/**
 *
 *******************************************************************************************************/
PROCEDURE pi-compara-pedidos-e-prepara-importacao:

   DEF BUFFER b-edi-ped-ent FOR edi-ped-ent.
   
   DEF VAR c-tipo AS CHAR NO-UNDO.
   DEF VAR c-obs  AS CHAR NO-UNDO.
   DEF VAR i-nr-entrega   AS INT NO-UNDO.
   DEF VAR i-nr-sequencia AS INT NO-UNDO.
   
   DEF VAR nr-pedcli LIKE ped-venda.nr-pedcli NO-UNDO.

   DEF VAR dt-entrega AS DATE NO-UNDO.
   DEF VAR dt-corte   AS DATE NO-UNDO.
   
   DEF VAR qtd-acum-atu AS INT NO-UNDO.
   DEF VAR qtd-acum-ped AS INT NO-UNDO.
   

   FIND FIRST tt-edi NO-LOCK NO-ERROR.
   IF NOT AVAIL tt-edi THEN
       RETURN "NOK".

   FIND FIRST emitente NO-LOCK
        WHERE emitente.cod-emitente = tt-edi.cod-emitente NO-ERROR.
   IF NOT AVAIL emitente THEN
       RETURN "NOK".

   FIND FIRST ccs-edi-conf NO-LOCK
        WHERE ccs-edi-conf.cod-emitente = tt-edi.cod-emitente NO-ERROR.
   IF NOT AVAIL ccs-edi-conf THEN
       RETURN "NOK".

   ASSIGN dt-corte = (TODAY + ccs-edi-conf.i-horizonte).
   IF dt-corte = ? THEN
       ASSIGN dt-corte = TODAY.


   OUTPUT STREAM st-ed 
       TO VALUE(fi-gera-nome-arquivo(SESSION:TEMP-DIRECTORY + "importacao_edi", "csv"))
       NO-CONVERT.

   FOR EACH edi-ped-vendas NO-LOCK 
           WHERE edi-ped-vendas.cod-emitente = tt-edi.cod-emitente
             AND edi-ped-vendas.dt-arquivo   = tt-edi.dt-movto :
       
       RUN pi-acompanhar IN h-acomp ("Verificando programa x pedido: " + 
                                     edi-ped-vendas.nr-pedcli).

       ASSIGN qtd-acum-atu = 0.
       IF ccs-edi-conf.l-qtd-acum THEN DO:

           ASSIGN qtd-acum-ped = edi-ped-vendas.qtd-acum.

           FIND FIRST ITEM NO-LOCK 
               WHERE ITEM.codigo-refer = edi-ped-vendas.codigo-refer NO-ERROR.
           IF AVAIL ITEM THEN DO:
               FIND FIRST it-fat-acum 
                   WHERE it-fat-acum.it-codigo = ITEM.it-codigo
                     AND it-fat-acum.nr-pedcli = edi-ped-vendas.nr-pedcli NO-ERROR.
               IF AVAIL it-fat-acum THEN
                   ASSIGN qtd-acum-atu = it-fat-acum.quantidade.
           END.
       END.
       
       // Caterpillar recebe EDI sempre no 10000124;
       // pi-procura-emitente corrige isso de acordo com o pedido
       RUN pi-procura-emitente (INPUT edi-ped-vendas.cod-emitente, 
                                INPUT edi-ped-vendas.nr-pedcli).

       FOR EACH edi-ped-ent NO-LOCK 
               WHERE edi-ped-ent.cod-emitente = edi-ped-vendas.cod-emitente
                 AND edi-ped-ent.dt-arquivo   = edi-ped-vendas.dt-arquivo
                 AND edi-ped-ent.nr-pedcli    = edi-ped-vendas.nr-pedcli
                 AND edi-ped-ent.id-programa  = edi-ped-vendas.id-programa
                 AND edi-ped-ent.codigo-refer = edi-ped-vendas.codigo-refer
           BY edi-ped-ent.dt-janela:

           IF edi-ped-ent.tipo-entr <= 0 THEN // previs∆o da cnh ?
               NEXT.
               
           IF edi-ped-ent.dt-janela <> ? THEN
               ASSIGN dt-entrega = edi-ped-ent.dt-janela.
           ELSE
               ASSIGN dt-entrega = edi-ped-ent.dt-entrega.
               
           RUN pi-acompanhar IN h-acomp ("Verificando entrega: " + 
                                         edi-ped-vendas.nr-pedcli + " " + 
                                         STRING(dt-entrega, '99/99/9999')).

           PUT STREAM st-ed UNFORMATTED
               SKIP
               STRING(TODAY, "99/99/9999") ";"
               STRING(TIME, "HH:MM:SS") ";"
               edi-ped-ent.dt-arquivo   ";"
               edi-ped-ent.nr-pedcli    ";"
               edi-ped-ent.codigo-refer ";"
               edi-ped-ent.it-codigo    ";"
               edi-ped-ent.dt-janela    ";"
               edi-ped-ent.dt-entrega   ";"
               edi-ped-ent.qtd-pedida   ";"
               edi-ped-ent.tipo-entr    ";".

           IF edi-ped-ent.tipo-entr <> 1 AND edi-ped-ent.tipo-entr <> 3 THEN DO: // n∆o Ç firme nem aquis. material
               IF ccs-edi-conf.i-horizonte <> 0 AND dt-entrega > dt-corte THEN DO:
                   PUT STREAM st-ed UNFORMATTED
                       ';Fora do horizonte: ' ccs-edi-conf.i-horizonte
                       SKIP.
                   NEXT.
               END.
           END.

           // tipos aceitos pelo cliente
           IF ccs-edi-conf.l-identifica[8] OR                            // se aceita desconhecido, ent∆o vai qquer tipo;
              ccs-edi-conf.i-codcanc = edi-ped-ent.tipo-entr OR          // ou pedidos cancelados;
              ccs-edi-conf.l-identifica[edi-ped-ent.tipo-entr] THEN DO:  // ou ainda Ç um tipo selecionado para aceitar 
                                                                         //   na configuraá∆o;
               ASSIGN c-tipo = ""
                      i-nr-entrega   = 1
                      i-nr-sequencia = 1
                      c-obs  = ""
                      .

               ASSIGN c-obs = fi-valida-item(edi-ped-ent.codigo-refer).
               IF c-obs <> '' THEN DO:

                   ASSIGN c-tipo = 'E'.

               END. ELSE DO:

                   ASSIGN c-obs = fi-valida-pedido(edi-ped-ent.nr-pedcli).
                   IF c-obs <> '' THEN DO:
                       ASSIGN c-tipo = 'E'.

                   END. ELSE IF edi-ped-ent.it-codigo = "" THEN DO:

                       ASSIGN c-tipo = "E"
                              c-obs  = "Item inv†lido, n∆o cadastrado ou amostra !!! "
                              .
                   END. ELSE IF dt-entrega = ? THEN DO:
    
                       ASSIGN c-tipo = "E"
                              c-obs  = "Data da programaá∆o nula (inv†lida) !!! "
                              .               
                   END. ELSE IF dt-entrega < TODAY THEN DO:

                       ASSIGN c-tipo = "E"
                              c-obs  = TRIM(c-obs + " Programaá∆o com data menor que a data atual ! N∆o Ç possivel importar !")
                              .
                   END. ELSE DO:

                       ASSIGN c-obs = fi-verifica-preco-item(edi-ped-venda.cod-estabel,
                                                             edi-ped-ent.nr-pedcli, 
                                                             edi-ped-ent.it-codigo,
                                                             edi-ped-ent.qtd-pedida).
                       IF c-obs <> '' THEN DO:
                           ASSIGN c-tipo = 'E'.
                       END.                       
                   END. 
               END.

               // Para Maxion n∆o Ç permitido adicionar itens que foram bloqueados.
               IF c-tipo = "" AND tt-edi.cod-emitente = 10005276 THEN DO:
                   FIND FIRST item-fatur-compl NO-LOCK 
                       WHERE item-fatur-compl.it-codigo = edi-ped-ent.it-codigo
                         AND item-fatur-compl.bloqueado NO-ERROR.
                   IF AVAIL item-fatur-compl THEN
                       ASSIGN c-tipo = "E"
                              c-obs  = TRIM(c-obs + " Item est† bloqueado! Verifique com a Qualidade.")
                              .
               END.

               /* ainda n∆o Ç possivel ?
               FIND FIRST ped-venda NO-LOCK 
                   WHERE ped-venda.nr-pedcli = edi-ped-vendas.nr-pedcli NO-ERROR.
               IF AVAIL ped-venda AND ped-venda.esp-ped = 1 THEN DO:
                   ASSIGN c-tipo = "E"
                          c-obs  = "Pedido Ç do tipo 'Pedido Simples' n∆o Ç permitido EDI para esses pedidos " + c-obs.
               END.
               */

               // verifica o item, se n∆o teve alguma exceá∆o 
               IF c-tipo = "" THEN DO:
                   // adicionar junto no verifica item ?
                   FIND FIRST ITEM NO-LOCK 
                       WHERE ITEM.it-codigo = edi-ped-ent.it-codigo NO-ERROR.
                   IF NOT AVAIL ITEM THEN
                       ASSIGN c-tipo = "E"
                              c-obs  = "Item n∆o encontrado no sistema!"
                              .
                   ELSE IF ITEM.cod-obsoleto <> 1 THEN
                       ASSIGN c-tipo = "E"
                              c-obs  = "Item obsoleto!"
                              .
               END.
               
               IF edi-ped-ent.qtd-pedida = 0 THEN DO:
                   ASSIGN c-tipo = "E"
                          c-obs  = "Quantidade do EDI est† zerada, programaá∆o cancelada ? Favor verificar!"
                          .
               END.

               ASSIGN nr-pedcli = edi-ped-ent.nr-pedcli.

               // tipo: cancelar a programaá∆o -> verifica se existe realmente programaá∆o para cancelar 
               IF edi-ped-ent.tipo-entr = ccs-edi-conf.i-codcanc AND ccs-edi-conf.i-codcanc <> 0 THEN DO:
                   FIND LAST ped-ent NO-LOCK 
                       WHERE ped-ent.nome-abrev   = emitente.nome-abrev
                         AND ped-ent.nr-pedcli    = edi-ped-ent.nr-pedcli
                         AND ped-ent.it-codigo    = edi-ped-ent.it-codigo
                         AND ped-ent.dt-entrega   = dt-entrega
                         AND ped-ent.cod-sit-ent  < 3 NO-ERROR.
                   IF AVAIL ped-ent THEN
                       ASSIGN c-tipo = "C"
                              c-obs  = "Programaá∆o cancelada pelo cliente !"
                              .
                   ELSE DO:
                       ASSIGN c-tipo = "E"
                              c-obs  = "Programaá∆o cancelada pelo cliente MAS sem programaá∆o na CCS!"
                              .
                   END.
               END.

               /* 2012.06.{11,12,15,21}
                  verificar se existe pedido com data de fechamento de janela igual ao atual.
                  Procura no buffer b-edi-ped-ent o ID anterior e procura seu pedido (aberto).
                  Apenas para a CATERPILLAR.
               */
               IF c-tipo = "" AND edi-ped-ent.cod-emitente = 10000124 THEN DO:

                   FIND LAST b-edi-ped-ent NO-LOCK 
                       WHERE b-edi-ped-ent.cod-emitente = edi-ped-ent.cod-emitente
                         AND b-edi-ped-ent.id-programa  = edi-ped-vendas.id-prog-ant
                         AND b-edi-ped-ent.nr-pedcli    = edi-ped-ent.nr-pedcli
                         AND b-edi-ped-ent.codigo-refer = edi-ped-ent.codigo-refer 
                         AND b-edi-ped-ent.dt-janela   <> edi-ped-ent.dt-janela   /* dt-janela  = data-abertura-janela   */
                         AND b-edi-ped-ent.dt-entrega   = edi-ped-ent.dt-entrega  /* dt-entrega = data-fechamento-janela */ 
                         NO-ERROR. 
                   IF AVAIL b-edi-ped-ent THEN DO:
   
                       FIND LAST ped-ent NO-LOCK 
                           WHERE ped-ent.nome-abrev   = emitente.nome-abrev
                             AND ped-ent.nr-pedcli    = edi-ped-ent.nr-pedcli
                             AND ped-ent.it-codigo    = edi-ped-ent.it-codigo
                             AND ped-ent.dt-entrega   = b-edi-ped-ent.dt-janela
                             AND ped-ent.cod-sit-ent <= 3 NO-ERROR.
                       IF AVAIL ped-ent AND dt-entrega <> ped-ent.dt-entrega  THEN DO:
                           ASSIGN c-tipo = "A"
                                  i-nr-entrega   = ped-ent.nr-entrega
                                  i-nr-sequencia = ped-ent.nr-sequencia
                                  .
                       END.
                   END.
               END. // c-tipo = "" AND 10000124 

               /* esses itens j† est∆o obsoletos em 2019-11
               // gambiarra para tratar diferente estes dois itens da volvo! 
               IF edi-ped-ent.it-codigo BEGINS "8.2155.01317.P" OR 
                  edi-ped-ent.it-codigo BEGINS "8.2155.01328.P" THEN DO:
                   ASSIGN nr-pedcli = TRIM(nr-pedcli) + "L". /* original deveria ser: 5500008945 */
               END.
               */

               // tudo ok -> segue ... 
               IF c-tipo = "" THEN DO:

                   /* procura entrega (ativa) */
                   FIND LAST ped-ent NO-LOCK 
                       WHERE ped-ent.nome-abrev   = emitente.nome-abrev
                         AND ped-ent.nr-pedcli    = nr-pedcli
                         AND ped-ent.it-codigo    = edi-ped-ent.it-codigo
                         AND ped-ent.dt-entrega   = dt-entrega
                         AND ped-ent.cod-sit-ent <= 3 NO-ERROR.
                   IF AVAIL ped-ent THEN DO:

                       /* se n∆o houve alteraá∆o da quantidade ent∆o pr¢ximo - verifica aberto mesmo ? */
                       IF (ped-ent.qt-pedida - ped-ent.qt-atendida) = edi-ped-ent.qtd-pedida THEN DO:
                           PUT STREAM st-ed UNFORMATTED
                               ';N∆o houve alteraá∆o' 
                               SKIP.
                           NEXT.
                       END.

                       // existe o pedido no sistema, ent∆o altera (muda p/ tipo Alteraá∆o) 
                       ASSIGN c-tipo = "A"
                              i-nr-entrega   = ped-ent.nr-entrega
                              i-nr-sequencia = ped-ent.nr-sequencia
                              .
                       // se o pedido foi atendido completamente, tem que fazer manualmente (avisa o usu†rio) 
                       // TODO: n∆o poderia adicionar nova entrega ?
                       IF (ped-ent.qt-pedida - ped-ent.qt-atendida) <= 0 THEN DO:
                           ASSIGN /*c-tipo = "A"*/
                                  c-obs  = "Quantidade n∆o pode ser alterada ! " + 
                                           "Qtd no sistema : " +  STRING(ped-ent.qt-pedida - ped-ent.qt-atendida) + " - " +
                                           "Qtd no EDI : " + STRING(edi-ped-ent.qtd-pedida)
                                            .
                       END.
                   END. ELSE DO:
                       // pedido novo, n∆o achou programaá∆o aberta nesta data. 
                       ASSIGN c-tipo = "N".
                   END.
               END. 

               PUT STREAM st-ed UNFORMATTED
                   c-tipo ";"
                   c-obs  ";"
                   .
               /* 
                  verificar acumulado 
                  OBS: este codigo pode estar incorreto, verificar com mais cuidado.
               */
               ASSIGN qtd-acum-ped = qtd-acum-ped + edi-ped-ent.qtd-pedida.
               IF ccs-edi-conf.l-qtd-acum AND qtd-acum-atu > qtd-acum-ped THEN DO:
                   ASSIGN c-obs  = "Acumulado do Cliente menor que Acumulado CCS, pedido n∆o ser† modificado !!!".
               END.

               // cria linha para importaá∆o se n∆o existir 
               FIND FIRST tt-edi-ccs 
                   WHERE tt-edi-ccs.dt-processo  = tt-edi.dt-movto          
                     AND tt-edi-ccs.cod-emitente = edi-ped-ent.cod-emitente   
                     AND tt-edi-ccs.nr-pedcli    = edi-ped-ent.nr-pedcli                  
                     AND tt-edi-ccs.codigo-refer = edi-ped-ent.codigo-refer   
                     AND tt-edi-ccs.dt-entrega   = dt-entrega                 
                     AND tt-edi-ccs.cod-estabel  = edi-ped-vendas.cod-estabel NO-ERROR.
               IF NOT AVAIL tt-edi-ccs THEN DO:
                   CREATE tt-edi-ccs.
                   ASSIGN tt-edi-ccs.dt-processo  = tt-edi.dt-movto
                          tt-edi-ccs.cod-emitente = emitente.cod-emitente
                          tt-edi-ccs.nr-pedcli    = edi-ped-ent.nr-pedcli
                          tt-edi-ccs.it-codigo    = edi-ped-ent.it-codigo
                          tt-edi-ccs.codigo-refer = edi-ped-ent.codigo-refer
                          tt-edi-ccs.qt-pedida    = edi-ped-ent.qtd-pedida
                          tt-edi-ccs.dt-entrega   = dt-entrega
                          tt-edi-ccs.nr-sequencia = i-nr-sequencia
                          tt-edi-ccs.nr-entrega   = i-nr-entrega
                          tt-edi-ccs.ult-nf       = edi-ped-vendas.ult-nf
                          tt-edi-ccs.i-estado     = edi-ped-ent.tipo-entr
                          tt-edi-ccs.l-processado = FALSE
                          tt-edi-ccs.cod-estabel  = edi-ped-vendas.cod-estabel.
               END.
               IF NOT tt-edi-ccs.l-processado THEN
                   ASSIGN tt-edi-ccs.c-tipo = c-tipo
                          tt-edi-ccs.c-obs  = c-obs
                          .
           END.
       END. // FOR EACH edi-ped-ent NO-LOCK 


       /**
        * 2012.06.{15,20,28}
        * Limpa entregas - cancela entregas que n∆o vieram no arquivo;
        * OBS: para Carterpillar apenas !!!
        ****************************************************************************************/
       RUN pi-processa-cancelados.
       
   END. /* FOR EACH edi-ped-vendas ... */

   OUTPUT STREAM st-ed CLOSE.

END PROCEDURE.

PROCEDURE pi-processa-cancelados:
    // caterpillar apenas

    IF edi-ped-vendas.cod-emitente <> 10000124 THEN 
        LEAVE.

    // n∆o cancelaremos remanufaturas. 
    IF edi-ped-vendas.nr-pedcli BEGINS "QRPP" THEN 
        LEAVE. 
    // 2013-04-01 ignorar amostras 
    IF edi-ped-vendas.nr-pedcli BEGINS "#" THEN 
        LEAVE. 
    IF edi-ped-vendas.nr-pedcli MATCHES "* C" THEN 
        LEAVE.
    //

    IF NOT AVAIL emitente THEN /* just in case */
       RUN pi-procura-emitente (INPUT edi-ped-vendas.cod-emitente, 
                                INPUT edi-ped-vendas.nr-pedcli).

    FOR EACH ped-venda NO-LOCK 
            WHERE ped-venda.nome-abrev = emitente.nome-abrev
              AND ped-venda.nr-pedcli  = edi-ped-vendas.nr-pedcli ,
        EACH ped-ent OF ped-venda NO-LOCK 
            WHERE ped-ent.cod-sit-ent < 3
              AND ped-ent.qt-pedida > ped-ent.qt-atendida
              AND ped-ent.dt-entrega > TODAY + 13, // hoje + periodo firme 
        FIRST ITEM OF ped-ent NO-LOCK :

        RUN pi-acompanhar IN h-acomp ("Entrega fora do EDI: " + ped-venda.nr-pedcli + 
                                      " " + STRING(ped-ent.dt-entrega)).

        FIND FIRST edi-ped-ent NO-LOCK 
            WHERE edi-ped-ent.cod-emitente = emitente.cod-emitente
              AND edi-ped-ent.dt-arquivo   = edi-ped-vendas.dt-arquivo
              AND edi-ped-ent.nr-pedcli    = edi-ped-vendas.nr-pedcli
              AND edi-ped-ent.codigo-refer = edi-ped-vendas.codigo-refer
              AND edi-ped-ent.dt-janela    = ped-ent.dt-entrega NO-ERROR.
        IF AVAIL edi-ped-ent THEN DO:
            NEXT.
        END.

        FIND FIRST tt-edi-ccs 
            WHERE tt-edi-ccs.dt-processo  = edi-ped-vendas.dt-arquivo
              AND tt-edi-ccs.cod-emitente = emitente.cod-emitente
              AND tt-edi-ccs.nr-pedcli    = ped-venda.nr-pedcli
              AND tt-edi-ccs.it-codigo    = ped-ent.it-codigo
              AND tt-edi-ccs.nr-sequencia = ped-ent.nr-sequencia
              AND tt-edi-ccs.nr-entrega   = ped-ent.nr-entrega 
              AND tt-edi-ccs.cod-estabel  = edi-ped-vendas.cod-estabel 
            NO-ERROR.
        IF AVAIL tt-edi-ccs THEN DO:
            NEXT.
        END.

        CREATE tt-edi-ccs.
        ASSIGN tt-edi-ccs.dt-processo  = edi-ped-vendas.dt-arquivo
               tt-edi-ccs.cod-emitente = emitente.cod-emitente
               tt-edi-ccs.nr-pedcli    = edi-ped-vendas.nr-pedcli 
               tt-edi-ccs.codigo-refer = edi-ped-vendas.codigo-refer
               tt-edi-ccs.it-codigo    = ped-ent.it-codigo
               tt-edi-ccs.qt-pedida    = ped-ent.qt-pedida
               tt-edi-ccs.dt-entrega   = ped-ent.dt-entrega
               tt-edi-ccs.nr-sequencia = ped-ent.nr-sequencia
               tt-edi-ccs.nr-entrega   = ped-ent.nr-entrega
               tt-edi-ccs.ult-nf       = ""
               tt-edi-ccs.l-processado = FALSE
               tt-edi-ccs.i-estado     = 0
               tt-edi-ccs.cod-estabel  = edi-ped-venda.cod-estabel
               .
        IF ped-ent.qt-atendida = 0 THEN 
            ASSIGN tt-edi-ccs.c-tipo = "C"
                   tt-edi-ccs.c-obs  = "Entrega n∆o estava no arquivo do EDI."
                   .
        ELSE
            ASSIGN tt-edi-ccs.c-tipo = "E"
                   tt-edi-ccs.c-obs  = "Entrega n∆o estava no arquivo do EDI, e j† foi faturada"
                   .

        PUT STREAM st-ed UNFORMATTED
            SKIP
            STRING(TODAY, "99/99/9999") ";"
            STRING(TIME, "HH:MM:SS") ";"
            edi-ped-vendas.dt-arquivo   ";"
            edi-ped-vendas.nr-pedcli    ";"
            edi-ped-vendas.cod-estabel  ";"
            edi-ped-vendas.codigo-refer ";"
            ped-ent.it-codigo    ";"
            ped-ent.dt-entrega   ";"
            ";"                        
            ped-ent.qt-pedida   ";"
            ";"
            tt-edi-ccs.c-tipo   ";"
            tt-edi-ccs.c-obs    ";"
            .

    END.  // FOR EACH ped-venda NO-LOCK ...

END PROCEDURE.


/*********************************************************************************************************/

FUNCTION fi-ignorar-pedidos RETURNS LOGICAL
  ( nr-pedcli AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  Pedidos que n∆o v∆o ser lidos no EDI
    Notes:  
------------------------------------------------------------------------------*/
    RETURN nr-pedcli = "QAPP41900 B" OR nr-pedcli = "QAPP41897 B".
END FUNCTION.


FUNCTION fi-corrige-nr-pedcli RETURNS CHAR
  ( nome-abrev AS CHAR, nr-pedcli AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  Acha o codigo de pedido "correto", caso tenha espaáos na frente
    Notes:  
------------------------------------------------------------------------------*/
    FIND FIRST ped-venda NO-LOCK 
        WHERE ped-venda.nome-abrev = nome-abrev
          AND ped-venda.nr-pedcli = nr-pedcli 
          AND ped-venda.cod-sit-ped <= 2 NO-ERROR.
    IF NOT AVAIL ped-venda THEN DO:
        FIND FIRST ped-venda NO-LOCK 
            WHERE ped-venda.nome-abrev = nome-abrev
              AND ped-venda.nr-pedcli MATCHES "*" + nr-pedcli 
              AND ped-venda.cod-sit-ped <= 2 NO-ERROR.
        IF AVAIL ped-venda THEN 
            ASSIGN nr-pedcli = ped-venda.nr-pedcli.
    END.
    RETURN nr-pedcli.
END FUNCTION.


FUNCTION fi-valida-item RETURNS CHAR (codigo-refer AS CHAR) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

    /* cat caáambas*/
    IF codigo-refer BEGINS "145-5164" OR codigo-refer BEGINS "145-5166" OR
       codigo-refer BEGINS "167-9223" OR codigo-refer BEGINS "167-9227" OR
       codigo-refer BEGINS "169-4872" OR codigo-refer BEGINS "174-9239" OR
       codigo-refer BEGINS "219-3359" OR codigo-refer BEGINS "219-3362" OR
       codigo-refer BEGINS "219-3370" OR codigo-refer BEGINS "219-3373" OR
       codigo-refer BEGINS "241-0937" OR codigo-refer BEGINS "251-1157" OR
       codigo-refer BEGINS "251-5414" OR codigo-refer BEGINS "252-6905" OR
       codigo-refer BEGINS "252-6911" THEN 
        RETURN  "Caáamba MAO. Item n∆o pode ser importado!":u.

    IF codigo-refer BEGINS "245-0105" OR codigo-refer BEGINS "245-0106" OR
       codigo-refer BEGINS "347-3845" OR codigo-refer BEGINS "347-3846" OR
       codigo-refer BEGINS "347-3849" OR codigo-refer BEGINS "348-8860" OR
       codigo-refer BEGINS "348-8873" OR codigo-refer BEGINS "348-8874" OR
       codigo-refer BEGINS "348-9200" OR codigo-refer BEGINS "348-9201" OR
       codigo-refer BEGINS "349-4394" OR codigo-refer BEGINS "349-9746" OR
       codigo-refer BEGINS "349-9748" OR codigo-refer BEGINS "350-9241" OR
       codigo-refer BEGINS "350-9247" OR codigo-refer BEGINS "350-9248" OR
       codigo-refer BEGINS "3T-3138" THEN 
        RETURN  "VERIFICAR, ITEM PROJETO ALUMAR!":u.

    IF codigo-refer BEGINS "219-3370" THEN 
        RETURN "N∆o foi importado por definiá∆o do Depto. de Vendas - Verificar preáos e componentes.":u.

    RETURN '':u.
END FUNCTION.

FUNCTION fi-valida-pedido RETURNS CHAR (nr-pedcli AS CHAR):
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    
    IF nr-pedcli BEGINS "QRPP" OR nr-pedcli MATCHES "* C" THEN 
        RETURN " Item N«O vai ser importado por definiá∆o do depto de Vendas, VERIFICAR!":u.

    IF nr-pedcli BEGINS "Q" AND nr-pedcli MATCHES "* B" THEN 
        RETURN " Pedido B, n∆o pode ser inclu°do pelo EDI.":u.

    RETURN '':u.
END FUNCTION.


FUNCTION fi-verifica-preco-item RETURNS CHAR (cod-estabel AS CHAR, nr-pedcli AS CHAR, it-codigo AS CHAR, qtd-pedida AS DECIMAL):
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

    IF nr-pedcli BEGINS "QEST" OR nr-pedcli BEGINS "HETZ" THEN  
        FIND FIRST preco-item NO-LOCK 
            WHERE preco-item.nr-tabpre = "10005825"
             AND preco-item.it-codigo = it-codigo
             AND preco-item.situacao  = 1 NO-ERROR.
    ELSE IF nr-pedcli BEGINS "QAPD" OR nr-pedcli BEGINS "QHPD" THEN 
        FIND FIRST preco-item NO-LOCK 
            WHERE preco-item.nr-tabpre = (IF cod-estabel = '100' THEN '10006602' 
                                                                 ELSE '20006602')
              AND preco-item.it-codigo = it-codigo
              AND preco-item.situacao  = 1 NO-ERROR.
    ELSE
        FIND FIRST preco-item NO-LOCK 
            WHERE preco-item.it-codigo = it-codigo
              AND preco-item.situacao  = 1 NO-ERROR.

    IF NOT AVAIL preco-item THEN 
        RETURN " Item n∆o est† em uma lista de preáo, ou preáo est† inativo !!!":u.

    IF preco-item.quant-min <> 0 AND preco-item.quant-min > qtd-pedida THEN 
        RETURN "Quantidade pedida Ç menor que a Quantidade Minima !!!":u.

    RETURN '':u.
END FUNCTION.
