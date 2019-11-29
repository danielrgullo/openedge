/**
 * Facilita o uso de BO para manutenir os pedidos
 *********************************************************************/
DEFINE VARIABLE hbodi159     AS HANDLE NO-UNDO.
DEFINE VARIABLE hbodi159sdf  AS HANDLE NO-UNDO.
DEFINE VARIABLE hbodi159com  AS HANDLE NO-UNDO.

DEFINE VARIABLE hbodi149     AS HANDLE NO-UNDO.
DEFINE VARIABLE hbodi149can  AS HANDLE NO-UNDO.

DEFINE VARIABLE hbodi154     AS HANDLE NO-UNDO.
DEFINE VARIABLE hbodi154sdf  AS HANDLE NO-UNDO.
DEFINE VARIABLE hbodi154cal  AS HANDLE NO-UNDO.


DEF STREAM st-erro.

FUNCTION get-hr-entrega 
    RETURNS CHAR 
        (nr-pedcli AS CHAR, nome-abrev AS CHAR, it-codigo AS CHAR, 
         dt-entrega AS DATE,  c-hora AS CHAR) FORWARD.

/* Temp-Tables *********************************************************/
DEFINE TEMP-TABLE tt-ped-venda NO-UNDO LIKE ped-venda
    FIELD r-rowid AS ROWID.

DEFINE TEMP-TABLE tt-ped-item  NO-UNDO LIKE ped-item
    FIELD r-rowid AS ROWID.

DEFINE TEMP-TABLE tt-ped-ent NO-UNDO LIKE ped-ent
    FIELD r-rowid AS ROWID .
  
//

DEFINE TEMP-TABLE RowErros NO-UNDO 
    FIELD errorSequence    AS INTEGER
    FIELD errorNumber      AS INTEGER
    FIELD errorDescription AS CHARACTER FORMAT 'x(60)'
    FIELD errorParameters  AS CHARACTER FORMAT 'x(60)'
    FIELD errorType        AS CHARACTER FORMAT 'x(60)'
    FIELD errorHelp        AS CHARACTER FORMAT 'x(60)'
    FIELD errorsubtype     AS CHARACTER FORMAT 'x(60)'.

/* Procedures ***********************************************************/

PROCEDURE pi-adicionaPedido:
    DEF INPUT PARAM c-cod-estabel  AS CHAR NO-UNDO.
    DEF INPUT PARAM c-nome-abrev   AS CHAR NO-UNDO.
    DEF INPUT PARAM c-nr-pedcli    AS CHAR NO-UNDO.
    DEF INPUT PARAM c-nr-tabpre    AS CHAR NO-UNDO. // emitente.nr-tabpre
    DEF INPUT PARAM i-esp-ped      AS  INT NO-UNDO. // 1: pedido simples; 2: programacao entrega 
    DEF INPUT PARAM c-nat-operacao AS CHAR NO-UNDO.
    DEF OUTPUT PARAM TABLE FOR RowErros .

    DEF VAR c-representante AS CHAR NO-UNDO INIT "CCS Tecnolog".

    FIND FIRST emitente NO-LOCK 
        WHERE emitente.nome-abrev = c-nome-abrev NO-ERROR.
    IF NOT AVAIL emitente THEN DO:
        RUN pi-cria-erro (INPUT 'Cliente n∆o encontrado.',
                          INPUT 'Cliente n∆o encontrado. Nome inv†lido : ' + c-nome-abrev).
        RETURN 'NOK':u.
    END.

    FIND FIRST natur-oper NO-LOCK
        WHERE natur-oper.nat-oper = c-nat-operacao NO-ERROR.
    IF NOT AVAIL natur-oper THEN DO:
        RUN pi-cria-erro (INPUT 'Natureza de operaá∆o n∆o encontrada.',
                          INPUT 'Natureza de operaá∆o n∆o encontrada: ' + c-nat-operacao).
        RETURN 'NOK':u.
    END.


    EMPTY TEMP-TABLE tt-ped-venda.

    RUN inicializa-api.
    IF RETURN-VALUE = 'NOK' THEN DO:
        RUN pi-cria-erro (INPUT 'Erro Inesperado. N∆o carregou BO, informar o TI.',
                          INPUT '').
        RETURN 'NOK':u.
    END.

    // ->

    RUN newRecord IN hbodi159.
    RUN getRecord IN hbodi159 (OUTPUT TABLE tt-ped-venda).
    
    FIND FIRST tt-ped-venda NO-ERROR.
    ASSIGN tt-ped-venda.nome-abrev = emitente.nome-abrev.

    RUN INPUTTable IN hbodi159sdf (INPUT TABLE tt-ped-venda).
    RUN setDefaultCustomerName IN hbodi159sdf.
    RUN setDefaultCentralSales IN hbodi159sdf.
    RUN OUTPUTTable IN hbodi159sdf (OUTPUT TABLE tt-ped-venda).
    
    IF c-nr-pedcli BEGINS "QYPP" OR c-nr-pedcli BEGINS "QYPD" THEN
        ASSIGN i-esp-ped = 1.
    IF c-nr-pedcli BEGINS "#QYPP" OR c-nr-pedcli BEGINS "#QYPD" THEN
        ASSIGN i-esp-ped = 1.
    
    FIND FIRST tt-ped-venda NO-ERROR.
    ASSIGN  tt-ped-venda.nr-pedcli       = c-nr-pedcli 
            tt-ped-venda.cod-estabel     = c-cod-estabel
            tt-ped-venda.nr-pedrep       = ""
            tt-ped-venda.contato         = ""
            tt-ped-venda.cod-gr-cli      = emitente.cod-gr-cli
            tt-ped-venda.no-ab-reppri    = c-representante
            tt-ped-venda.cod-mensagem    = natur-oper.cod-mensagem
            tt-ped-venda.esp-ped         = i-esp-ped /* 1 = pedido simples,  2 = programacao entrega */
            tt-ped-venda.origem          = 1 /* padr∆o: 1 */
            tt-ped-venda.cd-origem       = 2 /* 2 = EDI */
            tt-ped-venda.tp-preco        = 3 /* 3 = dia do fat. */
            tt-ped-venda.nr-tab-finan    = 1 /* padr∆o: 0, mas Ç obrigat¢rio */
            tt-ped-venda.nr-ind-finan    = 1 /* padr∆o: 0, mas Ç obrigat¢rio */
            tt-ped-venda.tab-ind-fin     = 1 /* padr∆o: mesmo que nr-tab-finan */ 
            tt-ped-venda.mo-codigo       = 0 /* 0 = moeda corrente */
            tt-ped-venda.cod-sit-aval    = 1 /* padr∆o: 1 */
            tt-ped-venda.cod-sit-pre     = 1 /* padr∆o: 1 */
            tt-ped-venda.cod-sit-ped     = 1 /* padr∆o: 1 */
            tt-ped-venda.cod-sit-com     = 1 /* padr∆o: 1 */
            tt-ped-venda.proc-edi        = 1 /* padr∆o: ? */
            tt-ped-venda.ind-aprov       = YES
            tt-ped-venda.tp-receita      = emitente.tp-rec-padrao
            tt-ped-venda.tp-pedido       = ""
            tt-ped-venda.nat-operacao    = c-nat-operacao
            tt-ped-venda.nr-pedido       = NEXT-VALUE(seq-nr-pedido)
            tt-ped-venda.cod-canal-venda = 0
            tt-ped-venda.cod-cond-pag    = emitente.cod-cond-pag
            tt-ped-venda.cod-portador    = 341 // fixo?
            tt-ped-venda.modalidade      = 6   // ?
            tt-ped-venda.cod-entrega     = "Padr∆o"
            tt-ped-venda.local-entreg    = emitente.endereco
            tt-ped-venda.bairro          = emitente.bairro
            tt-ped-venda.cidade          = emitente.cidade
            tt-ped-venda.estado          = emitente.estado
            tt-ped-venda.cep             = emitente.cep
            tt-ped-venda.caixa-postal    = ""
            tt-ped-venda.pais            = emitente.pais
            tt-ped-venda.cgc             = emitente.cgc
            tt-ped-venda.nome-transp     = "" // (n∆o obrigat¢rio) 
            tt-ped-venda.dt-emissao      = TODAY
            tt-ped-venda.dt-implant      = TODAY
            tt-ped-venda.observacoes     = ''
            .

    IF c-nr-tabpre <> "" THEN
        ASSIGN tt-ped-venda.nr-tabpre = c-nr-tabpre.
    
    /* transportador da atualizaá∆o clientes do estabelecimento - PD0507 */
    FOR FIRST estab-cli NO-LOCK 
        WHERE estab-cli.cod-estabel = c-cod-estabel
          AND estab-cli.nome-abrev = c-nome-abrev :
        ASSIGN tt-ped-venda.nome-transp = estab-cli.nome-transp.
    END.

    
    // DO TRANSACTION: // j† Ç???

        RUN setRecord IN hbodi159 (INPUT TABLE tt-ped-venda).
        RUN createRecord IN hbodi159.
        RUN getRowErrors IN hbodi159 (OUTPUT TABLE RowErros).

        FIND FIRST RowErros NO-LOCK
            WHERE RowErros.ErrorSubType <> 'WARNING' NO-ERROR.
        IF AVAIL RowErros THEN DO:        
            RUN pi-grava-erro-txt ('bodi159').
            UNDO, RETURN 'NOK':u.
        END.
    
    // END.

    RUN getRecord IN hbodi159 (OUTPUT TABLE tt-ped-venda).

    DEF VAR i-nr-pedido AS INTEGER NO-UNDO.
    FIND FIRST tt-ped-venda NO-LOCK NO-ERROR.
    IF AVAIL tt-ped-venda THEN
        ASSIGN i-nr-pedido = tt-ped-venda.nr-pedido.

    DEF VAR rw-pedido AS ROWID NO-UNDO.
    RUN getRowid IN hbodi159 (OUTPUT rw-pedido).

    FIND FIRST repres NO-LOCK 
        WHERE repres.nome-abrev = c-representante NO-ERROR.
    IF AVAIL repres THEN DO:
        CREATE ped-repre.
        ASSIGN ped-repre.nr-pedido   = i-nr-pedido
               ped-repre.nome-ab-rep = repres.nome-abrev
               ped-repre.ind-repbase = YES 
               ped-repre.perc-comis  = 0
               ped-repre.comis-emis  = 0
               .
    END.

END PROCEDURE.

PROCEDURE adicionaPedido:
    DEF INPUT PARAM c-cod-estabel AS CHAR NO-UNDO.
    DEF INPUT PARAM c-nome-abrev  AS CHAR NO-UNDO.
    DEF INPUT PARAM c-nr-pedcli   AS CHAR NO-UNDO.
    DEF INPUT PARAM c-nr-tabpre   AS CHAR NO-UNDO. // emitente.nr-tabpre
    DEF INPUT PARAM i-esp-ped     AS  INT NO-UNDO. // 1: pedido simples; 2: programacao entrega 
    DEF OUTPUT PARAM TABLE FOR RowErros.

    DEF VAR c-nat-operacao  AS CHAR NO-UNDO INIT "5101".

    EMPTY TEMP-TABLE RowErros.

    FIND FIRST emitente NO-LOCK 
        WHERE emitente.nome-abrev = c-nome-abrev NO-ERROR.
    IF NOT AVAIL emitente THEN DO:
        RUN pi-cria-erro (INPUT 'Cliente n∆o encontrado.',
                          INPUT 'Cliente n∆o encontrado. Nome inv†lido : ' + c-nome-abrev).
        RETURN 'NOK':u.
    END.
    
    IF emitente.nat-oper <> '' AND emitente.nat-oper <> ?  THEN
        ASSIGN c-nat-operacao = emitente.nat-oper.
    RUN pega-natureza-operacao (INPUT c-cod-estabel,
                                INPUT emitente.cod-emitente,
                                INPUT c-nr-pedcli, 
                                INPUT-OUTPUT c-nat-operacao).
    FIND FIRST natur-oper NO-LOCK
        WHERE natur-oper.nat-oper = c-nat-operacao NO-ERROR.
    IF NOT AVAIL natur-oper THEN DO:
        RUN pi-cria-erro (INPUT 'Natureza de operaá∆o n∆o encontrada.',
                          INPUT 'Natureza de operaá∆o n∆o encontrada: ' + c-nat-operacao).
        RETURN 'NOK':u.
    END.

    RUN pi-adicionaPedido (INPUT c-cod-estabel,
                           INPUT c-nome-abrev,
                           INPUT c-nr-pedcli,
                           INPUT c-nr-tabpre,
                           INPUT i-esp-ped,
                           INPUT c-nat-operacao,
                           OUTPUT TABLE RowErros).
    RETURN RETURN-VALUE.

    // RUN destroyAll.
END PROCEDURE.


PROCEDURE adicionaPedidoItem:
    DEF INPUT PARAM cod-estabel  AS CHAR NO-UNDO.
    DEF INPUT PARAM c-nome-abrev AS CHAR NO-UNDO.
    DEF INPUT PARAM c-nr-pedcli  AS CHAR NO-UNDO.
    DEF INPUT PARAM c-it-codigo  AS CHAR NO-UNDO.
    DEF INPUT PARAM dt-vencto    AS DATE NO-UNDO.
    DEF INPUT PARAM d-qtd     AS DECIMAL NO-UNDO.
    DEF INPUT PARAM d-preco   AS DECIMAL NO-UNDO.
    DEF INPUT PARAM c-obs        AS CHAR NO-UNDO.
    DEF OUTPUT PARAM TABLE FOR RowErros .

    DEF VAR c-nat-operacao AS CHAR NO-UNDO INIT "5101".    
    DEF VAR i-sequencia AS INTEGER NO-UNDO.

    EMPTY TEMP-TABLE RowErros.

    IF c-obs = ? THEN
        ASSIGN c-obs = ''.

    FIND FIRST ITEM NO-LOCK 
        WHERE ITEM.it-codigo = c-it-codigo NO-ERROR.
    IF NOT AVAIL ITEM THEN DO:
        RUN pi-cria-erro (INPUT 'Item n∆o encontrado.',
                          INPUT 'Item n∆o encontrado. C¢digo inv†lido : ' + c-it-codigo).
        RETURN 'NOK':u.
    END.

    FIND FIRST emitente NO-LOCK 
        WHERE emitente.nome-abrev = c-nome-abrev NO-ERROR.
    IF NOT AVAIL emitente THEN DO:
        RUN pi-cria-erro (INPUT 'Cliente n∆o encontrado.',
                          INPUT 'Cliente n∆o encontrado. Nome inv†lido : ' + c-nome-abrev).
        RETURN 'NOK':u.
    END.

    RUN inicializa-api.
    IF RETURN-VALUE = 'NOK'  THEN DO:
        RUN pi-cria-erro (INPUT 'Erro Inesperado. N∆o carregou BO, informar o TI.',
                          INPUT '').
        RETURN 'NOK':u.
    END.

    ASSIGN i-sequencia = 0.
    FOR EACH ped-item NO-LOCK 
            WHERE ped-item.nome-abrev = emitente.nome-abrev
              AND ped-item.nr-pedcli  = c-nr-pedcli:
        IF i-sequencia < ped-item.nr-sequencia THEN
            ASSIGN i-sequencia = ped-item.nr-sequencia.
    END.
    ASSIGN i-sequencia = i-sequencia + 10.
    
    // ->

    EMPTY TEMP-TABLE tt-ped-item.

    RUN newRecord IN hbodi154.
    RUN getRecord IN hbodi154 (OUTPUT TABLE tt-ped-item).

    FIND FIRST tt-ped-item NO-ERROR.
    ASSIGN tt-ped-item.nome-abrev   = emitente.nome-abrev
           tt-ped-item.nr-pedcli    = c-nr-pedcli
           tt-ped-item.it-codigo    = c-it-codigo
           tt-ped-item.nr-sequencia = i-sequencia.

    RUN INPUTTable IN hbodi154sdf (INPUT TABLE tt-ped-item).
    RUN setDefaultItem             IN hbodi154sdf.
    RUN setDefaultPriceTable       IN hbodi154sdf.
    RUN setDefaultTransactionType  IN hbodi154sdf.
    RUN OUTPUTTable IN hbodi154sdf(OUTPUT TABLE tt-ped-item).

    FIND FIRST tt-ped-item NO-ERROR.
    
    /* retorna erro se cabeáalho do pedido (ped-venda) n∆o existe. */
    EMPTY TEMP-TABLE tt-ped-venda.
    FIND ped-venda OF tt-ped-item NO-ERROR.
    IF NOT AVAIL ped-venda THEN DO:
        RUN destroyAll.    
        RETURN 'NOK':u.
    END.
    BUFFER-COPY ped-venda TO tt-ped-venda.
    ASSIGN tt-ped-venda.r-rowid = ROWID(tt-ped-venda).
    
    RUN pega-natureza-operacao-item 
                    (INPUT cod-estabel,                 
                     INPUT emitente.cod-emitente,
                     INPUT c-nr-pedcli,
                     INPUT c-it-codigo,
                     INPUT-OUTPUT c-nat-operacao ).

    ASSIGN tt-ped-item.esp-ped        = tt-ped-venda.esp-ped
           tt-ped-item.cd-origem      = 2 /* 2 = EDI */ 
           tt-ped-item.nr-sequencia   = i-sequencia // adiciona novamente? pq? 
           tt-ped-item.nr-ordem       = 0
           tt-ped-item.dt-entorig     = dt-vencto
           tt-ped-item.dt-entrega     = dt-vencto
           tt-ped-item.qt-pedida      = d-qtd
           tt-ped-item.qt-un-fat      = d-qtd
           tt-ped-item.qt-atendida    = 0
           tt-ped-item.vl-pretab      = d-preco
           tt-ped-item.vl-preori      = d-preco
           tt-ped-item.vl-preuni      = d-preco
           tt-ped-item.vl-liq-it      = d-preco
           tt-ped-item.vl-liq-abe     = d-preco 
           tt-ped-item.vl-desconto    = 0
           tt-ped-item.val-desconto-inform = 0           
           tt-ped-item.tp-adm-lote    = 1 // padr∆o: 1 
           tt-ped-item.tp-aloc-lote   = 1 // padr∆o: 1 
           tt-ped-item.tipo-atend     = 2 // parcial: 2
           tt-ped-item.cod-sit-item   = 1 // padr∆o: 1 
           tt-ped-item.cod-sit-pre    = tt-ped-venda.cod-sit-pre
           tt-ped-item.cod-sit-com    = tt-ped-venda.cod-sit-com
           tt-ped-item.cod-entrega    = tt-ped-venda.cod-entrega
           tt-ped-item.observacao     = c-obs
           .

    FOR FIRST preco-item FIELDS (nr-tabpre) NO-LOCK 
            WHERE preco-item.it-codigo = c-it-codigo
              AND preco-item.situacao = 1,
        FIRST tb-preco FIELDS (nr-tabpre) OF preco-item NO-LOCK 
            WHERE tb-preco.situacao = 1:
        ASSIGN tt-ped-item.nr-tabpre = tb-preco.nr-tabpre.
    END.

    RUN setRecord IN hbodi154 (INPUT TABLE tt-ped-item).

    DO TRANSACTION:

        RUN createRecord IN hbodi154.
        RUN getRowErrors IN hbodi154 (OUTPUT TABLE RowErros).

        FIND FIRST RowErros NO-LOCK
            WHERE RowErros.ErrorSubType <> 'WARNING' NO-ERROR.
        IF AVAIL RowErros THEN DO:
            RUN pi-grava-erro-txt ('bodi154').
            UNDO, RETURN 'NOK':u.
        END.
    END.

    RETURN 'OK':u.
END PROCEDURE. /* adicionaPedidoItem */


PROCEDURE alteraPedidoItem:
    DEF INPUT  PARAM c-nome-abrev AS CHAR NO-UNDO.
    DEF INPUT  PARAM c-nr-pedcli  AS CHAR NO-UNDO.
    DEF INPUT  PARAM i-sequencia  AS  INT NO-UNDO.
    DEF INPUT  PARAM c-it-codigo  AS CHAR NO-UNDO.
    DEF INPUT  PARAM dt-vencto    AS DATE NO-UNDO.
    DEF INPUT  PARAM d-qtd     AS DECIMAL NO-UNDO.
    DEF OUTPUT PARAM TABLE FOR RowErros.

    DEF VAR c-hora AS CHAR NO-UNDO.

    EMPTY TEMP-TABLE RowErros.

    FIND FIRST ITEM NO-LOCK 
        WHERE ITEM.it-codigo = c-it-codigo NO-ERROR.
    IF NOT AVAIL ITEM THEN DO:
        RUN pi-cria-erro (INPUT 'Item n∆o encontrado.',
                          INPUT 'Item n∆o encontrado. C¢digo inv†lido : ' + c-it-codigo).
        RETURN 'NOK':u.
    END.
    FIND FIRST emitente NO-LOCK 
        WHERE emitente.nome-abrev = c-nome-abrev NO-ERROR.
    IF NOT AVAIL emitente THEN DO:
        RUN pi-cria-erro (INPUT 'Cliente n∆o encontrado.',
                          INPUT 'Cliente n∆o encontrado. Nome inv†lido : ' + c-nome-abrev).
        RETURN 'NOK':u.
    END.


    RUN inicializa-api.
    IF RETURN-VALUE = 'NOK' THEN DO:
        RUN pi-cria-erro (INPUT 'Erro Inesperado. N∆o carregou BO, informar o TI.',
                          INPUT '').
        RETURN 'NOK':u.
    END.

    // ->

    EMPTY TEMP-TABLE tt-ped-item.

    RUN goToKey   IN hbodi159 (INPUT c-nome-abrev, 
                               INPUT c-nr-pedcli).
    RUN goToKey   IN hbodi154 (INPUT c-nome-abrev, 
                               INPUT c-nr-pedcli, 
                               INPUT i-sequencia, 
                               INPUT c-it-codigo, 
                               INPUT "").
    
    RUN getRecord IN hbodi154 (OUTPUT TABLE tt-ped-item).
    
    FIND FIRST tt-ped-item NO-ERROR.
    ASSIGN tt-ped-item.qt-pedida    = d-qtd
           tt-ped-item.qt-un-fat    = d-qtd               
           .
    IF tt-ped-item.dt-entrega <> dt-vencto THEN DO:
        ASSIGN tt-ped-item.dt-entrega = dt-vencto.
    END.

    RUN SetRecord IN hbodi154 (INPUT TABLE tt-ped-item).


    DO TRANSACTION:

        RUN updateRecord IN hbodi154.
        RUN getRowErrors IN hbodi154 (OUTPUT TABLE RowErros).

        FIND FIRST RowErros NO-LOCK
            WHERE RowErros.ErrorSubType <> 'WARNING' NO-ERROR.
        IF AVAIL RowErros THEN DO:
            RUN pi-grava-erro-txt ('bodi154').
            UNDO, RETURN 'NOK':u.
        END.
    END.
    //RUN destroyAll.
    RETURN 'OK':u.
END PROCEDURE. /* alteraPedidoItem */


PROCEDURE adicionaPedidoItemEntrega:
    DEF INPUT  PARAM c-nome-abrev AS CHAR NO-UNDO.
    DEF INPUT  PARAM c-nr-pedcli  AS CHAR NO-UNDO.
    DEF INPUT  PARAM i-sequencia  AS  INT NO-UNDO.
    DEF INPUT  PARAM c-it-codigo  AS CHAR NO-UNDO.
    DEF INPUT  PARAM dt-vencto    AS DATE NO-UNDO.
    DEF INPUT  PARAM d-qtd     AS DECIMAL NO-UNDO.
    DEF INPUT  PARAM d-preco   AS DECIMAL NO-UNDO.
    DEF INPUT  PARAM c-obs        AS CHAR NO-UNDO.
    DEF OUTPUT PARAM TABLE FOR RowErros.

    DEF VAR i-seq-entrega AS INTEGER NO-UNDO.
    DEF VAR c-hora AS CHAR INIT "000000".

    EMPTY TEMP-TABLE RowErros.

    IF c-obs = ? THEN
        ASSIGN c-obs = ''.

    FIND FIRST ITEM NO-LOCK 
        WHERE ITEM.it-codigo = c-it-codigo NO-ERROR.
    IF NOT AVAIL ITEM THEN DO:
        RUN pi-cria-erro (INPUT 'Item n∆o encontrado.',
                          INPUT 'Item n∆o encontrado. C¢digo inv†lido : ' + c-it-codigo).
        RETURN 'NOK':u.
    END.

    FIND FIRST emitente NO-LOCK
        WHERE emitente.nome-abrev = c-nome-abrev NO-ERROR.
    IF NOT AVAIL emitente THEN DO:
        RUN pi-cria-erro (INPUT 'Cliente n∆o encontrado.',
                          INPUT 'Cliente n∆o encontrado. Nome inv†lido : ' + c-nome-abrev).
        RETURN 'NOK':u.
    END.


    RUN inicializa-api.
    IF RETURN-VALUE = 'NOK'  THEN DO:
        RUN pi-cria-erro (INPUT 'Erro Inesperado. N∆o carregou BO, informar o TI.',
                          INPUT '').
        RETURN 'NOK':u.
    END.

    // ->

    ASSIGN i-seq-entrega = 0.
    FOR EACH ped-ent NO-LOCK 
            WHERE ped-ent.nome-abrev   = c-nome-abrev
              AND ped-ent.nr-pedcli    = c-nr-pedcli 
              AND ped-ent.nr-sequencia = i-sequencia :
        IF i-seq-entrega < ped-ent.nr-entrega THEN
            ASSIGN i-seq-entrega = ped-ent.nr-entrega.
    END.
    ASSIGN i-seq-entrega = i-seq-entrega + 10.    

    ASSIGN c-hora = get-hr-entrega(c-nr-pedcli, 
                                   c-nome-abrev,
                                   c-it-codigo, 
                                   dt-vencto, 
                                   '').

    EMPTY TEMP-TABLE tt-ped-ent.

    RUN newRecord IN hbodi149.
    RUN getRecord IN hbodi149 (OUTPUT TABLE tt-ped-ent).

    FIND FIRST tt-ped-ent NO-ERROR.
    ASSIGN tt-ped-ent.nr-pedcli    = c-nr-pedcli
           tt-ped-ent.nome-abrev   = c-nome-abrev
           tt-ped-ent.nr-sequencia = i-sequencia
           tt-ped-ent.nr-entrega   = i-seq-entrega
           tt-ped-ent.nr-ent-prog  = i-seq-entrega
           tt-ped-ent.it-codigo    = c-it-codigo
           tt-ped-ent.qt-pedida    = d-qtd
           tt-ped-ent.qt-un-fat    = d-qtd
           tt-ped-ent.tipo-atend   = 2 /* 1 = total, 2 = parcial */
           tt-ped-ent.cd-origem    = 2 /* 2 - EDI */
           tt-ped-ent.hr-entrega   = c-hora
           tt-ped-ent.tp-entrega   = 1 /* 1 = firme */
           tt-ped-ent.dt-entrega   = dt-vencto
           tt-ped-ent.observacao   = c-obs
           .

    FIND FIRST ped-item NO-LOCK
        WHERE ped-item.nome-abrev   = c-nome-abrev
          AND ped-item.nr-pedcli    = c-nr-pedcli 
          AND ped-item.nr-sequencia = i-sequencia NO-ERROR.
    ASSIGN tt-ped-ent.r-rowid = ROWID(ped-item).
    
    RUN setRecord IN hbodi149 (INPUT TABLE tt-ped-ent).

    DO TRANSACTION:
        
        RUN createRecord IN hbodi149.
        RUN getRowErrors IN hbodi149 (OUTPUT TABLE RowErros).

         FIND FIRST RowErros NO-LOCK
            WHERE RowErros.ErrorSubType <> 'WARNING' NO-ERROR.
        IF AVAIL RowErros THEN DO:
            RUN pi-grava-erro-txt ('bodi149').
            UNDO, RETURN 'NOK':u.
        END.
    END.
    RETURN 'OK':u.
END PROCEDURE. /* adicionaPedidoItemEntrega */


PROCEDURE alteraPedidoItemEntrega:
    DEF INPUT  PARAM c-nome-abrev AS CHAR NO-UNDO.
    DEF INPUT  PARAM c-nr-pedcli  AS CHAR NO-UNDO.
    DEF INPUT  PARAM i-sequencia  AS  INT NO-UNDO.
    DEF INPUT  PARAM i-entrega    AS  INT NO-UNDO.
    DEF INPUT  PARAM c-it-codigo  AS CHAR NO-UNDO.
    DEF INPUT  PARAM dt-vencto    AS DATE NO-UNDO.
    DEF INPUT  PARAM d-qtd     AS DECIMAL NO-UNDO.
    DEF OUTPUT PARAM TABLE FOR RowErros.

    DEF VAR c-hora AS CHAR NO-UNDO.

    EMPTY TEMP-TABLE RowErros.

    FIND FIRST ITEM NO-LOCK 
        WHERE ITEM.it-codigo = c-it-codigo NO-ERROR.
    IF NOT AVAIL ITEM THEN DO:
        RUN pi-cria-erro (INPUT 'Item n∆o encontrado.',
                          INPUT 'Item n∆o encontrado. C¢digo inv†lido : ' + c-it-codigo).
        RETURN 'NOK':u.
    END.

    FIND FIRST emitente NO-LOCK 
        WHERE emitente.nome-abrev = c-nome-abrev NO-ERROR.
    IF NOT AVAIL emitente THEN DO:
        RUN pi-cria-erro (INPUT 'Cliente n∆o encontrado.',
                          INPUT 'Cliente n∆o encontrado. Nome inv†lido : ' + c-nome-abrev).
        RETURN 'NOK':u.
    END.

    RUN inicializa-api.
    IF RETURN-VALUE = 'NOK' THEN DO:
        RUN pi-cria-erro (INPUT 'Erro Inesperado. N∆o carregou BO, informar o TI.',
                          INPUT '').
        RETURN 'NOK':u.
    END.
    
    EMPTY TEMP-TABLE tt-ped-ent.

    RUN goToKey   IN hbodi159 (INPUT c-nome-abrev, 
                               INPUT c-nr-pedcli).
    RUN goToKey   IN hbodi154 (INPUT c-nome-abrev, 
                               INPUT c-nr-pedcli, 
                               INPUT i-sequencia, 
                               INPUT c-it-codigo, 
                               INPUT "").
    RUN goToKey   IN hbodi149 (INPUT c-nome-abrev, 
                               INPUT c-nr-pedcli, 
                               INPUT i-sequencia, 
                               INPUT c-it-codigo, 
                               INPUT "", 
                               INPUT i-entrega).
    RUN getRecord IN hbodi149 (OUTPUT TABLE tt-ped-ent).
    
    FIND FIRST tt-ped-ent NO-ERROR.
    ASSIGN tt-ped-ent.qt-pedida = d-qtd
           tt-ped-ent.qt-un-fat = d-qtd               
           .

    IF tt-ped-ent.dt-entrega <> dt-vencto THEN DO:
        ASSIGN c-hora = get-hr-entrega (c-nr-pedcli, 
                                        c-nome-abrev,
                                        c-it-codigo, 
                                        dt-vencto, 
                                        tt-ped-ent.hr-entrega).

        ASSIGN tt-ped-ent.dt-entrega = dt-vencto
               tt-ped-ent.hr-entrega = c-hora.
    END.

    RUN setRecord IN hbodi149 (INPUT TABLE tt-ped-ent).

    DO TRANSACTION:

        RUN updateRecord IN hbodi149.
        RUN getRowErrors IN hbodi149 (OUTPUT TABLE RowErros).

        FIND FIRST RowErros NO-LOCK
            WHERE RowErros.ErrorSubType <> 'WARNING' NO-ERROR.
        IF AVAIL RowErros THEN DO:
            RUN pi-grava-erro-txt ('bodi149').
            UNDO, RETURN 'NOK':u.
        END.
    END.
    RETURN 'OK':u.
END PROCEDURE. /* alteraPedidoItemEntrega */


PROCEDURE cancelaPedidoItemEntrega:
    DEF INPUT  PARAM c-nome-abrev AS CHAR NO-UNDO.
    DEF INPUT  PARAM c-nr-pedcli  AS CHAR NO-UNDO.
    DEF INPUT  PARAM i-sequencia  AS  INT NO-UNDO.
    DEF INPUT  PARAM i-entrega    AS  INT NO-UNDO.
    DEF INPUT  PARAM c-it-codigo  AS CHAR NO-UNDO.
    DEF INPUT  PARAM dt-vencto    AS DATE NO-UNDO.
    DEF INPUT  PARAM dt-cancela   AS DATE NO-UNDO.
    DEF OUTPUT PARAM TABLE FOR RowErros.

    DEF VAR i-motivo AS INTEGER NO-UNDO INIT 1. // 1 = cancelado, 2 = suspende, 3 = reativa

    EMPTY TEMP-TABLE RowErros.

    FIND FIRST ITEM NO-LOCK 
        WHERE ITEM.it-codigo = c-it-codigo NO-ERROR.
    IF NOT AVAIL ITEM THEN DO:
        RUN pi-cria-erro (INPUT 'Item n∆o encontrado.',
                          INPUT 'Item n∆o encontrado. C¢digo inv†lido : ' + c-it-codigo).
        RETURN 'NOK':u.
    END.

    FIND FIRST emitente NO-LOCK 
        WHERE emitente.nome-abrev = c-nome-abrev NO-ERROR.
    IF NOT AVAIL emitente THEN DO:
        RUN pi-cria-erro (INPUT 'Cliente n∆o encontrado.',
                          INPUT 'Cliente n∆o encontrado. Nome inv†lido : ' + c-nome-abrev).
        RETURN 'NOK':u.
    END.

    IF NOT VALID-HANDLE(hbodi149can) THEN
        RUN dibo/bodi149can.p PERSISTENT SET hbodi149can.
    
    FOR FIRST ped-ent 
        WHERE ped-ent.nr-pedcli    = c-nr-pedcli 
          AND ped-ent.nome-abrev   = c-nome-abrev
          AND ped-ent.it-codigo    = c-it-codigo 
          AND ped-ent.nr-sequencia = i-sequencia
          AND ped-ent.nr-entrega   = i-entrega: END.
    IF NOT AVAIL ped-ent THEN DO:
        RUN pi-cria-erro (INPUT 'Entrega n∆o encontrada.',
                          INPUT 'Entrega n∆o encontrada.~nPedido : ' + c-nr-pedcli + 
                                '~nCliente : ' + c-nome-abrev + 
                                '~nItem : ' + c-it-codigo + 
                                '~Entrega : ' + STRING(dt-vencto, '99/99/9999')).
        RETURN 'NOK':u.
    END.

    // precisa disso?
    FIND FIRST ped-item OF ped-ent NO-ERROR.
    FIND FIRST ped-venda OF ped-item NO-ERROR.

    DO TRANSACTION:
        
        RUN updateCancelation IN hbodi149can (INPUT ROWID(ped-ent), 
                                              INPUT "Cancelamento via EDI", 
                                              INPUT dt-cancela, 
                                              INPUT i-motivo).

        RUN GetRowErrors IN hbodi149can (OUTPUT TABLE RowErros).

        FIND FIRST RowErros NO-LOCK
            WHERE RowErros.ErrorSubType <> 'WARNING' NO-ERROR.
        IF AVAIL RowErros THEN DO:
            RUN pi-grava-erro-txt ('bodi149can').
            UNDO, RETURN 'NOK':u.
        END.
    END.
    RETURN 'OK':u.
END PROCEDURE. /* cancelaPedidoItemEntrega */


PROCEDURE deletaPedidoItemEntrega:
    DEF INPUT  PARAM c-nome-abrev AS CHAR NO-UNDO.
    DEF INPUT  PARAM c-nr-pedcli        AS CHAR NO-UNDO.
    DEF INPUT  PARAM i-sequencia  AS  INT NO-UNDO.
    DEF INPUT  PARAM i-entrega    AS  INT NO-UNDO.
    DEF INPUT  PARAM c-it-codigo         AS CHAR NO-UNDO.
    DEF OUTPUT PARAM TABLE FOR RowErros.

    EMPTY TEMP-TABLE RowErros.

    FIND FIRST emitente NO-LOCK 
        WHERE emitente.nome-abrev = c-nome-abrev NO-ERROR.
    IF NOT AVAIL emitente THEN DO:
        RUN pi-cria-erro (INPUT 'Cliente n∆o encontrado.',
                          INPUT 'Cliente n∆o encontrado. Nome inv†lido : ' + c-nome-abrev).
        RETURN 'NOK':u.
    END.

    RUN inicializa-api.
    IF RETURN-VALUE = 'NOK' THEN DO:
        RUN pi-cria-erro (INPUT 'Erro Inesperado. N∆o carregou BO, informar o TI.',
                          INPUT '').
        RETURN 'NOK':u.
    END.


    // TODO: validar return-value !
    RUN goToKey   IN hbodi159 (INPUT c-nome-abrev, 
                               INPUT c-nr-pedcli).
    RUN goToKey   IN hbodi154 (INPUT c-nome-abrev, 
                               INPUT c-nr-pedcli, 
                               INPUT i-sequencia, 
                               INPUT c-it-codigo, 
                               INPUT "").

    DO TRANSACTION:
        
        RUN deleteRecord IN hbodi154.
        RUN getRowErrors IN hbodi154 (OUTPUT TABLE RowErros).

        FIND FIRST RowErros NO-LOCK
            WHERE RowErros.ErrorSubType <> 'WARNING' NO-ERROR.
        IF AVAIL RowErros THEN DO:
            RUN pi-grava-erro-txt ('bodi149del').
            UNDO, RETURN 'NOK':u.
        END.
    END.

    RETURN 'OK':u.

END PROCEDURE. // deletaPedidoItemEntrega


/* Procedures "uteis" ***********************************************/

PROCEDURE inicializa-api:

    // Instancia da DBO ped-venda
    IF NOT VALID-HANDLE(hbodi159) THEN
        RUN dibo/bodi159.p    PERSISTENT SET hbodi159.
    
    IF NOT VALID-HANDLE(hbodi159sdf) THEN
        RUN dibo/bodi159sdf.p PERSISTENT SET hbodi159sdf.
    
    IF VALID-HANDLE(hbodi159) THEN
        RUN openQueryStatic IN hbodi159 (INPUT "ChPedido":U).
    ELSE DO:
        RUN destroyAll.
        RETURN 'NOK':u.
    END.

    // Instancia da DBO ped-item
    IF NOT VALID-HANDLE(hbodi154) THEN
        RUN dibo/bodi154.p    PERSISTENT SET hbodi154.

    IF NOT VALID-HANDLE(hbodi154sdf) THEN
        RUN dibo/bodi154sdf.p PERSISTENT SET hbodi154sdf.

    IF NOT VALID-HANDLE(hbodi154cal) THEN
        RUN dibo/bodi154cal.p PERSISTENT SET hbodi154cal.

    IF VALID-HANDLE(hbodi154) THEN
       RUN openQueryStatic IN hbodi154 (INPUT "DEFAULT":U).
    ELSE DO:
        RUN destroyAll.
        RETURN 'NOK':u.
    END.

    // Instancia da DBO ped-ent
    IF NOT VALID-HANDLE(hbodi159) THEN
        RUN dibo/bodi159.p    PERSISTENT SET hbodi159.
    
    IF VALID-HANDLE(hbodi159) THEN
        RUN openQueryStatic IN hbodi159 (INPUT "ChPedido":U).
    ELSE DO:
        RUN destroyAll.
        RETURN 'NOK':u.
    END.

END PROCEDURE.


PROCEDURE destroyAll:

    IF VALID-HANDLE(hbodi149) THEN DO:
        RUN destroyBo IN hbodi149 NO-ERROR.
        IF VALID-HANDLE(hbodi149) THEN DO:
            DELETE PROCEDURE hbodi149.
            ASSIGN hbodi149 = ?.
        END.
    END.
    IF VALID-HANDLE(hbodi149can) THEN DO:
        RUN destroyBo IN hbodi149can NO-ERROR.
        IF VALID-HANDLE(hbodi149can) THEN DO:
            DELETE PROCEDURE hbodi149can.
            ASSIGN hbodi149can = ?.
        END.
    END.
    IF VALID-HANDLE(hbodi159) THEN DO:
        RUN destroyBo IN hbodi159.
        IF VALID-HANDLE(hbodi159) THEN DO:
            DELETE PROCEDURE hbodi159.
            ASSIGN hbodi159 = ?.
        END.
    END.
    IF VALID-HANDLE(hbodi154) THEN DO:
        RUN destroyBo IN hbodi154.
        IF VALID-HANDLE(hbodi154) THEN DO:
            DELETE PROCEDURE hbodi154.
            ASSIGN hbodi154 = ?.
        END.
    END.
    IF VALID-HANDLE(hbodi154sdf) THEN DO:
        DELETE PROCEDURE hbodi154sdf.
        ASSIGN hbodi154sdf = ?.
    END.
    IF VALID-HANDLE(hbodi154cal) THEN DO:
        DELETE PROCEDURE hbodi154cal.
        ASSIGN hbodi154cal = ?.
    END.
END PROCEDURE.


/**
 *  Para a Caterpillar Ç definido uma natureza de operaá∆o diferente
 *  para cada tipo de pedido. 
 *  O tipo de pedido s∆o os 4 primeiros caracteres "numero" do pedido.
 ************************************************************************/
PROCEDURE pega-natureza-operacao :
    DEF INPUT        PARAM cod-estabel  AS    CHAR NO-UNDO.
    DEF INPUT        PARAM cod-emitente AS INTEGER NO-UNDO.
    DEF INPUT        PARAM nr-pedcli    AS    CHAR NO-UNDO.
    DEF INPUT-OUTPUT PARAM nat-oper     AS    CHAR NO-UNDO.

    ASSIGN nr-pedcli = REPLACE(nr-pedcli, "#", "").

    /*
    IF emitente.nat-oper <> "" THEN /* para os outros clientes usa o padr∆o */
        ASSIGN nat-oper = emitente.nat-oper.
    */

    IF cod-emitente = 10000124 THEN DO:
        IF nr-pedcli BEGINS "QAPP" OR nr-pedcli BEGINS "QAPC" THEN
            ASSIGN nat-oper  = "5101CA".
        IF nr-pedcli BEGINS "QHPP" OR nr-pedcli BEGINS "QHPC" THEN
            ASSIGN nat-oper  = "5124QH".
        IF nr-pedcli BEGINS "QRPP" THEN
            ASSIGN nat-oper  = "5124".
        IF nr-pedcli BEGINS "QYPP" OR nr-pedcli BEGINS "QYPD" THEN
            ASSIGN nat-oper  = "5102".
        IF nr-pedcli BEGINS "#QYPP" OR nr-pedcli BEGINS "#QYPD" THEN
            ASSIGN nat-oper  = "5102".            
    END. ELSE IF cod-emitente = 10005825 THEN DO:
        IF nr-pedcli BEGINS "QEST" OR nr-pedcli BEGINS "HETZ" THEN
            ASSIGN nat-oper  = "5101CB".
    END. ELSE IF cod-emitente = 10006602 THEN DO:
        IF nr-pedcli BEGINS "QAPD" THEN
            ASSIGN nat-oper  = IF cod-estabel = "100" THEN "6101CA"
                               ELSE "5101PA".
        IF nr-pedcli BEGINS "QHPD" THEN
           ASSIGN nat-oper  = IF cod-estabel = "100" THEN "6124QH"
                              ELSE "5124PA".
    END.

    IF cod-emitente = 10007155 THEN
        ASSIGN nat-oper = "6101".
END PROCEDURE.


PROCEDURE pega-natureza-operacao-item :
    DEF INPUT        PARAM cod-estabel  AS    CHAR NO-UNDO.
    DEF INPUT        PARAM cod-emitente AS INTEGER NO-UNDO.
    DEF INPUT        PARAM nr-pedcli    AS    CHAR NO-UNDO.
    DEF INPUT        PARAM it-codigo    AS    CHAR NO-UNDO.
    DEF INPUT-OUTPUT PARAM nat-oper     AS    CHAR NO-UNDO.
    
    FIND FIRST item-fatur-compl NO-LOCK
        WHERE item-fatur-compl.it-codigo = it-codigo NO-ERROR.
    IF AVAIL item-fatur-compl AND item-fatur-compl.nat-oper <> '' THEN
        ASSIGN nat-oper = item-fatur-compl.nat-oper.
    ELSE
        RUN pega-natureza-operacao (INPUT cod-estabel,
                                    INPUT cod-emitente,
                                    INPUT nr-pedcli,
                                    INPUT-OUTPUT nat-oper).

END PROCEDURE.


PROCEDURE pi-grava-erro-txt:
    DEF INPUT PARAM c-arquivo AS CHAR NO-UNDO.

    OUTPUT STREAM st-erro 
        TO VALUE('c:\temp\erros_' + c-arquivo + '.txt')
        APPEND 
        NO-CONVERT.

    PUT STREAM st-erro UNFORMATTED
        STRING(TODAY, '99/99/9999') ' '
        STRING(TIME, 'HH:MM:SS') ' ERROS: ' SKIP.
    FOR EACH RowErros NO-LOCK:
        EXPORT STREAM st-erro DELIMITER ";" RowErros .
    END.
    OUTPUT STREAM st-erro CLOSE.

END PROCEDURE.


PROCEDURE pi-cria-erro:
    DEF INPUT PARAM c-mensagem AS CHAR NO-UNDO.
    DEF INPUT PARAM c-help     AS CHAR NO-UNDO.

    IF c-help = '' THEN
        ASSIGN c-help = c-mensagem.

    CREATE RowErros.
    ASSIGN RowErros.errorNumber = 17006
           RowErros.errorDescription = c-mensagem
           RowErros.errorType        = 'ERROR':u
           RowErros.errorHelp        = c-help.

END PROCEDURE.

/* Functions ***********************************************************/

/**
 * Retorna a proxima hora de entrega.
 * = mais 10 segundos da hora anterior (no mesmo dia)
 ************************************************************************/
FUNCTION get-hr-entrega RETURNS CHAR 
                (nr-pedcli AS CHAR, 
                 nome-abrev AS CHAR,
                 it-codigo AS CHAR, 
                 dt-entrega AS DATE, 
                 c-hora AS CHAR):

    IF c-hora = '' THEN 
        ASSIGN c-hora = "000000".

    FOR EACH ped-ent NO-LOCK
            WHERE ped-ent.nr-pedcli  = nr-pedcli
              AND ped-ent.nome-abrev = nome-abrev
              AND ped-ent.it-codigo  = it-codigo
              AND ped-ent.dt-entrega = dt-entrega: 
        IF c-hora < ped-ent.hr-entrega THEN
            ASSIGN c-hora = ped-ent.hr-entrega.
    END.
    RETURN STRING(INT64(c-hora) + 10, "999999").
END FUNCTION.

