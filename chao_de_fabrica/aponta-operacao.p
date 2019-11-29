/* todo : colocar 
    Estab !! */
    
{c:/work/desenv/ccs/cf/funcoes-split.i}
{c:/work/desenv/ccs/cf/aponta-operacao.i}

FUNCTION hr-ult-apont-ctrab RETURNS INTEGER (cod-ctrab AS CHAR) FORWARD.
FUNCTION formata-hora RETURNS CHAR (hora AS INTEGER) FORWARD.

DEF VAR h-sfapi009 AS HANDLE NO-UNDO.

PROCEDURE processa-apontamento:
    DEF INPUT PARAM cod-operador LIKE operador.cod-operador NO-UNDO.
    DEF INPUT PARAM cod-ctrab    LIKE ctrab.cod-ctrab NO-UNDO.
    DEF INPUT PARAM r-rowid AS ROWID NO-UNDO.
    DEF INPUT PARAM qt-aprovada AS DECIMAL NO-UNDO INIT 0.0.
    DEF INPUT PARAM qt-refugada AS DECIMAL NO-UNDO INIT 0.0.
    DEF INPUT PARAM cod-motiv-refugo LIKE motiv-refugo.cod-motiv-refugo NO-UNDO.
    DEF INPUT PARAM termina-ordem AS LOGICAL INIT NO.
    DEF OUTPUT PARAM TABLE FOR tt-erro.

    FIND FIRST ctrab
         WHERE ctrab.cod-ctrab = cod-ctrab NO-LOCK NO-ERROR.
    IF NOT AVAIL ctrab THEN 
        RETURN "NOK". /* erro inesperado! */

    FIND FIRST split-operac NO-LOCK 
        WHERE ROWID(split-operac) = r-rowid NO-ERROR.
    IF NOT AVAIL split-operac THEN 
        RETURN "NOK". /* erro inesperado! */

    FIND FIRST ord-prod OF split-operac NO-LOCK NO-ERROR.
    /* DESABILITADO TEMPORARIAMENTE EM 2019-08-23
    IF AVAIL ord-prod AND (ord-prod.tipo = 2 OR ord-prod.tipo = 3) THEN DO:
        /* split-operac.gm-codigo MATCHES '*BNF*' */

        /* verifica se existe antes oc vinculada a op */
        FIND FIRST ordem-compra NO-LOCK
            WHERE ordem-compra.ordem-servic = split-operac.nr-ord-prod NO-ERROR.
        IF AVAIL ordem-compra THEN DO:
    
            /* por enquanto, verifica se existe algum recebimento.
             * nÆo ‚ o ideal isso ... */
            FIND FIRST ordem-compra NO-LOCK
                WHERE ordem-compra.ordem-servic = split-operac.nr-ord-prod
                  AND ordem-compra.situacao = 6
                  /*AND ordem-compra.op-codigo = split-operac.op-codigo*/ NO-ERROR.
            IF NOT AVAIL ordem-compra THEN DO:
                CREATE tt-erro.
                ASSIGN tt-erro.mensagem = "Existe ordem de compra vinculada a essa ordem de produ‡Æo.~n" +
                                          "Essa ordem ainda nÆo foi recebida, s¢ entÆo essa ordem poder  ser apontada" .
                RETURN 'NOK'.
            END.
        END.
    END.
    ***********************************/


    RUN pi-inicializa-api(termina-ordem).

    RUN setaRowSplitCtrab IN h-sfapi009 (INPUT r-rowid,
                                         INPUT ROWID(ctrab)).

    IF RETURN-VALUE = "NOK" THEN DO:
        RUN retornaTTErro IN h-sfapi009 (OUTPUT TABLE tt-erro).
    END. ELSE DO:        
        
        RUN recebeValorCampos IN h-sfapi009 ("operador", REPLACE(cod-operador, "-", ""), ?, ?, ?).
        RUN recebeValorCampos IN h-sfapi009 ("qtd-aprov",  "", ?, qt-aprovada, ?).
        RUN recebeValorCampos IN h-sfapi009 ("qtd-refgda", "", ?, qt-refugada, ?).
        IF qt-refugada > 0.0 THEN DO:
            RUN pi-refuga-peca(split-operac.nr-ord-prod,
                               split-operac.op-codigo,
                               qt-refugada,
                               cod-motiv-refugo,
                               r-rowid).
        END.
        
        RUN pi-ajusta-tempo-reporte(split-operac.nr-ord-prod, 
                                    split-operac.op-codigo,
                                    qt-aprovada + qt-refugada).

        RUN pi-ajusta-lote-reporte(split-operac.it-codigo).        

        RUN geraMobGGFAutomatico IN h-sfapi009.        
        RUN pi-processa-reservas (cod-ctrab, 
                                  split-operac.gm-codigo, 
                                  split-operac.it-codigo).

        FIND FIRST tt-reservas NO-LOCK NO-ERROR.
        IF NOT AVAIL tt-reservas THEN DO:
            RUN pi-recebe-tt-reservas IN h-sfapi009 (INPUT TABLE tt-reservas-final).
            RUN pi-cria-tt-reservas-alocacoes IN h-sfapi009.
        END. ELSE DO:
            FOR EACH tt-reservas NO-LOCK:
                CREATE tt-erro.
                ASSIGN tt-erro.mensagem = "Falta saldo item: " + tt-reservas.it-codigo + 
                                          "~nQuantidade: " + STRING(tt-reservas.quant-calc) + " " + tt-reservas.un + "(s)" + 
                                          "~nNo dep¢sito : " + tt-reservas.cod-depos + 
                                          "~nContatar o PCP" .
            END.
        END.        

        /* 
         * verifica se ficou alguma reserva aberta, que por algum motivo 
         *  o sistema ignorou na verifica‡Æo automatica.
         */
        RUN pi-verifica-reservas-sem-apontamento(split-operac.nr-ord-prod,
                                                 split-operac.op-codigo,
                                                 (qt-aprovada + qt-refugada)).
        
        
        FIND FIRST tt-erro NO-LOCK NO-ERROR.
        IF NOT AVAIL tt-erro THEN DO:

            RUN processaReporte IN h-sfapi009.
        
            IF RETURN-VALUE = "NOK" THEN DO:
                RUN retornaTTErro IN h-sfapi009 (OUTPUT TABLE tt-erro).
                FIND FIRST tt-erro NO-LOCK NO-ERROR.
                IF NOT AVAIL tt-erro THEN DO:
                    CREATE tt-erro.
                    ASSIGN tt-erro.mensagem = "Erro desconhecido, Informar o TI. Enviar todos os detalhes do apontamento.".
                END.
            END.
        END.
    END.

    RUN setaRowSplitCtrab IN h-sfapi009  /* wtf? */
            (INPUT ?, INPUT ?) NO-ERROR.
    RUN pi-finaliza-api.

    FIND FIRST tt-erro NO-LOCK NO-ERROR.
    IF AVAIL tt-erro THEN DO:
        /* do log */
        RETURN "NOK".
    END. 

    /*IF termina-ordem THEN DO:*/
        RUN pi-fechar-split( r-rowid ) NO-ERROR.
    /*END.*/
    
    
    RETURN "OK".
END PROCEDURE.

PROCEDURE processa-apontamento-data:
    DEF INPUT PARAM cod-operador LIKE operador.cod-operador NO-UNDO.
    DEF INPUT PARAM cod-ctrab    LIKE ctrab.cod-ctrab NO-UNDO.
    DEF INPUT PARAM r-rowid AS ROWID NO-UNDO.
    DEF INPUT PARAM qt-aprovada AS DECIMAL NO-UNDO INIT 0.0.
    DEF INPUT PARAM qt-refugada AS DECIMAL NO-UNDO INIT 0.0.
    DEF INPUT PARAM data-aponta AS DATE NO-UNDO.
    DEF INPUT PARAM cod-motiv-refugo LIKE motiv-refugo.cod-motiv-refugo NO-UNDO.
    DEF INPUT PARAM termina-ordem AS LOGICAL INIT NO.
    DEF OUTPUT PARAM TABLE FOR tt-erro.    

    FIND FIRST ctrab
         WHERE ctrab.cod-ctrab = cod-ctrab NO-LOCK NO-ERROR.
    IF NOT AVAIL ctrab THEN 
        RETURN "NOK".

    FIND FIRST split-operac NO-LOCK 
        WHERE ROWID(split-operac) = r-rowid NO-ERROR.
    IF NOT AVAIL split-operac THEN 
        RETURN "NOK".

    RUN pi-inicializa-api(termina-ordem).
    RUN setaRowSplitCtrab IN h-sfapi009 (INPUT r-rowid,
                                         INPUT ROWID(ctrab)).

    IF RETURN-VALUE = "NOK" THEN DO:    
        RUN retornaTTErro IN h-sfapi009 (OUTPUT TABLE tt-erro).
    END. ELSE DO:        
        
        RUN recebeValorCampos IN h-sfapi009 ("operador", REPLACE(cod-operador, "-", ""), ?, ?, ?).
        RUN recebeValorCampos IN h-sfapi009 ("qtd-aprov",  "", ?, qt-aprovada, ?).
        RUN recebeValorCampos IN h-sfapi009 ("qtd-refgda", "", ?, qt-refugada, ?).
        IF qt-refugada > 0.0 THEN DO:
            RUN pi-refuga-peca(split-operac.nr-ord-prod,
                               split-operac.op-codigo,
                               qt-refugada,
                               cod-motiv-refugo,
                               r-rowid).            
        END.
        
        RUN pi-ajusta-tempo-reporte-data
                                   (split-operac.nr-ord-prod, 
                                    split-operac.op-codigo,
                                    qt-aprovada + qt-refugada,
                                    data-aponta).

        RUN pi-ajusta-lote-reporte(split-operac.it-codigo).        

        RUN geraMobGGFAutomatico IN h-sfapi009.        
        RUN pi-processa-reservas (cod-ctrab, 
                                  split-operac.gm-codigo, 
                                  split-operac.it-codigo).

        FIND FIRST tt-reservas NO-LOCK NO-ERROR.
        IF NOT AVAIL tt-reservas THEN DO:
            RUN pi-recebe-tt-reservas IN h-sfapi009 (INPUT TABLE tt-reservas-final).
            RUN pi-cria-tt-reservas-alocacoes IN h-sfapi009.
        END. ELSE DO:
            FOR EACH tt-reservas NO-LOCK:
                CREATE tt-erro.
                ASSIGN tt-erro.mensagem = "Falta saldo item: " + tt-reservas.it-codigo + 
                                          "~nQuantidade: " + STRING(tt-reservas.quant-calc) + " " + tt-reservas.un + "(s)" + 
                                          "~nNo dep¢sito : " + tt-reservas.cod-depos + 
                                          "~nContatar o PCP" .
            END.
        END.        

        /* 
         * verifica se ficou alguma reserva aberta, que por algum motivo 
         *  o sistema ignorou na verifica‡Æo automatica.
         */
        RUN pi-verifica-reservas-sem-apontamento(split-operac.nr-ord-prod,
                                                 split-operac.op-codigo,
                                                 (qt-aprovada + qt-refugada)).
        
        
        FIND FIRST tt-erro NO-LOCK NO-ERROR.
        IF NOT AVAIL tt-erro THEN DO:

            RUN processaReporte IN h-sfapi009.
            
            IF RETURN-VALUE = "NOK" THEN DO:
                RUN retornaTTErro IN h-sfapi009 (OUTPUT TABLE tt-erro).
                FIND FIRST tt-erro NO-LOCK NO-ERROR.
                IF NOT AVAIL tt-erro THEN DO:
                    CREATE tt-erro.
                    ASSIGN tt-erro.mensagem = "Erro desconhecido, Informar o TI. Enviar todos os detalhes do apontamento.".
                END.
            END.
        END.
    END.

    RUN setaRowSplitCtrab IN h-sfapi009  /* wtf? */
            (INPUT ?, INPUT ?) NO-ERROR.
    RUN pi-finaliza-api.

    FIND FIRST tt-erro NO-LOCK NO-ERROR.
    IF AVAIL tt-erro THEN DO:
        /* do log */
        RETURN "NOK".
    END.
    
    IF termina-ordem THEN DO:
        RUN pi-fechar-split( r-rowid ) NO-ERROR.
    END.
    
    RETURN "OK".
END PROCEDURE.

PROCEDURE pi-refuga-peca:
    DEF INPUT PARAM nr-ord-prod LIKE ord-prod.nr-ord-prod NO-UNDO.
    DEF INPUT PARAM op-codigo   LIKE oper-ord.op-codigo   NO-UNDO.
    DEF INPUT PARAM qt-refugada AS DECIMAL NO-UNDO.
    DEF INPUT PARAM cod-motiv-refugo LIKE motiv-refugo.cod-motiv-refugo NO-UNDO.
    DEF INPUT PARAM r-split     AS ROWID NO-UNDO.
    
    CREATE tt-ref. 
    ASSIGN tt-ref.line              = 1
           tt-ref.rw-split-operac   = r-split
           tt-ref.nr-ord-prod       = nr-ord-prod
           tt-ref.cod-motiv-refugo  = cod-motiv-refugo
           tt-ref.des-motiv-refugo  = ""
           tt-ref.codigo-rejei      = 0
           tt-ref.qtd-operac-refgda = qt-refugada
           tt-ref.qtd-operac-retrab = 0.
    RUN pi-recebe-tt-ref IN h-sfapi009 (INPUT TABLE tt-ref).

    /* TODO: 2017-03-31
     *****************************************
     * Se existir baixa de reserva na opera‡…o
     * precisa requisitar esses itens?
     ******************************************/

    RETURN "OK".       
END PROCEDURE.

PROCEDURE refuga-operacoes-posteriores:
    DEF INPUT PARAM nr-ord-prod LIKE ord-prod.nr-ord-prod NO-UNDO.
    DEF INPUT PARAM op-codigo   LIKE oper-ord.op-codigo   NO-UNDO.
    DEF INPUT PARAM qt-refugada AS DECIMAL NO-UNDO.
    
    DEF BUFFER b-split FOR split-operac.

    FOR EACH b-oper-ord NO-LOCK 
            WHERE b-oper-ord.nr-ord-prod = nr-ord-prod
              AND b-oper-ord.op-codigo > op-codigo:

        FIND FIRST b-split OF b-oper-ord NO-LOCK
            WHERE b-split.ind-estado-split < 5 NO-ERROR.
        IF NOT AVAIL b-split THEN 
            NEXT.
    
        IF qt-refugada > b-split.qtd-previs-operac - (b-split.qtd-operac-aprov + b-split.qtd-operac-refgda) THEN 
            NEXT.
    
        RUN divide-split( ROWID(b-split), 
                          qt-refugada).
    
        FIND FIRST b-split OF b-oper-ord NO-LOCK
            WHERE b-split.ind-estado-split < 5 
              AND b-split.qtd-previs-operac = qt-refugada NO-ERROR.
        IF NOT AVAIL b-split THEN 
            NEXT.
    
        RUN pi-fechar-split( ROWID(b-split) ).
    END.
END PROCEDURE.

PROCEDURE pi-fechar-split:
    DEF INPUT PARAM r-split AS ROWID NO-UNDO.

    DEF VAR h-boin536 AS HANDLE NO-UNDO.

    FIND FIRST split-operac NO-LOCK 
        WHERE ROWID(split-operac) = r-split NO-ERROR.
    IF split-operac.ind-estado-split >= 5 THEN 
        LEAVE.

    RUN inbo/boin536.r PERSISTENT SET h-boin536.
    RUN FecharSplit IN h-boin536 (BUFFER split-operac).
    RUN openQuery IN h-boin536 (1).
    RUN findFirst IN h-boin536.
    DELETE PROCEDURE h-boin536 NO-ERROR.
    ASSIGN h-boin536 = ?.

END PROCEDURE.

FUNCTION hr-ult-apont-ctrab RETURNS INTEGER (cod-ctrab AS CHAR):
    DEF VAR tempo-inicial AS INTEGER NO-UNDO INIT 0.
    FOR EACH split-operac NO-LOCK 
            WHERE split-operac.cod-ctrab = cod-ctrab
              AND split-operac.dat-fim-operac = TODAY
            BY split-operac.qtd-segs-fim-operac DESC:
        ASSIGN tempo-inicial = split-operac.qtd-segs-fim-operac.
        LEAVE.
    END.    
    RETURN tempo-inicial.
END FUNCTION.

FUNCTION formata-hora RETURNS CHAR (hora AS INTEGER):
    RETURN REPLACE(STRING(hora, "HH:MM"), ":", "").
END FUNCTION.

PROCEDURE pi-processa-reservas:
    DEF INPUT PARAM cod-ctrab LIKE ctrab.cod-ctrab NO-UNDO.
    DEF INPUT PARAM gm-codigo LIKE grup-maquin.gm-codigo NO-UNDO.
    DEF INPUT PARAM it-codigo LIKE ITEM.it-codigo NO-UNDO.

    DEF BUFFER b-ord-prod FOR ord-prod.

    /*DEF VAR cod-depos-a  AS CHAR NO-UNDO.*/
    DEF VAR cod-depos    AS CHAR NO-UNDO.
    DEF VAR quant-requis AS DECIMAL NO-UNDO.
    DEF VAR qt-req-aux   AS DECIMAL NO-UNDO.
    
    DEF VAR sequencia AS INTEGER NO-UNDO INIT 1.

    ASSIGN cod-depos = deposito-gm-codigo(gm-codigo).
    IF cod-depos = '' OR gm-codigo <> get-grup-maquina-ctrab(cod-ctrab) THEN DO:
        ASSIGN cod-depos = deposito-ctrab(cod-ctrab).
        IF cod-depos = '' THEN DO:
            FIND FIRST ITEM NO-LOCK 
                WHERE ITEM.it-codigo = it-codigo NO-ERROR.
            IF AVAIL ITEM THEN
                ASSIGN cod-depos = ITEM.deposito-pad.
            ELSE DO:
                CREATE tt-erro.
                ASSIGN tt-erro.mensagem = "Deposito padrÆo da opera‡Æo nÆo foi localizado.~nContatar o PCP".
                RETURN "NOK".
            END.
        END.
    END.

    EMPTY TEMP-TABLE tt-reservas.
    EMPTY TEMP-TABLE tt-reservas-final.

    RUN pi-cria-tt-reservas-alocacoes IN h-sfapi009.    
    RUN verificaSaldoReservas IN h-sfapi009.    
    RUN pi-retorna-tt-reservas IN h-sfapi009 (OUTPUT TABLE tt-reservas).

    FOR EACH tt-reservas NO-LOCK,
        FIRST b-reservas OF tt-reservas NO-LOCK,
        FIRST b-ord-prod OF b-reservas NO-LOCK :

        ASSIGN quant-requis = tt-reservas.quant-requis.

        /*FIND FIRST b-saldo-estoq NO-LOCK 
            WHERE b-saldo-estoq.cod-estab = b-ord-prod.cod-estabel
              AND b-saldo-estoq.it-codigo = tt-reservas.it-codigo
              AND b-saldo-estoq.cod-depos = cod-depos
              AND b-saldo-estoq.qtidade-atu > 0 NO-ERROR.
        IF AVAIL b-saldo-estoq THEN
            ASSIGN cod-depos-a = cod-depos.
        ELSE
            ASSIGN cod-depos-a = tt-reservas.cod-depos.*/
    
        verifica_reservas:
        FOR EACH b-saldo-estoq NO-LOCK 
                WHERE b-saldo-estoq.cod-estab = b-ord-prod.cod-estabel
                  AND b-saldo-estoq.it-codigo = tt-reservas.it-codigo
                  AND b-saldo-estoq.cod-depos = cod-depos
                  AND b-saldo-estoq.qtidade-atu > 0
                  AND (b-saldo-estoq.dt-vali-lote = ? OR 
                       b-saldo-estoq.dt-vali-lote >= TODAY):
           
            IF quant-requis <= 0 THEN DO:
                LEAVE verifica_reservas.
            END.
    
            IF quant-requis >= b-saldo-estoq.qtidade-atu THEN
                ASSIGN qt-req-aux = b-saldo-estoq.qtidade-atu.
            ELSE
                ASSIGN qt-req-aux = quant-requis.
            ASSIGN quant-requis = quant-requis - qt-req-aux.

            /*
             * TODO:
             * as vezes o saldo que ir  sobrar ‚ menor que uma chapa.
             * bem menor na verdade (gramas ou at‚ menos).
             * ver uma forma de requisitar o saldo todo aqui nesses casos.
             */
            
            CREATE tt-reservas-final.
            ASSIGN tt-reservas-final.proporcao = tt-reservas.proporcao
                   tt-reservas-final.log-sem-saldo  = NO
                   tt-reservas-final.nr-ord-produ = tt-reservas.nr-ord-produ
                   tt-reservas-final.cod-refer = tt-reservas.cod-refer
                   tt-reservas-final.it-codigo = tt-reservas.it-codigo
                   tt-reservas-final.quant-orig  = b-reservas.quant-orig
                   tt-reservas-final.quant-aloc = b-reservas.quant-aloc
                   tt-reservas-final.quant-atend = tt-reservas.quant-atend
                   tt-reservas-final.quant-calc = tt-reservas.quant-calc
                   tt-reservas-final.quant-requis = qt-req-aux
            
                   tt-reservas-final.cod-depos = b-saldo-estoq.cod-depos 
                   tt-reservas-final.cod-localiz =  b-saldo-estoq.cod-localiz
                   tt-reservas-final.lote-serie = b-saldo-estoq.lote 
                   tt-reservas-final.dt-vali-lote =  b-saldo-estoq.dt-vali-lote
                   tt-reservas-final.un = tt-reservas.un
                   tt-reservas-final.estado = tt-reservas.estado
                   tt-reservas-final.tipo-sobra  = tt-reservas.tipo-sobra
                   tt-reservas-final.item-pai  = tt-reservas.item-pai
                   tt-reservas-final.op-codigo = tt-reservas.op-codigo
                   tt-reservas-final.cod-roteiro = tt-reservas.cod-roteiro
                   tt-reservas-final.veiculo  = tt-reservas.veiculo
                   tt-reservas-final.per-ppm  = tt-reservas.per-ppm
                   tt-reservas-final.per-ppm-lote = tt-reservas.per-ppm-lote
                   tt-reservas-final.tipo-formula  = tt-reservas.tipo-formula
                   tt-reservas-final.qt-atend-lote = tt-reservas.qt-atend-lote
                   tt-reservas-final.qt-requis-lote = tt-reservas.qt-requis-lote
                   tt-reservas-final.qt-aloc-lote = tt-reservas.qt-aloc-lote
                  
                   tt-reservas-final.alternativo-de  = tt-reservas.alternativo-de
                   tt-reservas-final.cod-refer-it-original = tt-reservas.cod-refer-it-original
                  
                   tt-reservas-final.qt-atend-res = tt-reservas.qt-atend-res
                   tt-reservas-final.qt-atend-lote-res = tt-reservas.qt-atend-lote-res
                   tt-reservas-final.quant-calc-orig = tt-reservas.quant-calc-orig
                    
                   tt-reservas-final.processada  = tt-reservas.processada
                   tt-reservas-final.rw-reserva  = tt-reservas.rw-reserva
                   tt-reservas-final.rw-saldo-estoq  = ROWID(b-saldo-estoq)
                   tt-reservas-final.tipo-ordem  = tt-reservas.tipo-ordem
                   tt-reservas-final.tempo  = tt-reservas.tempo
                   tt-reservas-final.tentativas  = tt-reservas.tentativas
                   tt-reservas-final.sequencia  =  sequencia /*tt-reservas.sequencia*/
                   .
            ASSIGN sequencia = sequencia + 1.
        END.
        
        IF quant-requis <= 0 THEN 
            DELETE tt-reservas.
    END.

    FOR EACH tt-reservas SHARE-LOCK:
        ASSIGN tt-reservas.cod-depos = cod-depos.
    END.

END PROCEDURE.

PROCEDURE pi-config-api:

    CREATE tt-param-reporte.
    ASSIGN tt-param-reporte.cod-param = "l-gera-rowErrors"
           tt-param-reporte.log-result-param = YES.

    CREATE tt-param-reporte.
    ASSIGN tt-param-reporte.cod-param = "l-on-line"
           tt-param-reporte.log-result-param = NO.

    CREATE tt-param-reporte.
    ASSIGN tt-param-reporte.cod-param = "l-erro-manut"
           tt-param-reporte.log-result-param = NO.

    CREATE tt-param-reporte.
    ASSIGN tt-param-reporte.cod-param = "l-msg-req-manual-continua"
           tt-param-reporte.log-result-param = NO.

    CREATE tt-param-reporte.
    ASSIGN tt-param-reporte.cod-param = "l-msg-termina-ordem"
           tt-param-reporte.log-result-param = YES /*NO*/ .

    CREATE tt-param-reporte.
    ASSIGN tt-param-reporte.cod-param = "l-msg-termina-com-operac-incompl"
           tt-param-reporte.log-result-param = NO.

    CREATE tt-param-reporte.
    ASSIGN tt-param-reporte.cod-param = "l-msg-finaliza-split-operac"
           tt-param-reporte.log-result-param = YES /*NO*/ .

    CREATE tt-param-reporte.
    ASSIGN tt-param-reporte.cod-param = "l-encerra-ordem-ultima-operacao"
           tt-param-reporte.log-result-param = YES.

    CREATE tt-param-reporte.
    ASSIGN tt-param-reporte.cod-param = "l-msg-qtd-menor-rep-ant"
           tt-param-reporte.log-result-param = NO.

    CREATE tt-param-reporte.
    ASSIGN tt-param-reporte.cod-param = "l-msg-reporta-tempo-dif-real"
           tt-param-reporte.log-result-param = NO.

    CREATE tt-param-reporte.
    ASSIGN tt-param-reporte.cod-param = "l-msg-parada-sobrep-continua"
           tt-param-reporte.log-result-param = NO.

    CREATE tt-param-reporte.
    ASSIGN tt-param-reporte.cod-param = "l-msg-reservas-n-requis-continua"
           tt-param-reporte.log-result-param = NO.

    CREATE tt-param-reporte.
    ASSIGN tt-param-reporte.cod-param = "l-msg-requisita-saldo-disponivel"
           tt-param-reporte.log-result-param = NO.

    CREATE tt-param-reporte.
    ASSIGN tt-param-reporte.cod-param = "l-msg-nao-req-sem-saldo-sufic"
           tt-param-reporte.log-result-param = NO.

    CREATE tt-param-reporte.
    ASSIGN tt-param-reporte.cod-param = "l-msg-sem-reporte-mob"
           tt-param-reporte.log-result-param = NO.

    CREATE tt-param-reporte.
    ASSIGN tt-param-reporte.cod-param = "l-msg-sem-reporte-ggf"
           tt-param-reporte.log-result-param = NO.

    CREATE tt-param-reporte.
    ASSIGN tt-param-reporte.cod-param = "l-msg-sobrepoe-veiculos"
           tt-param-reporte.log-result-param = NO.

    CREATE tt-param-reporte.
    ASSIGN tt-param-reporte.cod-param = "l-msg-veic-sem-saldo-continua"
           tt-param-reporte.log-result-param = NO.

    CREATE tt-param-reporte.
    ASSIGN tt-param-reporte.cod-param = "l-valida-op-anterior"
           tt-param-reporte.log-result-param = YES.
    
    CREATE tt-param-reporte.
    ASSIGN tt-param-reporte.cod-param = "i-baixa-reservas"
           tt-param-reporte.dec-result-param = 1.
    
    CREATE tt-param-reporte.
    ASSIGN tt-param-reporte.cod-param = "l-informa-deposito"
           tt-param-reporte.log-result-param = NO.
    
    CREATE tt-param-reporte.
    ASSIGN tt-param-reporte.cod-param = "l-informa-localizacao"
           tt-param-reporte.log-result-param = NO.
    
    CREATE tt-param-reporte.
    ASSIGN tt-param-reporte.cod-param = "l-requisicao-automatica"
           tt-param-reporte.log-result-param = YES.
    
    CREATE tt-param-reporte.
    ASSIGN tt-param-reporte.cod-param = "l-buscao-saldos"
           tt-param-reporte.log-result-param = YES.
    
    RUN recebeParametros IN h-sfapi009 (INPUT TABLE tt-param-reporte).


END PROCEDURE.

PROCEDURE pi-configura-api:
    DEF INPUT PARAM termina-ordem AS LOGICAL NO-UNDO.

    CREATE tt-param-reporte.
    ASSIGN tt-param-reporte.cod-param = "l-on-line"
           tt-param-reporte.log-result-param = NO.

    CREATE tt-param-reporte.
    ASSIGN tt-param-reporte.cod-param = "l-online"
           tt-param-reporte.log-result-param = NO.
    
    CREATE tt-param-reporte.
    ASSIGN tt-param-reporte.cod-param = "l-gera-rowErrors"
           tt-param-reporte.log-result-param = YES.
    
    CREATE tt-param-reporte.
    ASSIGN tt-param-reporte.cod-param = "l-msg-req-manual-continua"
           tt-param-reporte.log-result-param = NO.
           
    CREATE tt-param-reporte.
    ASSIGN tt-param-reporte.cod-param = "l-msg-finaliza-split-operac"
           tt-param-reporte.log-result-param = YES.  /* termina-ordem. */
           
    CREATE tt-param-reporte.
    ASSIGN tt-param-reporte.cod-param = "l-msg-termina-ordem" 
           tt-param-reporte.log-result-param = YES /*termina-ordem*/ .
           
    CREATE tt-param-reporte.
    ASSIGN tt-param-reporte.cod-param = "l-msg-termina-com-operac-incompl" /* testar isso */
           tt-param-reporte.log-result-param = NO.
    
    /*
    CREATE tt-param-reporte.
    ASSIGN tt-param-reporte.cod-param = "l-msg-termina-operac-incompl" 
           tt-param-reporte.log-result-param = termina-ordem.*/
           
    CREATE tt-param-reporte.
    ASSIGN tt-param-reporte.cod-param = "l-encerra-ordem-ultima-operacao"
           tt-param-reporte.log-result-param = YES.
    
    CREATE tt-param-reporte.
    ASSIGN tt-param-reporte.cod-param = "l-msg-reporta-tempo-dif-real"
           tt-param-reporte.log-result-param =  YES.
    
    CREATE tt-param-reporte.
    ASSIGN tt-param-reporte.cod-param = "l-msg-qtd-menor-rep-ant"
           tt-param-reporte.log-result-param = NO.
    
    CREATE tt-param-reporte.
    ASSIGN tt-param-reporte.cod-param = "l-msg-reservas-n-requis-continua"
           tt-param-reporte.log-result-param = YES.
    
    CREATE tt-param-reporte.
    ASSIGN tt-param-reporte.cod-param = "l-msg-requisita-saldo-disponivel"
           tt-param-reporte.log-result-param = NO.
    
    CREATE tt-param-reporte.
    ASSIGN tt-param-reporte.cod-param = "l-msg-nao-req-sem-saldo-sufic"
           tt-param-reporte.log-result-param = YES.
    
    CREATE tt-param-reporte.
    ASSIGN tt-param-reporte.cod-param = "l-msg-sem-reporte-ggf"
           tt-param-reporte.log-result-param = YES.
    
    CREATE tt-param-reporte.
    ASSIGN tt-param-reporte.cod-param = "l-msg-sem-reporte-mob"
           tt-param-reporte.log-result-param = YES.
    
    CREATE tt-param-reporte.
    ASSIGN tt-param-reporte.cod-param = "l-valida-op-anterior"
           tt-param-reporte.log-result-param = YES.
    
    CREATE tt-param-reporte.
    ASSIGN tt-param-reporte.cod-param = "i-baixa-reservas"
           tt-param-reporte.dec-result-param = 1.
    
    CREATE tt-param-reporte.
    ASSIGN tt-param-reporte.cod-param = "l-informa-deposito"
           tt-param-reporte.log-result-param = NO.
    
    CREATE tt-param-reporte.
    ASSIGN tt-param-reporte.cod-param = "l-informa-localizacao"
           tt-param-reporte.log-result-param = NO.
    
    CREATE tt-param-reporte.
    ASSIGN tt-param-reporte.cod-param = "l-requisicao-automatica"
           tt-param-reporte.log-result-param = YES.
    
    CREATE tt-param-reporte.
    ASSIGN tt-param-reporte.cod-param = "l-buscao-saldos"
           tt-param-reporte.log-result-param = YES.
    
    RUN recebeParametros IN h-sfapi009 (INPUT TABLE tt-param-reporte).
END PROCEDURE.

PROCEDURE pi-inicializa-api:
    DEF INPUT PARAM termina-ordem AS LOGICAL NO-UNDO.

    IF NOT VALID-HANDLE(h-sfapi009) THEN DO:
        RUN sfc/sfapi009.r PERSISTENT SET h-sfapi009
                (INPUT 205,
                 INPUT TABLE tt-import-rep-oper,
                 OUTPUT TABLE tt-erro).
    END.
    EMPTY TEMP-TABLE tt-param-reporte.
    RUN pi-configura-api(termina-ordem).

    EMPTY TEMP-TABLE tt-ref.
    EMPTY TEMP-TABLE tt-reservas.
    EMPTY TEMP-TABLE tt-reservas-final.
END PROCEDURE.

PROCEDURE pi-finaliza-api:
    RUN setaRowSplitCtrab IN h-sfapi009 (INPUT ?, INPUT ?) NO-ERROR.
    RUN finalizaAPI IN h-sfapi009.
    /*IF VALID-HANDLE(h-sfapi009) THEN*/
        DELETE PROCEDURE h-sfapi009 NO-ERROR.
    ASSIGN h-sfapi009 = ?.
END PROCEDURE.

PROCEDURE pi-ajusta-tempo-reporte:
    DEF INPUT PARAM nr-ord-prod LIKE ord-prod.nr-ord-prod NO-UNDO.
    DEF INPUT PARAM op-codigo   LIKE oper-ord.op-codigo   NO-UNDO.
    DEF INPUT PARAM qt-apontada AS DECIMAL NO-UNDO.

    DEF VAR c-aux  AS CHAR NO-UNDO.
    DEF VAR de-aux AS DECIMAL NO-UNDO.
    DEF VAR dt-aux AS DATE NO-UNDO INIT TODAY.

    DEF VAR dt-inicial AS DATE NO-UNDO INIT TODAY.
    DEF VAR dt-final   AS DATE NO-UNDO INIT TODAY.

    DEF VAR tempo-inicial  AS INTEGER NO-UNDO.
    DEF VAR tempo-final    AS INTEGER NO-UNDO.
    DEF VAR tempo-operacao AS INTEGER NO-UNDO.

    ASSIGN tempo-operacao = get-tempo-operacao(nr-ord-prod,
                                               op-codigo,
                                               qt-apontada)
            tempo-inicial  = TIME - tempo-operacao /*hr-ult-apont-ctrab(cod-ctrab)*/
            tempo-final    = TIME.

    IF tempo-inicial < 0 THEN
        ASSIGN dt-inicial = TODAY - 1.

    RUN ajustaTempoPelaQtd IN h-sfapi009 (INPUT dt-inicial,
                                          INPUT formata-hora(tempo-inicial),
                                          INPUT-OUTPUT dt-aux,
                                          INPUT-OUTPUT c-aux,
                                          INPUT qt-apontada,
                                          OUTPUT de-aux).
    RUN recebeValorCampos IN h-sfapi009 ("dat-fim-reporte", "", ?, ?, dt-final).
    RUN recebeValorCampos IN h-sfapi009 ("hra-fim-rep", formata-hora(tempo-final), ?, ?, ?).
END PROCEDURE.

PROCEDURE pi-ajusta-tempo-reporte-data:
    DEF INPUT PARAM nr-ord-prod LIKE ord-prod.nr-ord-prod NO-UNDO.
    DEF INPUT PARAM op-codigo   LIKE oper-ord.op-codigo   NO-UNDO.
    DEF INPUT PARAM qt-apontada AS DECIMAL NO-UNDO.
    DEF INPUT PARAM dt-aponta   AS DATE NO-UNDO.

    DEF VAR c-aux  AS CHAR NO-UNDO.
    DEF VAR de-aux AS DECIMAL NO-UNDO.
    DEF VAR dt-aux AS DATE NO-UNDO.

    DEF VAR dt-inicial AS DATE NO-UNDO.
    DEF VAR dt-final   AS DATE NO-UNDO.

    DEF VAR tempo-inicial  AS INTEGER NO-UNDO.
    DEF VAR tempo-final    AS INTEGER NO-UNDO.
    DEF VAR tempo-operacao AS INTEGER NO-UNDO.


    ASSIGN dt-inicial = dt-aponta
           dt-final   = dt-aponta
           dt-aux     = dt-aponta
           tempo-operacao = get-tempo-operacao(nr-ord-prod,
                                               op-codigo,
                                               qt-apontada)
           tempo-inicial  = TIME - tempo-operacao /*hr-ult-apont-ctrab(cod-ctrab)*/
           tempo-final    = TIME.

    IF tempo-inicial < 0 THEN
        ASSIGN dt-inicial = dt-aponta - 1.

    RUN ajustaTempoPelaQtd IN h-sfapi009 (INPUT dt-inicial,
                                          INPUT formata-hora(tempo-inicial),
                                          INPUT-OUTPUT dt-aux,
                                          INPUT-OUTPUT c-aux,
                                          INPUT qt-apontada,
                                          OUTPUT de-aux).
    RUN recebeValorCampos IN h-sfapi009 ("dat-fim-reporte", "", ?, ?, dt-final).
    RUN recebeValorCampos IN h-sfapi009 ("hra-fim-rep", formata-hora(tempo-final), ?, ?, ?).
END PROCEDURE.

PROCEDURE pi-ajusta-lote-reporte:
    DEF INPUT PARAM it-codigo LIKE ITEM.it-codigo NO-UNDO.

    DEF VAR lote AS CHAR NO-UNDO.
    DEF VAR c-dep-acab AS CHAR NO-UNDO.

    FIND FIRST ITEM NO-LOCK 
        WHERE ITEM.it-codigo = it-codigo NO-ERROR.
    IF NOT AVAIL ITEM THEN 
        LEAVE.
    
    /* TESTAR */
    DEF VAR de-aux AS DECIMAL NO-UNDO.
    DEF VAR in-aux AS INTEGER NO-UNDO.
    DEF VAR dt-aux AS DATE NO-UNDO INIT TODAY.
              
    ASSIGN de-aux = 0
           in-aux = 0
           dt-aux = ?.
    RUN retornaValorCampos IN h-sfapi009 ("dep-acab", OUTPUT c-dep-acab, OUTPUT in-aux, OUTPUT de-aux, OUTPUT dt-aux).

    IF item.contr-qualid = YES OR ITEM.it-codigo BEGINS "5." OR c-dep-acab = 'CQ' THEN DO:
        RUN recebeValorCampos IN h-sfapi009 ("dep-acab", ITEM.deposito-pad, ?, ?, ?).
    END.

    IF item.tipo-con-est = 3 THEN DO: /* controlado por lote, entÆo cria um lote */ 
        RUN upc/p-ccs-lote.p (OUTPUT lote).
        RUN recebeValorCampos IN h-sfapi009 ("lote-acab", lote, ?, ?, ?).
        RUN recebeValorCampos IN h-sfapi009 ("dt-valid-lote", "", ?, ?, 12/31/3999).
    END.    
END PROCEDURE.

PROCEDURE pi-verifica-reservas-sem-apontamento:
    DEF INPUT PARAM nr-ord-prod LIKE ord-prod.nr-ord-prod NO-UNDO.
    DEF INPUT PARAM op-codigo   LIKE oper-ord.op-codigo   NO-UNDO.
    DEF INPUT PARAM qt-apontado AS DECIMAL NO-UNDO.

    DEF VAR qtd-requis   AS DECIMAL NO-UNDO.
    DEF VAR qtd-variacao AS DECIMAL NO-UNDO.
    DEF VAR sld-operacao AS DECIMAL NO-UNDO.

    ASSIGN sld-operacao = saldo-operacao(nr-ord-prod, op-codigo).

    IF sld-operacao <> qt-apontado OR 
       NOT ultima-operacao(nr-ord-prod, op-codigo) THEN 
        LEAVE.

    FOR EACH b-reservas NO-LOCK 
        WHERE b-reservas.nr-ord-prod = nr-ord-prod
          AND b-reservas.estado = 1:

        FIND FIRST tt-reservas NO-LOCK
            WHERE tt-reservas.it-codigo = b-reservas.it-codigo NO-ERROR.
        IF AVAIL tt-reservas THEN 
            NEXT. /* j  vai aparecer erro de saldo do item ... ignora */

        FIND FIRST tt-reservas-final NO-LOCK
            WHERE tt-reservas-final.it-codigo = b-reservas.it-codigo NO-ERROR.
        IF AVAIL tt-reservas-final THEN 
            NEXT. /* vai baixar pois tem saldo (s¢ que ainda nÆo fez)  */

        ASSIGN qtd-variacao = b-reservas.quant-orig
               qtd-requis = b-reservas.quant-atend + b-reservas.quant-aloc. /* TOTEST: validar se fica alocado mesmo !!! */

        FOR FIRST item-manuf-estab NO-LOCK 
                WHERE item-manuf-estab.it-codigo = b-reservas.it-codigo :
            ASSIGN qtd-variacao = qtd-variacao - qtd-variacao * item-manuf-estab.var-apto-menor / 100.
        END.

        IF qtd-variacao > qtd-requis THEN DO:
            CREATE tt-erro.
            ASSIGN tt-erro.mensagem = "Item: " + b-reservas.it-codigo + " nÆo foi requisitado completamente." +
                                      "~nQuantidade : " + STRING(b-reservas.quant-orig) + 
                                      "~nRequisitado: " + STRING(qtd-requis) + 
                                      "~nContatar o PCP.".
        END.
    END.
END PROCEDURE.
