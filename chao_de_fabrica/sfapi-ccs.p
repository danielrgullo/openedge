{c:/work/desenv/ccs/cf/aponta-operacao.i}
{c:/work/desenv/ccs/cf/funcoes-operacoes.i}

DEF BUFFER b-split-operac FOR split-operac.

DEF VAR h-sfapi009 AS HANDLE NO-UNDO.

/**
 * Inicializa a SFAPI009 para realizar os apontamentos
 ****************************************************************/
PROCEDURE pi-inicializa-api:

    IF NOT VALID-HANDLE(h-sfapi009) THEN DO:
        RUN sfc/sfapi009.r PERSISTENT SET h-sfapi009
                (INPUT 205,
                 INPUT TABLE tt-import-rep-oper,
                 OUTPUT TABLE tt-erro).
    END.    
END PROCEDURE.

/**
 * Executa a configuraá∆o da API de apontamentos
 * e limpa todas as tabelas tempor†rias.
 ****************************************************************/
PROCEDURE configura-apontamento:
    IF NOT VALID-HANDLE(h-sfapi009) THEN DO:
        RUN pi-inicializa-api.
    END.
    EMPTY TEMP-TABLE tt-param-reporte.
    RUN pi-configura-api.
    EMPTY TEMP-TABLE tt-ref.
    EMPTY TEMP-TABLE tt-reservas.
    EMPTY TEMP-TABLE tt-reservas-final.    
END PROCEDURE.

/**
 * Realiza a configuraá∆o da API gravando em:
 * tt-param-reporte
 ****************************************************************/
PROCEDURE pi-configura-api:
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
           tt-param-reporte.log-result-param = YES.
    CREATE tt-param-reporte.
    ASSIGN tt-param-reporte.cod-param = "l-msg-termina-ordem" 
           tt-param-reporte.log-result-param = YES.
    CREATE tt-param-reporte.
    ASSIGN tt-param-reporte.cod-param = "l-msg-termina-com-operac-incompl"
           tt-param-reporte.log-result-param = NO.
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
                                                                  
/**
 * Finaliza a API e limpa o handle
 ****************************************************************/
PROCEDURE pi-finaliza-api:
    RUN setaRowSplitCtrab IN h-sfapi009 (INPUT ?, INPUT ?) NO-ERROR.
    RUN finalizaAPI IN h-sfapi009.
    DELETE PROCEDURE h-sfapi009.
    ASSIGN h-sfapi009 = ?.    
END PROCEDURE.

/**
 * Deixa a hora no formato que a SFAPI009 "entende".
 ****************************************************************/
FUNCTION formata-hora RETURNS CHAR (hora AS INTEGER):
    RETURN REPLACE(STRING(hora, "HH:MM"), ":", "").
END FUNCTION.

/**
 * Procedimento para iniciar o setup de um split
 ****************************************************************/
PROCEDURE inicia-setup-split:
    DEF INPUT PARAM cod-ctrab AS CHAR NO-UNDO.
    DEF INPUT PARAM cod-operador AS CHAR NO-UNDO.
    DEF INPUT PARAM split-rowid AS ROWID NO-UNDO.
    DEF OUTPUT PARAM TABLE FOR RowErrors.

    DEF VAR valor-retorno AS CHAR NO-UNDO INIT 'Erro inesperado. N∆o localizou o split.'.

    IF NOT VALID-HANDLE(h-sfapi009) THEN DO:
        RUN pi-inicializa-api.
    END.

    FIND FIRST split-operac NO-LOCK
        WHERE ROWID(split-operac) = split-rowid NO-ERROR.
    IF AVAIL split-operac THEN DO:
        RUN iniciaReporte IN  h-sfapi009
            (split-rowid,
             TODAY, 
             formata-hora(TIME),
             ?, /* dt fim setup */
             0, /* hr fim setup */
             cod-ctrab,
             '', /* ferramenta */
             '', /* equipe */
             cod-operador, 
             ?, /* dt inicio reporte */
             0, /* hr inicio reporte */
             OUTPUT TABLE RowErrors).
        ASSIGN valor-retorno = RETURN-VALUE.
    END.
    
    RUN pi-finaliza-api.
    RETURN valor-retorno.
END PROCEDURE.

/**
 * Procedimento para preencher o termino do setup do split
 ****************************************************************/
PROCEDURE finaliza-setup-split:
    DEF INPUT PARAM cod-ctrab AS CHAR NO-UNDO.
    DEF INPUT PARAM cod-operador AS CHAR NO-UNDO.
    DEF INPUT PARAM split-rowid AS ROWID NO-UNDO.
    DEF OUTPUT PARAM TABLE FOR RowErrors.

    DEF VAR valor-retorno AS CHAR NO-UNDO INIT 'Erro inesperado. N∆o localizou o split.'.
    DEF VAR c-hora-ini AS CHAR NO-UNDO.
    DEF VAR c-hora-fim AS CHAR NO-UNDO.

    IF NOT VALID-HANDLE(h-sfapi009) THEN DO:
        RUN pi-inicializa-api.
    END.

    FIND FIRST split-operac NO-LOCK
        WHERE ROWID(split-operac) = split-rowid NO-ERROR.
    IF AVAIL split-operac THEN DO:

        ASSIGN c-hora-ini = formata-hora(INT(split-operac.qtd-segs-inic-setup))
               c-hora-fim = formata-hora(TIME).

        RUN iniciaReporte IN  h-sfapi009
            (split-rowid,
             split-operac.dat-inic-setup, c-hora-ini,
             TODAY, c-hora-fim,  /* fim setup */
             cod-ctrab,
             '', /* ferramenta */
             '', /* equipe */
             cod-operador, 
             TODAY, c-hora-fim, /* inicio reporte */ 
             OUTPUT TABLE RowErrors).
        ASSIGN valor-retorno = RETURN-VALUE.
    END.
    
    RUN pi-finaliza-api.
    RETURN valor-retorno.
END PROCEDURE.

/**
 * Procedimento que realiza o apontamento no ch∆o de f†brica
 ****************************************************************/
PROCEDURE aponta-split:
    DEF INPUT PARAM cod-operador LIKE operador.cod-operador NO-UNDO.
    DEF INPUT PARAM cod-ctrab    LIKE ctrab.cod-ctrab NO-UNDO.
    DEF INPUT PARAM split-rowid AS ROWID NO-UNDO.
    DEF INPUT PARAM qt-aprovada AS DECIMAL NO-UNDO INIT 0.0.
    DEF INPUT PARAM qt-refugada AS DECIMAL NO-UNDO INIT 0.0.
    DEF INPUT PARAM cod-motiv-refugo LIKE motiv-refugo.cod-motiv-refugo NO-UNDO.
    DEF OUTPUT PARAM TABLE FOR tt-erro.

    DEF VAR qt-saldo-split AS DECIMAL NO-UNDO.

    IF NOT VALID-HANDLE(h-sfapi009) THEN DO:
        RUN pi-inicializa-api.
    END.
    RUN configura-apontamento.

    FIND FIRST ctrab
         WHERE ctrab.cod-ctrab = cod-ctrab NO-LOCK NO-ERROR.
    IF NOT AVAIL ctrab THEN 
        RETURN "NOK". 
    
    FIND FIRST split-operac NO-LOCK 
        WHERE ROWID(split-operac) = split-rowid NO-ERROR.
    IF NOT AVAIL split-operac THEN 
        RETURN "NOK". 

    /* se apontamento for total, finaliza ordem */
    ASSIGN qt-saldo-split = split-operac.qtd-previs-operac - (split-operac.qtd-operac-aprov + split-operac.qtd-operac-refgda).
    IF qt-saldo-split <= (qt-aprovada + qt-refugada) THEN DO:
        FOR tt-param-reporte EXCLUSIVE-LOCK
                WHERE tt-param-reporte.cod-param = "l-msg-termina-ordem" :
            ASSIGN tt-param-reporte.log-result-param = YES.
        END.
    END.

    RUN setaRowSplitCtrab IN h-sfapi009 (INPUT split-rowid,
                                         INPUT ROWID(ctrab)).

    IF RETURN-VALUE = "NOK" THEN DO:
        RUN retornaTTErro IN h-sfapi009 (OUTPUT TABLE tt-erro).
    END. ELSE DO:        
        
        RUN recebeValorCampos IN h-sfapi009 ("operador", REPLACE(cod-operador, "-", ""), ?, ?, ?).
        RUN recebeValorCampos IN h-sfapi009 ("qtd-aprov",  "", ?, qt-aprovada, ?).
        RUN recebeValorCampos IN h-sfapi009 ("qtd-refgda", "", ?, qt-refugada, ?).

        RUN recebeValorCampos IN h-sfapi009 ("hra-fim-rep", formata-hora(TIME), ?, ?, ?).

        IF qt-refugada > 0.0 THEN DO:
            RUN pi-refuga-peca(split-operac.nr-ord-prod,
                               split-operac.op-codigo,
                               qt-refugada,
                               cod-motiv-refugo,
                               split-rowid).            
        END.

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
        /** 
         * verifica se ficou alguma reserva aberta, que por algum motivo 
         *  o sistema ignorou na verificaá∆o automatica.
         ************************************************************************/
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
                    ASSIGN tt-erro.mensagem = "Erro desconhecido, Informar o TI. Enviar TODOS OS DETALHES do apontamento.".
                END.
            END. ELSE DO:
                /* apontou corretamente.
                 *  Executa procedimento para fechar a ordem.
                 *  Aparentemente n∆o fecha sozinho!!
                 */
                IF qt-saldo-split <= (qt-aprovada + qt-refugada) THEN DO:
                    RUN terminaOrdemOperac IN h-sfapi009.
                    IF RETURN-VALUE = "NOK" THEN DO:
                        RUN retornaTTErro IN h-sfapi009 (OUTPUT TABLE tt-erro).
                        FIND FIRST tt-erro NO-LOCK NO-ERROR.
                        IF NOT AVAIL tt-erro THEN DO:
                            CREATE tt-erro.
                            ASSIGN tt-erro.mensagem = "Erro desconhecido ao fechar ordem, Informar o TI. Enviar TODOS OS DETALHES do apontamento.".
                        END.
                    END.
                END.
            END.
        END.
    END.

    RUN pi-finaliza-api.
END PROCEDURE.

/**
 * Procedimento (interno) para refugar peáas
 ****************************************************************/
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
     * Se existir baixa de reserva na operaáÖo
     * precisa requisitar esses itens?
     ******************************************/

    RETURN "OK".       
END PROCEDURE.

/**
 * Procedimento (interno) para informar o lote e 
 * dependendo do caso altera o deposito para padr∆o do item 
 ****************************************************************/
PROCEDURE pi-ajusta-lote-reporte:
    DEF INPUT PARAM it-codigo LIKE ITEM.it-codigo NO-UNDO.

    DEF VAR lote AS CHAR NO-UNDO.
    DEF VAR c-dep-acab AS CHAR NO-UNDO.

    FIND FIRST ITEM NO-LOCK 
        WHERE ITEM.it-codigo = it-codigo NO-ERROR.
    IF NOT AVAIL ITEM THEN LEAVE.
    
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

    IF item.tipo-con-est = 3 THEN DO: /* controlado por lote, ent∆o cria um lote */ 
        RUN upc/p-ccs-lote.p (OUTPUT lote).
        RUN recebeValorCampos IN h-sfapi009 ("lote-acab", lote, ?, ?, ?).
        RUN recebeValorCampos IN h-sfapi009 ("dt-valid-lote", "", ?, ?, 12/31/3999).
    END.

END PROCEDURE.

/**
 * Procedimento que realiza a divis∆o do split em dois ...
 * de acordo com a quantidade informada
 ****************************************************************/
PROCEDURE divide-split:
    DEF INPUT PARAM r-split     AS ROWID NO-UNDO.
    DEF INPUT PARAM qt-apontado AS DECIMAL NO-UNDO.

    DEF VAR c-retorno AS CHAR NO-UNDO.
    DEF VAR qt-saldo-split AS DECIMAL NO-UNDO.
    DEF VAR h-boin535a AS HANDLE NO-UNDO.
    

    FIND FIRST split-operac EXCLUSIVE
        WHERE ROWID(split-operac) = r-split NO-ERROR. /* n∆o pode ter no-lock ! */
    IF NOT AVAIL split-operac THEN DO:
        RELEASE split-operac NO-ERROR.
        RETURN "NOK".
    END.

    ASSIGN qt-saldo-split = split-operac.qtd-previs-operac - (split-operac.qtd-operac-aprov + split-operac.qtd-operac-refgda).

    IF qt-saldo-split < qt-apontado THEN DO:
        RELEASE split-operac.
        RETURN "NOK".
    END.
        
    IF qt-saldo-split - qt-apontado = 0 THEN DO:
        RELEASE split-operac.
        RETURN "NOK".
    END.

    RUN inbo/boin535a.p PERSISTENT SET h-boin535a.
    RUN DividirSplitQtd IN h-boin535a (BUFFER split-operac,
    								   INPUT qt-apontado,
    								   INPUT (qt-saldo-split - qt-apontado),
    								   OUTPUT TABLE RowErrors).
    ASSIGN c-retorno = RETURN-VALUE.

    RELEASE split-operac.

    DELETE PROCEDURE h-boin535a.
    RETURN c-retorno.
END PROCEDURE.

/**
 * Procedimento que realiza a uni∆o do split previamente separados.
 ****************************************************************/
PROCEDURE unir-split:
    DEF INPUT PARAM nr-ord-prod LIKE ord-prod.nr-ord-prod NO-UNDO.
    DEF INPUT PARAM op-codigo   LIKE oper-ord.op-codigo NO-UNDO.

    DEF VAR h-boin535a AS HANDLE NO-UNDO.
    DEF VAR c AS INTEGER NO-UNDO.
    
    DEF VAR r-split1 AS ROWID NO-UNDO.
    DEF VAR r-split2 AS ROWID NO-UNDO.
    
    ASSIGN c = 0.
    FOR EACH split-operac NO-LOCK 
            WHERE split-operac.nr-ord-prod = nr-ord-prod
              AND split-operac.op-codigo = op-codigo
              AND split-operac.ind-estado-split < 5:
        ASSIGN c = c + 1.
        IF r-split1 = ? THEN
            ASSIGN r-split1 = ROWID(split-operac).
        ELSE
            ASSIGN r-split2 = ROWID(split-operac).
    END.

    IF c < 2 THEN 
        RETURN "OK".

    RUN inbo/boin535a.p PERSISTENT SET h-boin535a.
    RUN UnirSplit IN h-boin535a
            (INPUT r-split2,
             INPUT r-split1, /* essa que vai ficar */
             OUTPUT TABLE RowErrors).

    DELETE PROCEDURE h-boin535a.
    RETURN RETURN-VALUE.
END PROCEDURE.


/**
 * Procedimento para fechar o split;
 * ê usado principalmente quando Ç feito a divis∆o do split.
 ****************************************************************/
PROCEDURE fechar-split:
    DEF INPUT PARAM r-split AS ROWID NO-UNDO.

    DEF VAR h-boin536 AS HANDLE NO-UNDO.

    FIND FIRST split-operac NO-LOCK 
        WHERE ROWID(split-operac) = r-split NO-ERROR.
    IF split-operac.ind-estado-split >= 5 THEN 
        RETURN 'OK'.

    RUN inbo/boin536.r PERSISTENT SET h-boin536.
    RUN FecharSplit IN h-boin536 (BUFFER split-operac).

    RUN openQuery IN h-boin536 (1).
    RUN findFirst IN h-boin536.
    DELETE PROCEDURE h-boin536.
    ASSIGN h-boin536 = ?.
END PROCEDURE.

/**
 * Procedimento (interno) para processar as reservas da ordem
 ****************************************************************/
PROCEDURE pi-processa-reservas:
    DEF INPUT PARAM cod-ctrab LIKE ctrab.cod-ctrab NO-UNDO.
    DEF INPUT PARAM gm-codigo LIKE grup-maquin.gm-codigo NO-UNDO.
    DEF INPUT PARAM it-codigo LIKE ITEM.it-codigo NO-UNDO.

    DEF BUFFER b-ord-prod FOR ord-prod.

    DEF VAR cod-depos    AS CHAR NO-UNDO.
    DEF VAR quant-requis AS DECIMAL NO-UNDO.
    DEF VAR qt-req-aux   AS DECIMAL NO-UNDO.
    
    DEF VAR sequencia AS INTEGER NO-UNDO INIT 1.

    IF NOT VALID-HANDLE(h-sfapi009) THEN DO:
        RETURN 'NOK'.
    END.

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
                ASSIGN tt-erro.mensagem = "Deposito padr∆o da operaá∆o n∆o foi localizado.~nContatar o PCP".
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
             * as vezes o saldo que ir† sobrar Ç menor que uma chapa.
             * bem menor na verdade (gramas ou atÇ menos).
             * ver uma forma de requisitar o saldo todo aqui nesses casos.
             *
             * tipo: if qtidade-atu - quant-requis > 0.1 then 
             *               quant-requis += ? 
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
                   tt-reservas-final.sequencia  =  sequencia
                   .
            ASSIGN sequencia = sequencia + 1.
        END.
        IF quant-requis <= 0 THEN 
            DELETE tt-reservas.
    END.
    /* no que sobrou ... */
    FOR EACH tt-reservas SHARE-LOCK:
        ASSIGN tt-reservas.cod-depos = cod-depos.
    END.
END PROCEDURE.

/* verificar funá∆o abaixo se pode alter†-la para usar oper-ord
 * e colocar dentro de ccs/cf/funcoes-operacoes.i ****************/
FUNCTION saldo-operacao RETURNS DECIMAL (nr-ord-prod AS INTEGER,
                                         op-codigo   AS INTEGER):
    DEF VAR qt-saldo AS DECIMAL NO-UNDO.
    FOR EACH b-split-operac NO-LOCK
        WHERE b-split-operac.nr-ord-prod = nr-ord-prod
          AND b-split-operac.op-codigo = op-codigo
          AND b-split-operac.ind-estado-split < 5 :
        ASSIGN qt-saldo  = qt-saldo + (b-split-operac.qtd-previs-operac - 
                                       (b-split-operac.qtd-operac-aprov + b-split-operac.qtd-operac-refgda)).
    END.
    RETURN qt-saldo.
END FUNCTION.

/**
 * Procedimento (interno) para verificar as reservas da ordem
 * que n∆o apareceram para serem apontadas pela funá∆o padr∆o do
 * sistema.
 * Verificado apenas quando o apontamento Ç 100% ou 
 * Ç a ultima operaá∆o.
 ******************************************************************/
PROCEDURE pi-verifica-reservas-sem-apontamento:
    DEF INPUT PARAM nr-ord-prod LIKE ord-prod.nr-ord-prod NO-UNDO.
    DEF INPUT PARAM op-codigo   LIKE oper-ord.op-codigo   NO-UNDO.
    DEF INPUT PARAM qt-apontado AS DECIMAL NO-UNDO.

    DEF VAR qtd-requis   AS DECIMAL NO-UNDO.
    DEF VAR qtd-variacao AS DECIMAL NO-UNDO.
    DEF VAR sld-operacao AS DECIMAL NO-UNDO.

    ASSIGN sld-operacao = saldo-operacao(nr-ord-prod, op-codigo).

    IF sld-operacao <> qt-apontado OR 
       NOT ultima-operacao(nr-ord-prod, op-codigo) THEN LEAVE.

    FOR EACH b-reservas NO-LOCK 
        WHERE b-reservas.nr-ord-prod = nr-ord-prod
          AND b-reservas.estado = 1:

        FIND FIRST tt-reservas NO-LOCK
            WHERE tt-reservas.it-codigo = b-reservas.it-codigo NO-ERROR.
        IF AVAIL tt-reservas THEN 
            NEXT. /* j† vai aparecer erro de saldo do item ... ignora esse */

        FIND FIRST tt-reservas-final NO-LOCK
            WHERE tt-reservas-final.it-codigo = b-reservas.it-codigo NO-ERROR.
        IF AVAIL tt-reservas-final THEN 
            NEXT. /* vai baixar pois tem saldo (s¢ que ainda n∆o fez?) ... ignora */

        /* TOTEST: validar se fica alocado mesmo !!! */
        ASSIGN qtd-variacao = b-reservas.quant-orig
               qtd-requis = b-reservas.quant-atend + b-reservas.quant-aloc. 

        FOR FIRST item-manuf-estab NO-LOCK 
                WHERE item-manuf-estab.it-codigo = b-reservas.it-codigo :
            ASSIGN qtd-variacao = qtd-variacao - qtd-variacao * item-manuf-estab.var-apto-menor / 100.
        END.

        /* verifica se Ç permitida a variaá∆o do que foi requisitado caso houver */
        IF qtd-variacao > qtd-requis THEN DO:
            CREATE tt-erro.
            ASSIGN tt-erro.mensagem = "Item: " + b-reservas.it-codigo + " n∆o foi requisitado completamente." +
                                      "~nQuantidade : " + STRING(b-reservas.quant-orig) + 
                                      "~nRequisitado: " + STRING(qtd-requis) + 
                                      "~nContatar o PCP.".
        END.
    END.
END PROCEDURE.

