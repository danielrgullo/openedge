{c:/work/desenv/edi/tt-ediccs.i}
{c:/work/desenv/edi/edi-ccs-util.p}

DEF INPUT PARAM TABLE FOR tt-edi.
DEF OUTPUT PARAM TABLE FOR RowErros.

DEF VAR h-acomp AS HANDLE NO-UNDO.

DEF VAR h-boedi AS HANDLE NO-UNDO.
DEF VAR c-obs   AS CHAR NO-UNDO.
DEF VAR d-preco AS DECIMAL NO-UNDO.
DEF VAR nr-tabpre AS CHAR NO-UNDO.

DO 
    ON STOP UNDO, LEAVE:

    FIND FIRST tt-edi NO-LOCK NO-ERROR.
    IF NOT AVAIL tt-edi THEN DO:
        RETURN "Erro inesperado !!! Vocà leu o arquivo sem erros ?".
    END.
    
/*     FIND FIRST emitente OF tt-edi NO-LOCK NO-ERROR. */
    FIND FIRST emitente NO-LOCK
         WHERE emitente.cod-emitente = tt-edi.cod-emitente NO-ERROR.
    IF NOT AVAIL emitente THEN DO:
        RETURN "Erro inesperado !!! Cliente n∆o encontrado !!!".
    END.

    FIND FIRST ccs-edi-conf OF emitente NO-LOCK NO-ERROR.
    IF NOT AVAIL ccs-edi-conf THEN DO:
        RETURN "Erro inesperado!!! N∆o localizou configuraá∆o do cliente !!!".
    END.


    RUN edi/boedipv01.p PERSISTENT SET h-boedi.
    IF NOT VALID-HANDLE(h-boedi) THEN DO:
        RETURN "Erro inesperado !!! N∆o consegue processar pedidos.".
    END.

    RUN utp/ut-acomp.p PERSISTENT SET h-acomp.
    RUN pi-inicializar in h-acomp (input "Processando ..."). 
  
    FOR EACH tt-edi-ccs 
            WHERE tt-edi-ccs.l-processado = FALSE 
              AND tt-edi-ccs.cod-emitente = tt-edi.cod-emitente
              AND tt-edi-ccs.dt-processo = tt-edi.dt-movto
              AND tt-edi-ccs.dt-entrega <> ? 
              AND tt-edi-ccs.it-codigo <> "" 
              AND tt-edi-ccs.c-tipo <> "E"
              AND tt-edi-ccs.c-tipo <> "C" /* 2012-09-25: separar os cancelados. */
        BY tt-edi-ccs.nr-pedcli 
        BY tt-edi-ccs.dt-entrega:

        // criar log disso abaixo?
        IF ccs-edi-conf.i-horizonte <> 0 AND tt-edi-ccs.dt-entrega > (TODAY + ccs-edi-conf.i-horizonte) THEN 
            NEXT.
        IF TRIM(tt-edi-ccs.c-obs) <> "" OR TRIM(tt-edi-ccs.c-tipo) = "" THEN 
            NEXT.
        // ?

        RUN pi-acompanhar IN h-acomp (INPUT tt-edi-ccs.c-tipo + " " + 
                                            tt-edi-ccs.nr-pedcli).

        EMPTY TEMP-TABLE RowErros.

        RUN pi-procura-emitente (INPUT tt-edi-ccs.cod-emitente, 
                                 INPUT tt-edi-ccs.nr-pedcli).
     
        IF tt-edi-ccs.c-tipo = "A" THEN DO:

            RUN alteraPedidoItemEntrega IN h-boedi (
                    INPUT  emitente.nome-abrev ,
                    INPUT  tt-edi-ccs.nr-pedcli   ,
                    INPUT  tt-edi-ccs.nr-sequencia,
                    INPUT  tt-edi-ccs.nr-entrega  ,
                    INPUT  tt-edi-ccs.it-codigo   ,
                    INPUT  tt-edi-ccs.dt-entrega  ,
                    INPUT  tt-edi-ccs.qt-pedida   ,
                    OUTPUT TABLE RowErros
                    ).

        END. ELSE IF tt-edi-ccs.c-tipo = "N" THEN DO:

            FIND FIRST tt-ped-obs NO-LOCK 
                WHERE tt-ped-obs.nr-pedcli = tt-edi-ccs.nr-pedcli NO-ERROR.
            IF AVAIL tt-ped-obs THEN
                ASSIGN c-obs = tt-ped-obs.narrativa.
            ELSE
                ASSIGN c-obs = "".

            IF NOT CAN-FIND(ped-venda WHERE ped-venda.nome-abrev = emitente.nome-abrev 
                                        AND ped-venda.nr-pedcli = tt-edi-ccs.nr-pedcli) THEN DO:

                // temporario enquanto recebe o pedido nos dois estabelecimentos 
                ASSIGN nr-tabpre = emitente.nr-tabpre.
                IF tt-edi-ccs.cod-estabel = "200" AND (emitente.cod-emitente = 10000124 OR 
                                                       emitente.cod-emitente = 10006602) THEN
                    ASSIGN nr-tabpre = "20006602".

                // pedido 
                RUN adicionaPedido IN h-boedi (
                            INPUT tt-edi-ccs.cod-estabel,
                            INPUT emitente.nome-abrev ,
                            INPUT tt-edi-ccs.nr-pedcli ,
                            INPUT nr-tabpre,
                            INPUT ccs-edi-conf.esp-ped ,
                            OUTPUT TABLE RowErros
                            ).                
            END.

            IF NOT CAN-FIND(ped-item WHERE ped-item.nome-abrev = emitente.nome-abrev 
                                       AND ped-item.nr-pedcli  = tt-edi-ccs.nr-pedcli
                                       AND ped-item.it-codigo  = tt-edi-ccs.it-codigo
                                       AND ped-item.cod-sit-item <> 6) THEN DO:
                // item 
                FIND FIRST preco-item NO-LOCK 
                    WHERE preco-item.it-codigo = tt-edi-ccs.it-codigo 
                      AND preco-item.situacao = 1 NO-ERROR.
                IF NOT AVAIL preco-item THEN
                    ASSIGN d-preco = 0.
                ELSE
                    ASSIGN d-preco = preco-item.preco-venda.

                RUN adicionaPedidoItem IN h-boedi (
                            INPUT tt-edi-ccs.cod-estabel,
                            INPUT emitente.nome-abrev ,
                            INPUT tt-edi-ccs.nr-pedcli ,
                            INPUT tt-edi-ccs.it-codigo ,
                            INPUT tt-edi-ccs.dt-entrega ,
                            INPUT tt-edi-ccs.qt-pedida ,
                            INPUT d-preco,
                            INPUT c-obs ,
                            OUTPUT TABLE RowErros 
                            ).
            END. ELSE DO:

                // entrega 
                FIND LAST ped-item 
                    WHERE ped-item.nome-abrev = emitente.nome-abrev 
                      AND ped-item.nr-pedcli  = tt-edi-ccs.nr-pedcli
                      AND ped-item.it-codigo  = tt-edi-ccs.it-codigo
                      AND ped-item.cod-sit-item <> 6 NO-ERROR.

                FIND FIRST preco-item NO-LOCK 
                    WHERE preco-item.it-codigo = tt-edi-ccs.it-codigo 
                      AND preco-item.situacao = 1 NO-ERROR.
                IF NOT AVAIL preco-item THEN
                    ASSIGN d-preco = ped-item.vl-preori.
                ELSE
                    ASSIGN d-preco = preco-item.preco-venda.

                RUN adicionaPedidoItemEntrega IN h-boedi (                            
                            INPUT  emitente.nome-abrev ,
                            INPUT  tt-edi-ccs.nr-pedcli ,
                            INPUT  ped-item.nr-sequencia ,
                            INPUT  tt-edi-ccs.it-codigo ,
                            INPUT  tt-edi-ccs.dt-entrega ,
                            INPUT  tt-edi-ccs.qt-pedida ,
                            INPUT  d-preco,
                            INPUT  c-obs ,
                            OUTPUT TABLE RowErros
                            ).
            END.
        END.

        FIND FIRST RowErros NO-LOCK
             WHERE RowErros.ErrorSubType <> 'WARNING' NO-ERROR.
        IF NOT AVAIL RowErros THEN DO:
            ASSIGN tt-edi-ccs.l-processado = TRUE.
        END. ELSE DO:

            /* TODO : melhorar isso ! 
                      colocar em temp-table e mostrar problemas no final */
            RUN  pi-log-erros.

        END.
    END.

    /* 2012-09-25: fazer agora s¢ os que foram cancelados : */
    FOR EACH tt-edi-ccs 
            WHERE tt-edi-ccs.l-processado = FALSE 
              AND tt-edi-ccs.cod-emitente = tt-edi.cod-emitente
              AND tt-edi-ccs.dt-processo = tt-edi.dt-movto
              AND tt-edi-ccs.dt-entrega <> ? 
              AND tt-edi-ccs.it-codigo <> "" 
              AND tt-edi-ccs.c-tipo = "C":

        RUN pi-acompanhar IN h-acomp (INPUT tt-edi-ccs.c-tipo + " " + 
                                            tt-edi-ccs.nr-pedcli).
        
        EMPTY TEMP-TABLE RowErros.
        
        RUN pi-procura-emitente (INPUT tt-edi-ccs.cod-emitente, 
                                 INPUT tt-edi-ccs.nr-pedcli).

        RUN cancelaPedidoItemEntrega IN h-boedi (
                            INPUT  emitente.nome-abrev ,
                            INPUT  tt-edi-ccs.nr-pedcli   ,
                            INPUT  tt-edi-ccs.nr-sequencia,
                            INPUT  tt-edi-ccs.nr-entrega  ,
                            INPUT  tt-edi-ccs.it-codigo   ,
                            INPUT  tt-edi-ccs.dt-entrega  ,
                            INPUT  tt-edi-ccs.dt-processo ,
                            OUTPUT TABLE RowErros
                            ).

        FIND FIRST RowErros NO-LOCK
             WHERE RowErros.ErrorSubType <> 'WARNING' NO-ERROR.
        IF NOT AVAIL RowErros THEN DO:
           ASSIGN tt-edi-ccs.l-processado = TRUE.
        END. ELSE DO:
            /* TODO : melhorar isso ! 
                      colocar em temp-table e mostrar problemas no final */
            RUN  pi-log-erros.
        END.

    END.

    /* finaliza bo                 */
    IF VALID-HANDLE(h-boedi) THEN DO:
        DELETE PROCEDURE h-boedi.
        ASSIGN h-boedi = ?.
    END.
    
    RUN pi-finalizar IN h-acomp.

    RETURN 'OK':u.    

END. /* DO ON STOP UNDO, LEAVE: */
    
RETURN '':u. // ?
