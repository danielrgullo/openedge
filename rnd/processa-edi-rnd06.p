/**
    pi-processa-EDI
    	traduz codigo do cliente
    	gera temp-tables:
    		tt-edi e tt-ped-edi

*/
/* temp-table  */
{c:/work/desenv/edi/tt-ediccs.i}
/***************/
{c:/work/desenv/rnd/rnd06.p}
/***************/
/*DEFINE NEW GLOBAL SHARED VAR h-acomp-edi AS HANDLE NO-UNDO.*/
/***************/


PROCEDURE get-temp-tables:
    DEF OUTPUT PARAM TABLE FOR tt-edi.
    DEF OUTPUT PARAM TABLE FOR tt-ped-edi.
    DEF OUTPUT PARAM TABLE FOR tt-ped-edi-ent.
END PROCEDURE.

PROCEDURE pi-limpa-temp-tables:
    EMPTY TEMP-TABLE tt-edi.
    EMPTY TEMP-TABLE tt-ped-edi.
    EMPTY TEMP-TABLE tt-ped-edi-ent.
END.

PROCEDURE pi-processa-EDI:
    
    DEF VAR dt-aux       AS DATE NO-UNDO.    
    DEF VAR codigo-refer AS CHAR NO-UNDO.
    DEF VAR nr-pedcli    AS CHAR NO-UNDO.

    DEF VAR h-formata-codigo          AS HANDLE  NO-UNDO.
    DEF VAR c-programaFormataCodigo AS CHAR NO-UNDO INIT "edi/edicodcli.p".

    DEF VAR l-acomp   AS LOGICAL NO-UNDO INIT FALSE.
    ASSIGN  l-acomp = VALID-HANDLE(h-acomp-edi).

    IF l-acomp THEN DO:
        RUN pi-acompanhar IN h-acomp-edi (INPUT  "Aguarde processando arquivo lido ...")  NO-ERROR.
    END.

    RUN pi-limpa-temp-tables.

    DO 
        ON STOP UNDO, RETURN "NOK" :

        FIND FIRST tt-itp NO-LOCK NO-ERROR.
        IF NOT AVAIL tt-itp THEN 
            STOP.
    
        FIND FIRST emitente NO-LOCK WHERE emitente.cgc = tt-itp.id-transmis NO-ERROR.
        IF NOT AVAIL emitente THEN 
            STOP.

        FIND FIRST ccs-edi-conf 
            WHERE ccs-edi-conf.cod-emitente = emitente.cod-emitente NO-ERROR.
        IF NOT AVAIL ccs-edi-conf THEN 
            STOP.
                    
        IF ccs-edi-conf.proc-it-codigo <> "" THEN DO:
            ASSIGN c-programaFormataCodigo = ccs-edi-conf.proc-it-codigo.
        END.
        RUN VALUE(c-programaFormataCodigo) PERSISTENT SET h-formata-codigo.
    
        ASSIGN dt-aux = fi-conv-date(SUBSTR(tt-itp.id-movimento, 1, 6)).
        CREATE tt-edi.
        ASSIGN tt-edi.cod-emitente = emitente.cod-emitente
               tt-edi.id-movimento = tt-itp.id-movimento
               tt-edi.dt-movto     = dt-aux
               tt-edi.hr-movto     = INT(SUBSTR(tt-itp.id-movimento,7,6))
               tt-edi.dt-trans     = TODAY
               tt-edi.hr-trans     = TIME.
        FIND FIRST tt-edi NO-LOCK NO-ERROR.

        FOR EACH tt-pp1 NO-LOCK:

            IF l-acomp THEN DO:
                RUN pi-acompanhar IN h-acomp-edi (INPUT "Processando pedido : " + tt-pp1.numero-pedido) NO-ERROR.
            END.


            FOR EACH tt-pp2 NO-LOCK WHERE tt-pp2.r-rowid = ROWID(tt-pp1):


                RUN pi-traduz IN h-formata-codigo ( INPUT TRIM(tt-pp2.codigo-refer),
                                                    OUTPUT codigo-refer ).

                ASSIGN nr-pedcli = REPLACE(TRIM(tt-pp1.numero-pedido), "  ", " ").


                FIND FIRST tt-ped-edi WHERE tt-ped-edi.cod-emitente = tt-edi.cod-emitente
                                        AND tt-ped-edi.id-movimento = tt-edi.id-movimento
                                        AND tt-ped-edi.id-programa  = tt-pp1.id-versao-pedido
                                        AND tt-ped-edi.codigo-refer = codigo-refer
                                        AND tt-ped-edi.nr-pedcli    = nr-pedcli NO-ERROR.
                IF NOT AVAIL tt-ped-edi THEN DO:
    
                    CREATE tt-ped-edi.
                    ASSIGN tt-ped-edi.cod-emitente = tt-edi.cod-emitente
                           tt-ped-edi.id-movimento = tt-edi.id-movimento
                           tt-ped-edi.id-programa  = tt-pp1.id-versao-pedido
                           tt-ped-edi.dt-programa  = tt-pp1.data-emissao
                           tt-ped-edi.codigo-refer = codigo-refer
                           tt-ped-edi.nr-pedcli    = nr-pedcli
                           /*tt-ped-edi.tipo-fornec  = */
                           tt-ped-edi.alt-tec      = ""
                           .
                    ASSIGN tt-ped-edi.it-codigo = fi-get-it-codigo (tt-ped-edi.cod-emitente,
                                                                    tt-ped-edi.nr-pedcli,
                                                                    tt-ped-edi.codigo-refer,
                                                                    tt-ped-edi.alt-tec).
                END.

                CREATE tt-ped-edi-ent.
                ASSIGN tt-ped-edi-ent.cod-emitente = tt-edi.cod-emitente
                       tt-ped-edi-ent.id-movimento = tt-edi.id-movimento
                       tt-ped-edi-ent.id-programa  = tt-ped-edi.id-programa
                       tt-ped-edi-ent.nr-pedcli    = tt-ped-edi.nr-pedcli
                       tt-ped-edi-ent.codigo-refer = tt-ped-edi.codigo-refer
                       tt-ped-edi-ent.alt-tec      = tt-ped-edi.alt-tec
                       tt-ped-edi-ent.it-codigo    = tt-ped-edi.it-codigo
                       tt-ped-edi-ent.dt-entrega   = tt-pp2.data-entrega
                       tt-ped-edi-ent.qt-entrega   = tt-pp2.qtd-entrega
                       tt-ped-edi-ent.numero-item  = tt-pp2.numero-item
                       tt-ped-edi-ent.situacao     = 1 /* ? */.

                FIND FIRST tt-te1 WHERE tt-te1.r-rowid = ROWID(tt-pp2) NO-LOCK NO-ERROR.
                IF AVAIL tt-te1 THEN
                    ASSIGN tt-ped-edi-ent.narrativa = TRIM(tt-te1.txt-informa).

            END.
            FIND FIRST tt-te1 WHERE tt-te1.r-rowid = ROWID(tt-pp1) NO-LOCK NO-ERROR.
            IF AVAIL tt-te1 THEN
                ASSIGN tt-ped-edi.narrativa = TRIM(tt-te1.txt-informa).

        END. /* /for each tt-pp1 */
    END. /* /do on stop .. */
END PROCEDURE.

