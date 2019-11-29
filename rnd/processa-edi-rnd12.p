/**
    pi-processa-EDI
    	traduz codigo do cliente
    	gera temp-tables:
    		tt-edi e tt-ped-edi

*/
/* temp-table  */
{edi/tt-ediccs.i}
/***************/
{rnd/rnd12.p}
/***************/
/*DEFINE NEW GLOBAL SHARED VAR h-acomp-edi AS HANDLE NO-UNDO.*/
/***************/

FUNCTION fi-menos-diasuteis RETURNS DATE (DATE, INTEGER) FORWARD.

DEF VAR h-formata-codigo          AS HANDLE  NO-UNDO.
DEF VAR c-programaFormataCodigo AS CHAR NO-UNDO INIT "edi/edicodcli.p".


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
    DEF VAR i-aux        AS INTEGER NO-UNDO.
    DEF VAR codigo-refer AS CHAR NO-UNDO.
    DEF VAR nr-pedcli    AS CHAR NO-UNDO.

    RUN pi-limpa-temp-tables.

    main_loop:
    DO 
        ON ERROR UNDO, RETURN "NOK"
        ON STOP  UNDO, RETURN "NOK":

        FIND FIRST tt-itp NO-LOCK NO-ERROR.
        IF NOT AVAIL tt-itp THEN STOP.
    
        FIND FIRST emitente NO-LOCK 
            WHERE emitente.cgc = tt-itp.id-transmis NO-ERROR.
        IF NOT AVAIL emitente THEN STOP.

        FIND FIRST ccs-edi-conf 
            WHERE ccs-edi-conf.cod-emitente = emitente.cod-emitente NO-ERROR.
        IF NOT AVAIL ccs-edi-conf THEN STOP.

        /*IF ccs-edi-conf.proc-it-codigo <> "" THEN DO:
            ASSIGN c-programaFormataCodigo = ccs-edi-conf.proc-it-codigo.
        END.
        RUN VALUE(c-programaFormataCodigo) PERSISTENT SET h-formata-codigo.
        */
    
        ASSIGN dt-aux = fi-conv-date(SUBSTR(tt-itp.id-movimento, 1, 6)).
        CREATE tt-edi.
        ASSIGN tt-edi.cod-emitente = emitente.cod-emitente
               tt-edi.id-movimento = tt-itp.id-movimento
               tt-edi.dt-movto     = dt-aux
               tt-edi.hr-movto     = INT(SUBSTR(tt-itp.id-movimento,7,6))
               tt-edi.dt-trans     = TODAY
               tt-edi.hr-trans     = TIME.
        FIND FIRST tt-edi NO-LOCK NO-ERROR.

        

    DEF VAR l-acomp   AS LOGICAL NO-UNDO INIT FALSE.
    ASSIGN  l-acomp = VALID-HANDLE(h-acomp-edi).

    IF l-acomp THEN DO:
        RUN pi-acompanhar IN h-acomp-edi (INPUT  "Aguarde processando arquivo lido ...")  NO-ERROR.
    END.

    FOR EACH tt-pd1 NO-LOCK,
        FIRST tt-ep1 NO-LOCK
            WHERE tt-ep1.r-rowid = ROWID(tt-pd1):

        /*IF l-acomp THEN DO:
            RUN pi-acompanhar IN h-acomp-edi (INPUT "Processando pedido : " + tt-pd1.num-ped-compra) NO-ERROR.
        END.*/

        /*RUN pi-traduz IN h-formata-codigo ( INPUT TRIM(tt-pd1.codigo-refer),
                                            OUTPUT codigo-refer ).*/
        RUN pi-traduz ( INPUT TRIM(tt-pd1.codigo-refer),
                        OUTPUT codigo-refer ).

        FOR EACH tt-pd2 NO-LOCK
                WHERE tt-pd2.r-rowid = ROWID(tt-pd1),
            EACH tt-pd3 NO-LOCK
                WHERE tt-pd3.r-rowid = ROWID(tt-pd2):

            ASSIGN nr-pedcli = REPLACE(TRIM(tt-pd3.pedido-cliente), "  ", " ").

            FIND FIRST tt-ped-edi 
                WHERE tt-ped-edi.cod-emitente = tt-edi.cod-emitente
                  AND tt-ped-edi.id-movimento = tt-edi.id-movimento
                  AND tt-ped-edi.id-programa  = tt-ep1.id-programa-atu
                  AND tt-ped-edi.codigo-refer = codigo-refer NO-ERROR.
            IF NOT AVAIL tt-ped-edi THEN DO:
        
                CREATE tt-ped-edi.
                ASSIGN tt-ped-edi.cod-emitente   = tt-edi.cod-emitente
                       tt-ped-edi.id-movimento   = tt-edi.id-movimento
                       tt-ped-edi.id-programa    = tt-ep1.id-programa-atu
                       tt-ped-edi.dt-programa    = tt-ep1.dt-programa-atu
                       tt-ped-edi.codigo-refer   = codigo-refer
                       tt-ped-edi.nr-pedcli      = nr-pedcli
                       tt-ped-edi.tipo-fornec    = tt-pd1.cod-tipo-forn
                       tt-ped-edi.alt-tec        = tt-pd1.altera-tecnica
                       tt-ped-edi.dt-ult-entrega = tt-pd1.dt-ult-nf
                       tt-ped-edi.qt-ult-entrega = 0 /* pegar da NF ? */
                       tt-ped-edi.ult-nf         = tt-pd1.num-ult-nf
                       tt-ped-edi.ult-nf-serie   = tt-pd1.serie-ult-nf
                       tt-ped-edi.ult-nf-data    = tt-pd1.dt-ult-nf
                       tt-ped-edi.qt-acumul      = tt-pd1.qt-entrega-acu
                       tt-ped-edi.it-codigo = fi-get-it-codigo (tt-ped-edi.cod-emitente,
                                                                tt-ped-edi.nr-pedcli,
                                                                tt-ped-edi.codigo-refer,
                                                                tt-ped-edi.alt-tec)
                       .
                IF tt-ped-edi.it-codigo = '' AND tt-ped-edi.cod-emitente = 10000326 THEN DO:
                    ASSIGN codigo-refer = STRING(INT(codigo-refer))
                           tt-ped-edi.it-codigo = fi-get-it-codigo (tt-ped-edi.cod-emitente,
                                                                    tt-ped-edi.nr-pedcli,
                                                                    codigo-refer,
                                                                    tt-ped-edi.alt-tec) NO-ERROR.
                    IF tt-ped-edi.it-codigo <> '' THEN 
                        ASSIGN tt-ped-edi.codigo-refer = codigo-refer.
                END.
                
        
                FIND FIRST tt-te1 NO-LOCK 
                    WHERE tt-te1.r-rowid = ROWID(tt-pd1) NO-ERROR.
                IF AVAIL tt-te1 THEN
                    ASSIGN tt-ped-edi.narrativa = TRIM(tt-te1.txt-informa).
            END. /* not avail tt-ped-edi */

            DO i-aux = 1 TO EXTENT(tt-pd2.dt-entrega):

                IF tt-pd2.dt-entrega[i-aux] = ? THEN NEXT.

/*                 CREATE tt-ped-edi-ent.                                                     */
/*                 ASSIGN tt-ped-edi-ent.cod-emitente = tt-edi.cod-emitente                   */
/*                        tt-ped-edi-ent.id-movimento = tt-edi.id-movimento                   */
/*                        tt-ped-edi-ent.id-programa  = tt-ped-edi.id-programa                */
/*                        tt-ped-edi-ent.nr-pedcli    = tt-ped-edi.nr-pedcli                  */
/*                        tt-ped-edi-ent.codigo-refer = tt-ped-edi.codigo-refer               */
/*                        tt-ped-edi-ent.alt-tec      = tt-ped-edi.alt-tec                    */
/*                        tt-ped-edi-ent.it-codigo    = tt-ped-edi.it-codigo                  */
/*                        tt-ped-edi-ent.dt-entrega   = tt-pd2.dt-entrega[i-aux]              */
/*                        tt-ped-edi-ent.qt-entrega   = tt-pd2.qt-entrega[i-aux]              */
/*                        tt-ped-edi-ent.situacao     = 1. /* s¢ tem firme nesse arquivo? */  */

                FIND FIRST tt-ped-edi-ent
                     WHERE tt-ped-edi-ent.cod-emitente = tt-edi.cod-emitente
                       AND tt-ped-edi-ent.nr-pedcli    = tt-ped-edi.nr-pedcli
                       AND tt-ped-edi-ent.it-codigo    = tt-ped-edi.it-codigo
                       AND tt-ped-edi-ent.dt-entrega   = tt-pd2.dt-entrega[i-aux] NO-ERROR.
                IF NOT AVAIL tt-ped-edi-ent THEN DO:
                   CREATE tt-ped-edi-ent.
                   ASSIGN tt-ped-edi-ent.cod-emitente = tt-edi.cod-emitente
                          tt-ped-edi-ent.id-movimento = tt-edi.id-movimento
                          tt-ped-edi-ent.id-programa  = tt-ped-edi.id-programa
                          tt-ped-edi-ent.nr-pedcli    = tt-ped-edi.nr-pedcli
                          tt-ped-edi-ent.codigo-refer = tt-ped-edi.codigo-refer
                          tt-ped-edi-ent.alt-tec      = tt-ped-edi.alt-tec
                          tt-ped-edi-ent.it-codigo    = tt-ped-edi.it-codigo
                          tt-ped-edi-ent.dt-entrega   = tt-pd2.dt-entrega[i-aux]
                          /* tt-ped-edi-ent.qt-entrega   = tt-pd2.qt-entrega[i-aux] */
                          tt-ped-edi-ent.situacao     = 1 /* s¢ tem firme nesse arquivo? */
                          tt-ped-edi-ent.cod-destino  = tt-ep1.cod-fab-destino
                          .
                END.
                IF AVAIL tt-ped-edi-ent THEN
                   ASSIGN tt-ped-edi-ent.qt-entrega = tt-ped-edi-ent.qt-entrega + tt-pd2.qt-entrega[i-aux].

            END.
        END. /* each tt-pd2 ... */

    END. /* /for each tt-pd1 */


    END. /* main_loop .. */
END PROCEDURE.

FUNCTION fi-menos-diasuteis RETURNS DATE (data AS DATE, n-dias AS INTEGER) :
    IF n-dias < 0 THEN
        RETURN data.
    DEF VAR tot-dias AS INT NO-UNDO INIT 0.
    looop:
    DO WHILE TRUE:
        ASSIGN data = data - 1.
        IF WEEKDAY(data) <> 1 AND WEEKDAY(data) <> 7 THEN DO:
            tot-dias = tot-dias + 1.
        END.
        IF n-dias <= tot-dias THEN LEAVE looop.
    END.
    RETURN data.
END FUNCTION.

PROCEDURE pi-traduz:
    DEF INPUT  PARAM i-codigo-refer AS CHAR NO-UNDO.
    DEF OUTPUT PARAM o-codigo-refer AS CHAR NO-UNDO.

    IF SUBSTRING(i-codigo-refer, 12, 1) = '0' THEN
        ASSIGN o-codigo-refer = SUBSTRING(i-codigo-refer, 13, 6).
    ELSE
        ASSIGN o-codigo-refer = SUBSTRING(i-codigo-refer, 12, 7).
END PROCEDURE.
