/**
    pi-processa-EDI
    	traduz codigo do cliente
    	gera temp-tables:
    		tt-edi e tt-ped-edi

*/
/* temp-table  */
{c:/work/desenv/edi/tt-ediccs.i}
/***************/
{c:/work/desenv/edi/rnd/fi-util.i}
{c:/work/desenv/rnd/rnd01.p}
/***************/

DEFINE NEW GLOBAL SHARED VAR h-acomp-edi AS HANDLE NO-UNDO.

FUNCTION fi-seta-data-janela RETURNS DATE (INTEGER, DATE) FORWARD.

DEF VAR h-formata-codigo          AS HANDLE  NO-UNDO.

PROCEDURE le-EDI:
    DEF INPUT PARAM nomeArquivo AS CHAR NO-UNDO.

    RUN pi-le-arquivo(INPUT nomeArquivo).
    IF RETURN-VALUE = "NOK" THEN 
        RETURN RETURN-VALUE.
    ELSE
        RUN pi-processa-EDI.
END.

PROCEDURE pi-processa-EDI:
    
    DEF VAR dt-aux  AS DATE NO-UNDO.    
    DEF VAR c-programaFormataCodigo AS CHAR NO-UNDO INIT "edi/edicodcli.p".

    RUN pi-limpa-temp-tables.

    DO 
        ON STOP UNDO, RETURN "NOK" :

        FIND FIRST tt-itp NO-LOCK NO-ERROR.
        IF NOT AVAIL tt-itp THEN 
            STOP.

        FIND FIRST emitente NO-LOCK 
            WHERE emitente.cgc = tt-itp.ident-transmis NO-ERROR.
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
               tt-edi.hr-movto     = INT(SUBSTR(tt-itp.id-movimento, 7, 6))
               tt-edi.dt-trans     = TODAY
               tt-edi.hr-trans     = TIME.
        FIND FIRST tt-edi NO-LOCK NO-ERROR.

        IF ccs-edi-conf.l-oc-pe5 = NO THEN
            RUN pi-processa-normal.
        ELSE
            RUN pi-processa-oc-pe5.

        // RUN limpa-maxion.
        IF tt-edi.cod-emitente = 10005276 THEN
            RUN limpa-entrega-maxion.

    END. /* /do on stop .. */
END PROCEDURE.

PROCEDURE pi-processa-normal :

    DEF VAR dt-aux       AS DATE NO-UNDO.    
    DEF VAR codigo-refer AS CHAR NO-UNDO.
    DEF VAR c-revisao    AS CHAR NO-UNDO.
    DEF VAR nr-pedcli    AS CHAR NO-UNDO.


    DEF VAR l-acomp   AS LOGICAL NO-UNDO INIT FALSE.
    ASSIGN  l-acomp = VALID-HANDLE(h-acomp-edi).

    IF l-acomp THEN DO:
        RUN pi-acompanhar IN h-acomp-edi (INPUT  "Aguarde processando arquivo lido ...")  NO-ERROR.
    END.

    FOR EACH tt-pe1 NO-LOCK:

        IF l-acomp THEN DO:
            RUN pi-acompanhar IN h-acomp-edi (INPUT "Processando pedido : " + tt-pe1.num-ped-compra) NO-ERROR.
        END.

        RUN pi-traduz IN h-formata-codigo ( INPUT TRIM(tt-pe1.cod-it-cliente),
                                            OUTPUT codigo-refer ).

        ASSIGN nr-pedcli = REPLACE(TRIM(tt-pe1.num-ped-compra), "  ", " ").


        CREATE tt-ped-edi.
        ASSIGN tt-ped-edi.cod-emitente = tt-edi.cod-emitente
               tt-ped-edi.id-movimento = tt-edi.id-movimento
               tt-ped-edi.id-programa  = tt-pe1.id-programa
               tt-ped-edi.dt-programa  = tt-pe1.dt-prg-atu
               tt-ped-edi.codigo-refer = codigo-refer
               tt-ped-edi.nr-pedcli    = nr-pedcli
               tt-ped-edi.tipo-fornec  = tt-pe1.cod-tipo-fornec
               tt-ped-edi.alt-tec      = ""
               .

        FIND FIRST tt-pe6 NO-LOCK 
            WHERE tt-pe6.r-rowid = ROWID(tt-pe1) NO-ERROR.
        IF AVAIL tt-pe6 THEN
            ASSIGN tt-ped-edi.alt-tec = REPLACE(tt-pe6.altera-tecnica," ","").

        /*IF tt-ped-edi.alt-tec <> "" THEN
            ASSIGN codigo-refer = codigo-refer + "#" + tt-ped-edi.alt-tec.*/

        ASSIGN tt-ped-edi.it-codigo = fi-get-it-codigo (tt-ped-edi.cod-emitente,
                                                        tt-ped-edi.nr-pedcli,
                                                        tt-ped-edi.codigo-refer,
                                                        tt-ped-edi.alt-tec).

        IF tt-ped-edi.it-codigo = '' AND LENGTH(tt-ped-edi.alt-tec) = 1 THEN DO:

            ASSIGN c-revisao = STRING(INT(tt-ped-edi.alt-tec), '99') NO-ERROR.
            IF c-revisao <> ? AND c-revisao <> '' THEN
                ASSIGN tt-ped-edi.it-codigo = fi-get-it-codigo (tt-ped-edi.cod-emitente,
                                                                tt-ped-edi.nr-pedcli,
                                                                tt-ped-edi.codigo-refer,
                                                                c-revisao).

        END.

            
        FIND FIRST tt-pe2 NO-LOCK 
            WHERE tt-pe2.r-rowid = ROWID(tt-pe1) NO-ERROR.
        IF AVAIL tt-pe2 THEN
            ASSIGN tt-ped-edi.dt-ult-entrega = tt-pe2.dt-ult-entrega
                   tt-ped-edi.qt-ult-entrega = tt-pe2.qt-ult-entrega
                   tt-ped-edi.ult-nf         = tt-pe2.num-ult-nf
                   tt-ped-edi.ult-nf-serie   = tt-pe2.serie-ult-nf
                   tt-ped-edi.ult-nf-data    = tt-pe2.dt-ult-nf
                   tt-ped-edi.qt-acumul      = tt-pe2.qt-entrega-acu
                   .
        
        FIND FIRST tt-te1 NO-LOCK 
            WHERE tt-te1.r-rowid = ROWID(tt-pe1) NO-ERROR.
        IF AVAIL tt-te1 THEN
            ASSIGN tt-ped-edi.narrativa = TRIM(tt-te1.txt-informa).

        RUN pi-grava-entregas(ROWID(tt-pe1), ROWID(tt-ped-edi)).

    END. /* /for each tt-pe1 */

END PROCEDURE.

PROCEDURE pi-grava-entregas:
    DEF INPUT PARAM r-rowid   AS ROWID.
    DEF INPUT PARAM r-ped-edi AS ROWID.

    DEF VAR i-aux         AS INTEGER NO-UNDO.
    DEF VAR it-codigo     AS CHAR    NO-UNDO.
    DEF VAR dt-vigencia   AS DATE NO-UNDO.
    DEF VAR c-alt-tec     AS CHAR NO-UNDO.
    DEF VAR c-alt-tec-alt AS CHAR NO-UNDO.
    
    FIND FIRST tt-ped-edi NO-LOCK 
        WHERE ROWID(tt-ped-edi) = r-ped-edi NO-ERROR.
    IF NOT AVAIL tt-ped-edi THEN STOP.
    
    RUN verifica-narrativa-agco (OUTPUT c-alt-tec-alt,
                                 OUTPUT dt-vigencia).

    FOR EACH tt-pe3 NO-LOCK
            WHERE tt-pe3.r-rowid = r-rowid :

        FIND FIRST tt-pe5 NO-LOCK 
            WHERE tt-pe5.ind = tt-pe3.ind 
              AND tt-pe5.r-rowid = tt-pe3.r-rowid NO-ERROR.
        DO i-aux = 1 TO 7:
            IF tt-pe3.dt-entrega[i-aux] = ? THEN NEXT.
            
            ASSIGN c-alt-tec = tt-ped-edi.alt-tec.
            IF dt-vigencia <> ? AND tt-pe3.dt-entrega[i-aux] > dt-vigencia THEN DO:
                ASSIGN c-alt-tec = c-alt-tec-alt.
            END.
            
            CREATE tt-ped-edi-ent.
            ASSIGN tt-ped-edi-ent.cod-emitente = tt-edi.cod-emitente
                   tt-ped-edi-ent.id-movimento = tt-edi.id-movimento
                   tt-ped-edi-ent.id-programa  = tt-ped-edi.id-programa
                   tt-ped-edi-ent.nr-pedcli    = tt-ped-edi.nr-pedcli
                   tt-ped-edi-ent.codigo-refer = tt-ped-edi.codigo-refer
                   tt-ped-edi-ent.alt-tec      = c-alt-tec
                   tt-ped-edi-ent.it-codigo    = tt-ped-edi.it-codigo
                   tt-ped-edi-ent.dt-entrega   = tt-pe3.dt-entrega[i-aux]
                   tt-ped-edi-ent.qt-entrega   = tt-pe3.qt-entrega[i-aux]
                   tt-ped-edi-ent.cod-destino  = tt-pe1.cod-local-destino
                   .
            IF AVAIL tt-pe5 THEN
                ASSIGN tt-ped-edi-ent.dt-janela = tt-pe5.dt-inic-entrega[i-aux]
                       tt-ped-edi-ent.situacao  = INT(tt-pe5.id-programacao[i-aux]).
        END.
    END.
END PROCEDURE.

PROCEDURE verifica-narrativa-agco:
    DEF OUTPUT PARAM revisao     AS CHAR NO-UNDO FORMAT 'x(10)'.
    DEF OUTPUT PARAM dt-vigencia AS DATE NO-UNDO FORMAT '99/99/9999'.

    DEF VAR c-obs AS CHAR NO-UNDO FORMAT 'x(240)'.
    DEF VAR c-aux AS CHAR NO-UNDO FORMAT 'x(60)'.

    FIND FIRST tt-te1 NO-LOCK 
        WHERE tt-te1.r-rowid = ROWID(tt-pe1) NO-ERROR.
    IF NOT AVAIL tt-te1 THEN
        LEAVE.

    ASSIGN c-obs = TRIM(tt-te1.txt-informa).
    
    IF NOT c-obs MATCHES '*Proxima Emiss*vigenci*' THEN 
        LEAVE.

    /* extrai revisao */
    ASSIGN c-aux = SUBSTRING(c-obs, INDEX(c-obs, 'Desenho:'), 40)
           c-aux = ENTRY(2, c-aux, ':')
           revisao = TRIM(ENTRY(1,  c-aux, ' ')) NO-ERROR.

    /* extrai data */
    ASSIGN c-aux = SUBSTRING(c-obs, INDEX(c-obs, 'a partir de'), 40)
           c-aux = TRIM(REPLACE(c-aux, 'a partir de', ''))
           c-aux = ENTRY(1, c-aux, ' ') 
           dt-vigencia = DATE(c-aux) NO-ERROR.
END PROCEDURE.

PROCEDURE limpa-maxion:

    DEF BUFFER pe FOR tt-ped-edi.

    FOR EACH tt-ped-edi :
        FIND FIRST pe NO-LOCK 
            WHERE pe.cod-emitente = tt-ped-edi.cod-emitente 
              AND pe.nr-pedcli    = tt-ped-edi.nr-pedcli
              AND pe.codigo-refer = tt-ped-edi.codigo-refer
              AND pe.id-programa  > tt-ped-edi.id-programa NO-ERROR.
        IF AVAIL pe THEN DO:
            FOR EACH tt-ped-edi-ent 
                WHERE tt-ped-edi-ent.cod-emitente = tt-ped-edi.cod-emitente
                  AND tt-ped-edi-ent.id-movimento = tt-ped-edi.id-movimento
                  AND tt-ped-edi-ent.id-programa  = tt-ped-edi.id-programa
                  AND tt-ped-edi-ent.nr-pedcli    = tt-ped-edi.nr-pedcli
                  AND tt-ped-edi-ent.codigo-refer = tt-ped-edi.codigo-refer:
                DELETE tt-ped-edi-ent.
            END.
            DELETE tt-ped-edi.
        END.
    END.
END PROCEDURE.

PROCEDURE limpa-entrega-maxion:

    DEF BUFFER pee FOR tt-ped-edi-ent.

    FOR EACH tt-ped-edi-ent :
        FIND FIRST pee NO-LOCK 
            WHERE pee.cod-emitente = tt-ped-edi-ent.cod-emitente 
              AND pee.nr-pedcli    = tt-ped-edi-ent.nr-pedcli
              AND pee.codigo-refer = tt-ped-edi-ent.codigo-refer
              AND pee.id-programa  > tt-ped-edi-ent.id-programa 
              AND pee.dt-entrega   = tt-ped-edi-ent.dt-entrega
              AND pee.qt-entrega   > 0 NO-ERROR.
        IF AVAIL pee THEN DO:
            DELETE tt-ped-edi-ent.
        END.
    END.
END PROCEDURE.

PROCEDURE pi-processa-oc-pe5:

    DEF VAR dt-aux       AS DATE NO-UNDO.
    DEF VAR i-aux        AS INTEGER NO-UNDO.
    DEF VAR codigo-refer AS CHAR NO-UNDO.
    DEF VAR c-revisao    AS CHAR NO-UNDO.
    DEF VAR nr-pedcli    AS CHAR NO-UNDO.

    DEF VAR c-alt-tec-nova AS CHAR NO-UNDO.
    DEF VAR dt-vigencia    AS DATE NO-UNDO.
    
    DEF VAR l-acomp   AS LOGICAL NO-UNDO INIT FALSE.
    ASSIGN  l-acomp = VALID-HANDLE(h-acomp-edi).
    
    IF l-acomp THEN DO:
        RUN pi-acompanhar IN h-acomp-edi (INPUT  "Aguarde processando arquivo lido ...")  NO-ERROR.
    END.
    
    FOR EACH tt-pe5 NO-LOCK,
        FIRST tt-pe1 NO-LOCK
            WHERE ROWID(tt-pe1) = tt-pe5.r-rowid:

        FIND FIRST tt-pe3 NO-LOCK
            WHERE tt-pe3.r-rowid = tt-pe5.r-rowid 
              AND tt-pe3.ind = tt-pe5.ind NO-ERROR.

        RUN verifica-narrativa-agco(OUTPUT c-alt-tec-nova,
                                    OUTPUT dt-vigencia).
        
        DO i-aux = 1 TO 7:

            IF TRIM(tt-pe5.id-prg-atual[i-aux]) = "" THEN NEXT.
            
            RUN pi-traduz IN h-formata-codigo ( INPUT TRIM(tt-pe1.cod-it-cliente),
                                                OUTPUT codigo-refer ).
            
            ASSIGN nr-pedcli = REPLACE(TRIM(tt-pe5.id-prg-atual[i-aux]), "  ", " ").
            
            /* TODO: talvez tenha que adicionar a revis∆o aqui (no filtro) ... */
            FIND FIRST tt-ped-edi NO-LOCK
                WHERE tt-ped-edi.cod-emitente = tt-edi.cod-emitente
                  AND tt-ped-edi.id-movimento = tt-edi.id-movimento
                  AND tt-ped-edi.id-programa  = tt-pe1.id-programa
                  AND tt-ped-edi.codigo-refer = codigo-refer
                  AND tt-ped-edi.nr-pedcli    = nr-pedcli NO-ERROR.
            IF NOT AVAIL tt-ped-edi THEN DO:
                CREATE tt-ped-edi.
                ASSIGN tt-ped-edi.cod-emitente = tt-edi.cod-emitente
                       tt-ped-edi.id-movimento = tt-edi.id-movimento
                       tt-ped-edi.id-programa  = tt-pe1.id-programa
                       tt-ped-edi.dt-programa  = tt-pe1.dt-prg-atu
                       tt-ped-edi.codigo-refer = codigo-refer
                       tt-ped-edi.nr-pedcli    = nr-pedcli
                       tt-ped-edi.tipo-fornec  = tt-pe1.cod-tipo-fornec
                       tt-ped-edi.alt-tec      = ""
                       .
                
                FIND FIRST tt-pe6 NO-LOCK 
                    WHERE tt-pe6.r-rowid = ROWID(tt-pe1) NO-ERROR.
                IF AVAIL tt-pe6 THEN
                    ASSIGN tt-ped-edi.alt-tec = REPLACE(tt-pe6.altera-tecnica, " ", "").

                ASSIGN c-revisao = tt-ped-edi.alt-tec.
                IF dt-vigencia <> ? AND dt-vigencia < tt-pe3.dt-entrega[i-aux] THEN DO:
                    ASSIGN c-revisao = c-alt-tec-nova.
                END.


                ASSIGN tt-ped-edi.it-codigo = fi-get-it-codigo (tt-ped-edi.cod-emitente,
                                                                tt-ped-edi.nr-pedcli,
                                                                tt-ped-edi.codigo-refer,
                                                                c-revisao).

                IF tt-ped-edi.it-codigo = '' AND LENGTH(c-revisao) = 1 THEN DO:

                    ASSIGN c-revisao = STRING(INT(c-revisao), '99') NO-ERROR.
                    IF c-revisao <> ? AND c-revisao <> '' THEN
                        ASSIGN tt-ped-edi.it-codigo = fi-get-it-codigo (tt-ped-edi.cod-emitente,
                                                                        tt-ped-edi.nr-pedcli,
                                                                        tt-ped-edi.codigo-refer,
                                                                        c-revisao).
                END.

                
                FIND FIRST tt-pe2 NO-LOCK 
                    WHERE tt-pe2.r-rowid = ROWID(tt-pe1) NO-ERROR.
                IF AVAIL tt-pe2 THEN
                    ASSIGN tt-ped-edi.dt-ult-entrega = tt-pe2.dt-ult-entrega
                           tt-ped-edi.qt-ult-entrega = tt-pe2.qt-ult-entrega
                           tt-ped-edi.ult-nf         = tt-pe2.num-ult-nf
                           tt-ped-edi.ult-nf-serie   = tt-pe2.serie-ult-nf
                           tt-ped-edi.ult-nf-data    = tt-pe2.dt-ult-nf
                           tt-ped-edi.qt-acumul      = tt-pe2.qt-entrega-acu
                           .
                
                FIND FIRST tt-te1 NO-LOCK 
                    WHERE tt-te1.r-rowid = ROWID(tt-pe1) NO-ERROR.
                IF AVAIL tt-te1 THEN
                    ASSIGN tt-ped-edi.narrativa = TRIM(tt-te1.txt-informa).
            END.
            
            
            CREATE tt-ped-edi-ent.
            ASSIGN tt-ped-edi-ent.cod-emitente = tt-edi.cod-emitente
                   tt-ped-edi-ent.id-movimento = tt-edi.id-movimento
                   tt-ped-edi-ent.id-programa  = tt-ped-edi.id-programa
                   tt-ped-edi-ent.nr-pedcli    = tt-ped-edi.nr-pedcli
                   tt-ped-edi-ent.codigo-refer = tt-ped-edi.codigo-refer
                   tt-ped-edi-ent.alt-tec      = c-revisao /* tt-ped-edi.alt-tec */
                   tt-ped-edi-ent.it-codigo    = tt-ped-edi.it-codigo
                   tt-ped-edi-ent.dt-entrega   = tt-pe3.dt-entrega[i-aux]
                   tt-ped-edi-ent.qt-entrega   = tt-pe3.qt-entrega[i-aux]
                   tt-ped-edi-ent.dt-janela = tt-pe5.dt-inic-entrega[i-aux]
                   tt-ped-edi-ent.situacao  = INT(tt-pe5.id-programacao[i-aux])
                   tt-ped-edi-ent.cod-destino  = tt-pe1.cod-local-destino
                   .

            ASSIGN tt-ped-edi-ent.dt-janela = fi-seta-data-janela(tt-edi.cod-emitente,
                                                                  tt-ped-edi-ent.dt-entrega).

        END. /* DO i-aux ... */
    END. /* /for each tt-pe1 */
END PROCEDURE.

FUNCTION fi-seta-data-janela RETURNS DATE (cod-emitente AS INTEGER, dt-entrega AS DATE):
    /* Troca data de entrega.  */

    DEF VAR c-cod-emitente AS CHAR NO-UNDO.
    ASSIGN c-cod-emitente = STRING(cod-emitente).
    
    /** 
     * Para AGCO:
     * Temos que tirar 3 dias, pois Ç esse o tempo que leva
     * para a carga chegar l† ... e a data de entrega enviada
     * Ç a data para estar l† e n∆o sair daqui !!!
     * Mantive a data de janela original para referencia! Decis∆o minha!!
     * Mas n∆o Ç janela realmente!!!!!!
     */
    IF INDEX('10000553,10006577,10007613,10007755,10008551,10008552,10008689,10008690,10009044,10009046,10009047', c-cod-emitente) > 0 THEN DO:
        RETURN fi-menos-diasuteis(dt-entrega, 3). /* 3 dias uteis (hoje - 2) */
    END.

    // Caruline 05/11/2019 15:03
    IF INDEX('10009048,10009049,', c-cod-emitente) > 0 THEN DO:
        RETURN fi-menos-diasuteis(dt-entrega, 5). 
    END.

    IF INDEX('10008092,10009045,', c-cod-emitente) > 0 THEN DO:
        RETURN fi-menos-diasuteis(dt-entrega, 5). /* 6 dias uteis; 5 porque: atual + 5 = 6 */
    END.
    /**
     * caso especial John Deere:
     *   Retirar um dia da data de final (Tamires 9/10/18 07:16)
     *  outro c¢digo e dias corridos para 6441 (Tamires 13/03/19 14:28)
     ********************************************************************************************************/
    IF INDEX('10006441,10005705,10008330,10006052,10006891,', c-cod-emitente) > 0 THEN  DO:

        IF INDEX('10006891,', c-cod-emitente) > 0 THEN DO:

            RETURN fi-menos-diasuteis(dt-entrega, 2).

        END. ELSE IF INDEX('10006441,', c-cod-emitente) > 0 THEN DO:

            RETURN dt-entrega - 6.

        END. ELSE DO:

            RETURN fi-menos-diasuteis(dt-entrega, 1).

        END.
    END.

    /**
     * n∆o existe regra .. 
     * retorna a mesma data */
    RETURN dt-entrega.

END FUNCTION.


/* TESTE *****
 *
DEF VAR arquivo AS CHAR NO-UNDO 
    INIT "\\servcitrix\edi-rec-sawluz\RND-001-08-65880916.EDI".
    /*INIT '\\servcitrix\EDI-REC-CAT\20190718\ADM.0255.000001.RND00107_61064911000177_20190718025004_067.txt'.*/
RUN le-EDI(arquivo).

FOR EACH tt-ped-edi-ent NO-LOCK
        /*WHERE tt-ped-edi-ent.codigo-refer = 'ACW2230500'*/ 
    BY tt-ped-edi-ent.codigo-refer
    BY tt-ped-edi-ent.dt-entrega:

    DISP tt-ped-edi-ent.codigo-refer
         tt-ped-edi-ent.alt-tec
         tt-ped-edi-ent.dt-entrega
         tt-ped-edi-ent.it-codigo.
END.
 */
