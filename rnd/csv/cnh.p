{c:/work/desenv/edi/tt-ediccs.i}
{c:/work/desenv/edi/edi-util.p}
{c:/work/desenv/edi/csv/cnh/tt-tabelas.i}
{c:/work/desenv/edi/csv/cnh/util.i}

DEFINE NEW GLOBAL SHARED VAR h-acomp-edi AS HANDLE NO-UNDO.

PROCEDURE le-EDI:
   DEF INPUT PARAM c-arquivo AS CHAR NO-UNDO.

   main_loop:
   DO 
       ON ERROR UNDO, LEAVE
       ON STOP  UNDO, LEAVE:

       RUN importa-csv (c-arquivo).
       RUN pi-processa-arquivo.
       IF RETURN-VALUE = 'NOK' THEN DO:
           RUN utp/ut-msgs.p (INPUT "show":U, 
                              INPUT 17006, 
                              INPUT "Erro EDI~~Erro importando EDI, envie o arquivo que est  sendo importado para o TI para analise."
                             ).
       END.
   END.
END.


/**************************************************************/

PROCEDURE pi-processa-arquivo:

    DEF VAR id-movimento AS CHAR NO-UNDO.
    DEF VAR c-cgc AS CHAR NO-UNDO.

    DEF VAR i-cod-emitente LIKE emitente.cod-emitente NO-UNDO.
    DEF VAR nr-pedcli AS CHAR NO-UNDO.
    DEF VAR codigo-refer LIKE ITEM.codigo-refer NO-UNDO.
    DEF VAR dt-entrega AS DATE NO-UNDO.

    DEF VAR l-acomp   AS LOGICAL NO-UNDO INIT FALSE.
    ASSIGN  l-acomp = VALID-HANDLE(h-acomp-edi).

    RUN pi-limpa-temp-tables.

    DO 
        ON ERROR UNDO, RETURN "NOK"
        ON STOP  UNDO, RETURN "NOK":

        FOR EACH tt-csv NO-LOCK:

            IF l-acomp THEN DO:
                RUN pi-acompanhar IN h-acomp-edi (INPUT "Processando pedido : " + tt-csv.po) NO-ERROR.
            END.

            ASSIGN i-cod-emitente = fi-emitente-da-planta(tt-csv.plant).

            IF i-cod-emitente = 0 THEN DO:
                /* do log */
                NEXT.
            END.

            FIND FIRST emitente NO-LOCK
                WHERE emitente.cod-emitente = i-cod-emitente NO-ERROR.
            IF NOT AVAIL emitente THEN DO: /* na verdade precisa ver se est  ativo!! */
                /* do log */
                NEXT.
            END.

            /* not yet.
            FIND FIRST ccs-edi-conf 
                WHERE ccs-edi-conf.cod-emitente = emitente.cod-emitente NO-ERROR.
            IF NOT AVAIL ccs-edi-conf THEN STOP.
            */

            RUN get-id-movtimento(OUTPUT id-movimento).
            
            FIND FIRST tt-edi 
                WHERE tt-edi.cod-emitente = emitente.cod-emitente
                  AND tt-edi.id-movimento = id-movimento NO-ERROR.
            IF NOT AVAIL tt-edi THEN DO:
                CREATE tt-edi.
                ASSIGN tt-edi.cod-emitente = emitente.cod-emitente
                       tt-edi.id-movimento = id-movimento
                       tt-edi.dt-movto     = fi-converte-data(tt-csv.rel-date)
                       tt-edi.hr-movto     = 0
                       tt-edi.dt-trans     = TODAY
                       tt-edi.hr-trans     = TIME NO-ERROR.
                IF tt-edi.dt-movto = ? THEN
                    ASSIGN tt-edi.dt-movto = TODAY.
            END.

            ASSIGN nr-pedcli = TRIM(tt-csv.po)
                   codigo-refer = TRIM(tt-csv.codigo-refer).
            IF codigo-refer BEGINS "00" THEN
                ASSIGN codigo-refer = SUBSTR(codigo-refer, 3, 20). // aparentemente tem itens que vem com zeros a esquerda!

            FIND FIRST tt-ped-edi 
                WHERE tt-ped-edi.cod-emitente = tt-edi.cod-emitente
                  AND tt-ped-edi.id-movimento = tt-edi.id-movimento
                  AND tt-ped-edi.nr-pedcli    = nr-pedcli
                  AND tt-ped-edi.codigo-refer = codigo-refer NO-ERROR.
            IF NOT AVAIL tt-ped-edi THEN DO:
                CREATE tt-ped-edi.
                ASSIGN tt-ped-edi.cod-emitente = tt-edi.cod-emitente
                       tt-ped-edi.id-movimento = tt-edi.id-movimento
                       tt-ped-edi.id-programa  = tt-edi.id-movimento
                       tt-ped-edi.dt-programa  = tt-edi.dt-movto
                       tt-ped-edi.codigo-refer = codigo-refer
                       tt-ped-edi.nr-pedcli    = nr-pedcli
                       tt-ped-edi.tipo-fornec  = ""
                       tt-ped-edi.alt-tec      = TRIM(tt-csv.rev)
                       .

                ASSIGN tt-ped-edi.it-codigo = fi-get-it-codigo (INPUT tt-edi.cod-emitente,
                                                                INPUT nr-pedcli,
                                                                INPUT codigo-refer,
                                                                INPUT tt-csv.rev).
    
                ASSIGN tt-ped-edi.dt-ult-entrega = fi-converte-data(tt-csv.last-rec)
                       tt-ped-edi.qt-ult-entrega = fi-converte-int(tt-csv.last-qty)
                       tt-ped-edi.ult-nf         = tt-csv.last-pack-nbr                       
                       tt-ped-edi.qt-acumul      = fi-converte-int(tt-csv.cum-qty)
                       tt-ped-edi.narrativa = TRIM(tt-csv.rel-note-1) +
                                              TRIM(tt-csv.rel-note-2) +
                                              TRIM(tt-csv.rel-note-3) + 
                                              TRIM(tt-csv.rel-note-4) +
                                              TRIM(tt-csv.rel-note-5) +
                                              TRIM(tt-csv.rel-note-6) +
                                              TRIM(tt-csv.rel-note-7) +
                                              TRIM(tt-csv.rel-note-8) +
                                              TRIM(tt-csv.rel-note-9) +
                                              TRIM(tt-csv.rel-note-10)
                      .
            END. /* end of not avail tt-ped-edi */

            CREATE tt-ped-edi-ent.
            ASSIGN tt-ped-edi-ent.cod-emitente = tt-edi.cod-emitente
                   tt-ped-edi-ent.id-movimento = tt-edi.id-movimento
                   tt-ped-edi-ent.id-programa  = tt-ped-edi.id-programa
                   tt-ped-edi-ent.nr-pedcli    = tt-ped-edi.nr-pedcli
                   tt-ped-edi-ent.codigo-refer = tt-ped-edi.codigo-refer
                   tt-ped-edi-ent.alt-tec      = tt-ped-edi.alt-tec
                   tt-ped-edi-ent.it-codigo    = tt-ped-edi.it-codigo
                   tt-ped-edi-ent.dt-entrega   = fi-converte-data(tt-csv.date-due)
                   tt-ped-edi-ent.dt-janela    = tt-ped-edi-ent.dt-entrega
                   tt-ped-edi-ent.qt-entrega   = fi-converte-int(tt-csv.qty-due)
                   tt-ped-edi-ent.situacao     = fi-tipo-entrega(tt-csv.qty-type)
                   .
        END.
    END.
    RETURN 'OK'.
END PROCEDURE.

PROCEDURE get-id-movtimento:
    DEF OUTPUT PARAM id-movimento AS CHAR NO-UNDO.

    ASSIGN id-movimento = tt-csv.rel WHEN tt-csv.rel <> ""
           id-movimento = tt-csv.rel-date WHEN tt-csv.rel = "".
    IF TRIM(id-movimento) = "" THEN DO:
        ASSIGN id-movimento = tt-csv.plant + 
                              STRING(YEAR(TODAY)) + 
                              STRING(MONTH(TODAY), '99') + 
                              STRING(DAY(TODAY), '99').
    END.
END PROCEDURE.

PROCEDURE importa-csv:
    DEF INPUT PARAM nome-arquivo AS CHAR NO-UNDO.

    INPUT FROM VALUE(nome-arquivo).
    REPEAT :
        CREATE tt-csv.
        IMPORT DELIMITER "," tt-csv.
    END.
    INPUT CLOSE.
    
    /* apaga cabe‡alhos e "ruido" */
    FOR EACH tt-csv EXCLUSIVE-LOCK
        WHERE tt-csv.codigo-refer = "PARTNBR" OR 
              tt-csv.codigo-refer = "": 
            DELETE tt-csv. 
    END.
    
END PROCEDURE.

