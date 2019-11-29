{c:/work/desenv/edi/tt-ediccs.i}
{c:/work/desenv/edi/edi-util.p}

{esp/excel.i}

DEFINE NEW GLOBAL SHARED VAR h-acomp-edi AS HANDLE NO-UNDO.

PROCEDURE le-EDI:
    DEF INPUT PARAM c-arquivo AS CHAR NO-UNDO.

    main_loop:
    DO 
        ON ERROR UNDO, LEAVE
        ON STOP  UNDO, LEAVE:

        RUN pi-limpa-temp-tables.

        RUN importa-xls (c-arquivo).
        IF RETURN-VALUE = 'NOK' THEN DO:
            RUN utp/ut-msgs.p (INPUT "show":U, 
                               INPUT 17006, 
                               INPUT "Erro EDI~~Erro importando EDI, envie o arquivo que est  sendo importado para o TI para analise."
                              ).
        END.
    END.
END.

/**************************************************************/

PROCEDURE importa-xls:
    
    DEF INPUT PARAM c-arquivo AS CHAR NO-UNDO.
    
    DEF VAR i-cod-emitente AS INTEGER NO-UNDO.
    DEF VAR c-nr-pedcli AS CHAR NO-UNDO.
    DEF VAR dt-entrega AS DATE NO-UNDO.
    DEF VAR d-qt-pedida AS DECIMAL NO-UNDO.
    DEF VAR c-it-codigo AS CHAR NO-UNDO INIT "".
    
    DEF VAR c-id-processo AS CHAR NO-UNDO.
    
    DEF VAR l-acomp   AS LOGICAL NO-UNDO INIT FALSE.
    ASSIGN  l-acomp = VALID-HANDLE(h-acomp-edi).
    
    RUN initExcel(c-arquivo).
    
    ASSIGN i = 2
           c-id-processo = REPLACE(STRING(TODAY, '99/99/9999'), '/', '').
    
    main_loop:
    DO WHILE TRUE
        ON ERROR UNDO, LEAVE
        ON STOP  UNDO, LEAVE:
    
        IF l-acomp THEN
            RUN pi-acompanhar IN h-acomp-edi (INPUT STRING(i)).
    
        i-cod-emitente = eX:CElls(i, "A"):VALUE.
        IF i-cod-emitente = 0 OR i-cod-emitente = ? THEN
            LEAVE main_loop.
    
        c-nr-pedcli = ex:Cells(i, 'B'):TEXT.
        IF c-nr-pedcli = '' THEN
            NEXT.
    
        c-it-codigo = TRIM(ex:Cells(i, "C"):TEXT).
        IF c-it-codigo = "" THEN 
            NEXT.
    
        FIND FIRST ITEM NO-LOCK
            WHERE ITEM.it-codigo = c-it-codigo NO-ERROR.
        IF NOT AVAIL ITEM THEN
            NEXT.
        IF ITEM.codigo-refer = '' THEN
            NEXT.
    
        ASSIGN dt-entrega = ?
               dt-entrega = ex:CElls(i, 'F'):VALUE NO-ERROR.
        IF dt-entrega = ? THEN
            NEXT.
    
        ASSIGN d-qt-pedida = 0
               d-qt-pedida = ex:Cells(i, 'G'):VALUE NO-ERROR.
    
        IF d-qt-pedida = 0 OR d-qt-pedida = ? THEN
            NEXT.
    
        ex:Cells(i, "H") = 'Registro lido'.
    
    
        FIND FIRST tt-edi SHARE-LOCK 
            WHERE tt-edi.cod-emitente = i-cod-emitente NO-ERROR.
        IF NOT AVAIL tt-edi THEN DO:
            CREATE tt-edi.
            ASSIGN tt-edi.cod-emitente = i-cod-emitente
                   tt-edi.id-movimento = c-id-processo
                   tt-edi.dt-movto     = TODAY 
                   tt-edi.hr-movto     = 0
                   tt-edi.dt-trans     = TODAY
                   tt-edi.hr-trans     = TIME.
        END.
    
        FIND FIRST tt-ped-edi SHARE-LOCK
            WHERE tt-ped-edi.nr-pedcli = c-nr-pedcli
              AND tt-ped-edi.it-codigo = c-it-codigo NO-ERROR.
        IF NOT AVAIL tt-ped-edi THEN DO:
            CREATE tt-ped-edi.
            ASSIGN tt-ped-edi.cod-emitente = i-cod-emitente               
                   tt-ped-edi.nr-pedcli = c-nr-pedcli               
                   tt-ped-edi.id-programa = c-id-processo
                   tt-ped-edi.id-movimento = c-id-processo
                   tt-ped-edi.codigo-refer = ITEM.codigo-refer
                   tt-ped-edi.it-codigo = c-it-codigo
                   tt-ped-edi.ult-nf = ''
                   tt-ped-edi.dt-ult-entr = ?
                   tt-ped-edi.dt-programa = TODAY
                   tt-ped-edi.tipo-fornec = ''
                   .
            ASSIGN tt-ped-edi.alt-tec = ENTRY(2, ITEM.codigo-refer, '#') NO-ERROR.
        END.
    
        CREATE tt-ped-edi-ent.
        ASSIGN tt-ped-edi-ent.cod-emitente = tt-edi.cod-emitente
               tt-ped-edi-ent.id-movimento = tt-edi.id-movimento
               tt-ped-edi-ent.id-programa  = tt-ped-edi.id-programa
               tt-ped-edi-ent.nr-pedcli    = tt-ped-edi.nr-pedcli
               tt-ped-edi-ent.codigo-refer = tt-ped-edi.codigo-refer
               tt-ped-edi-ent.alt-tec      = tt-ped-edi.alt-tec
               tt-ped-edi-ent.it-codigo    = tt-ped-edi.it-codigo
               tt-ped-edi-ent.dt-janela    = dt-entrega
               tt-ped-edi-ent.dt-entrega   = dt-entrega
               tt-ped-edi-ent.qt-entrega   = d-qt-pedida
               tt-ped-edi-ent.situacao     = 4
               .
    
        CATCH erro AS PROGRESS.Lang.SysError:
            DEFINE VARIABLE i-erros AS INTEGER NO-UNDO.
            OUTPUT TO c:\temp\edixls.LOG APPEND.
            PUT UNFORMATTED 
                STRING(TODAY, '99/99/9999') ' ' STRING(TIME, 'HH:MM:SS') SKIP.
            DO i-erros = 1 TO erro:NumMessages :
                PUT UNFORMATTED
                    erro:GetMessageNum(i-erros) ' '
                    erro:GetMessage(i-erros) SKIP.
            END.
            OUTPUT CLOSE.
            ex:VISIBLE = TRUE.
            RUN killExcel.            
            RETURN 'NOK'.
        END.
    END.
    
    ex:VISIBLE = TRUE.
    RUN killExcel.

END PROCEDURE.
