&IF DEFINED(EXCEL_I) <> 1 &THEN
&GLOBAL-DEFINE EXCEL_I Y
/***************************************************
 *   Fun‡äes (diversas) para interface com o Excel.
 ***************************************************/
DEFINE NEW SHARED VARIABLE ex AS COM-HANDLE         NO-UNDO LABEL "Excel Handle":U.
DEFINE NEW SHARED VARIABLE i  AS INTEGER INITIAL 2  NO-UNDO LABEL "Linha":U.
DEFINE NEW SHARED VARIABLE sheet_number AS INTEGER INITIAL 1  NO-UNDO LABEL "Pagina":U.
/***************************************************
 * "constantes" para alinhamento do valor da c‚lula 
 ***************************************************/
DEFINE NEW SHARED VARIABLE AL_NENHUM   AS INTEGER INITIAL 0     LABEL "sem alinhamento":u   NO-UNDO.
DEFINE NEW SHARED VARIABLE AL_ESQUERDA AS INTEGER INITIAL -4131 LABEL "alinhado esquerda":u NO-UNDO.
DEFINE NEW SHARED VARIABLE AL_DIREITA  AS INTEGER INITIAL -4152 LABEL "alinhado direita":u  NO-UNDO.
DEFINE NEW SHARED VARIABLE AL_CENTRO   AS INTEGER INITIAL -4108 LABEL "centralizado":u      NO-UNDO.
DEFINE NEW SHARED VARIABLE COLOR_RED   AS INTEGER INITIAL -16776961.
/***************************************************/
FUNCTION existe_arquivo RETURNS LOGICAL (nome_arquivo AS CHAR):
    FILE-INFO:FILE-NAME = nome_arquivo.
    IF FILE-INFO:FULL-PATHNAME = "":U0 OR FILE-INFO:FULL-PATHNAME = ? THEN
        RETURN FALSE.
    ELSE
        RETURN TRUE.
END FUNCTION.
/***************************************************/
PROCEDURE initExcel:
    DEF INPUT PARAM arquivo AS CHAR INITIAL "":U NO-UNDO LABEL "Arquivo":U.

    CREATE "Excel.Application":U ex.

    IF existe_arquivo(arquivo) THEN
        RUN _openWorkBook(arquivo, /*ReadOnly:*/ FALSE).
    ELSE
        RUN _addWorkBook.    
                
    ASSIGN i = 2.
    ex:Calculation = -4135 NO-ERROR. /*xlManual */
END PROCEDURE.
/***************************************************/
PROCEDURE initExcelRO:
    DEF INPUT PARAM arquivo AS CHAR INITIAL "":U NO-UNDO LABEL "Arquivo":U.

    CREATE "Excel.Application":U ex.

    IF existe_arquivo(arquivo) THEN
        RUN _openWorkBook(arquivo, /*ReadOnly:*/ TRUE).
    ELSE
        RUN _addWorkBook.    

    ASSIGN i = 2.
    ex:Calculation = -4135 NO-ERROR. /*xlManual */
END PROCEDURE.
/***************************************************/
PROCEDURE abrirExcel:
    DEF INPUT PARAM arquivo AS CHAR INITIAL "":U NO-UNDO LABEL "Arquivo":U.
    RUN initExcelRO(arquivo).
END PROCEDURE.
/***************************************************/
PROCEDURE _openWorkBook:
    DEF INPUT PARAM file_name AS CHAR NO-UNDO.
    DEF INPUT PARAM read_only AS LOGICAL NO-UNDO.

    DEF VAR update_links AS LOGICAL NO-UNDO INIT FALSE.

    ex:VISIBLE = TRUE.
    ex:ScreenUpdating = FALSE NO-ERROR.
    ex:WorkBooks:OPEN(file_name, update_links, read_only) NO-ERROR.
    ex:VISIBLE = FALSE.

    IF ex:Workbooks:COUNT = 0 THEN DO:
        RUN utp/ut-msgs.p (INPUT "show":U, 
                           INPUT 17006, 
                           INPUT "Erro Excel~~Erro ao abrir planilha! ~ Os erros mais comuns sÆo: filtros ativados, processo do Excel travado, a planilha estar uso (pelo mesmo ou) por outro usu rio, sem permissÆo de acesso a planilha."
                          ).
        RUN killError.
    END.
END PROCEDURE.
/***************************************************/
PROCEDURE _addWorkBook:
    ex:WorkBooks:ADD().
    DO WHILE ex:Sheets:COUNT > 1:
        ex:Sheets(2):DELETE.
    END.
END PROCEDURE.
/***************************************************/
PROCEDURE killExcel:
    ex:Calculation = -4105 NO-ERROR. /* xlAutomatic */
    ex:ScreenUpdating = TRUE NO-ERROR.
    RUN killExObject.
END PROCEDURE.
/***************************************************/
PROCEDURE killExcel2:
    ex:Calculation = -4105 NO-ERROR. /* xlAutomatic */
    RUN killExObject.
END PROCEDURE.
/***************************************************/
PROCEDURE killExObject:    
    RELEASE OBJECT ex NO-ERROR.
END PROCEDURE.
/***************************************************/
PROCEDURE killError:
    ex:ScreenUpdating = TRUE NO-ERROR.
    RUN killExObject.
    STOP.
END PROCEDURE.
/***************************************************/
PROCEDURE showExcel:    
    RUN freeze-primeira-linha NO-ERROR.
    RUN ajusta-colunas NO-ERROR.

    ex:VISIBLE = TRUE.
END PROCEDURE.
/***************************************************/
PROCEDURE freeze-primeira-linha:
    ex:Range("A1:A1"):SELECT.
    ex:Range("1:1":U):FONT:Bold = TRUE.
    ex:ActiveWindow:SplitColumn = 0  NO-ERROR.
    ex:ActiveWindow:SplitRow = 1  NO-ERROR.
    ex:ActiveWindow:FreezePanes = TRUE NO-ERROR.
END PROCEDURE.
/***************************************************/
PROCEDURE ajusta-colunas:
    ex:Cells:Select.
    ex:Cells:EntireColumn:AutoFit NO-ERROR.
END PROCEDURE.
/*********************************
 * ultima linha vazia da planilha 
 **********************************/
/* TODO: tem como ver isso usando uma fun‡Æo de busca do excel !!! */
FUNCTION ultimaLinha RETURNS INTEGER (lin AS INTEGER, co AS CHAR):
    procura_empty:
    REPEAT:
        IF ex:Cells(lin, co):TEXT = '':U THEN
            LEAVE procura_empty.
        lin = lin + 1.
        IF lin >= ex:Rows:COUNT THEN
            RETURN 0.
    END.
    RETURN lin.
END .
/********************************************** 
 * pegar letra (excel) pelo numero da coluna   
 **********************************************/
FUNCTION num2letra RETURNS CHAR (num AS INTEGER):
    DEF VAR letra AS CHAR FORMAT "x(2)":U NO-UNDO LABEL "Coluna":U.
    DEF VAR div AS INTEGER NO-UNDO.
    DEF VAR aux AS CHAR FORMAT "X(1)":U EXTENT 26 NO-UNDO
        INITIAL ["A":U,"B":U,"C":U,"D":U,"E":U,"F":U,"G":U,"H":U,"I":U,"J":U,
                 "K":U,"L":U,"M":U,"N":U,"O":U,"P":U,"Q":U,"R":U,"S":U,"T":U,
                 "U":U,"V":U,"W":U,"X":U,"Y":U,"Z":U].
    IF num <= 26 THEN
        ASSIGN letra = aux[num].
    ELSE DO:
        ASSIGN div = INT(TRUNC((num / 26),0)).
        IF (num - 26) MODULO 26 = 0 THEN
        DO:
            ASSIGN letra = aux[div - 1].        
            ASSIGN letra = letra + aux[26].           
        END.
        ELSE 
        DO:
            ASSIGN letra = aux[div].        
            ASSIGN letra = letra + aux[num - 26 * div].
        END.
    END.
    RETURN letra.
END .
/**********************************************
 * Verifica se existe Sheet pelo Nome  
 **********************************************/
FUNCTION existeSheet RETURNS LOGICAL (Sheet AS CHAR):

    DEF VAR num_sheets AS INTEGER NO-UNDO LABEL "Num. Sheets":U11.
    DEF VAR cur_sheet  AS INTEGER NO-UNDO LABEL "Aux int":U7.

    DEF VAR sheet_name AS CHAR NO-UNDO LABEL "Nome Sheet":U10.

    ASSIGN num_sheets = ex:Sheets:COUNT.
    DO cur_sheet = 1 TO num_sheets:
        ASSIGN sheet_name = ex:Sheets(cur_sheet):NAME.
        IF Sheet = sheet_name THEN DO:
            RETURN TRUE.
        END.
    END.
    RETURN FALSE.
END.
/**********************************************
 *  Adiciona Celula, alinhando a mesma    
 **********************************************/
PROCEDURE addCellStr:
    DEF INPUT PARAM coluna AS CHAR NO-UNDO LABEL "Coluna":U.
    DEF INPUT PARAM valor  AS CHAR NO-UNDO LABEL "Valor celula":U.
    DEF INPUT PARAM alinhamento AS INTEGER NO-UNDO LABEL "Alinhamento":U.

    ex:Cells(i, coluna):NumberFormat = "@":U.
    ex:Cells(i, coluna) = valor NO-ERROR.

    IF alinhamento <> 0 THEN
        ex:Cells(i, coluna):HorizontalAlignment = alinhamento.

END PROCEDURE.

PROCEDURE addCellInt:
    DEF INPUT PARAM coluna AS CHAR NO-UNDO LABEL "Coluna":U.
    DEF INPUT PARAM valor  AS INTEGER NO-UNDO LABEL "Valor celula":U.
    DEF INPUT PARAM alinhamento AS INTEGER NO-UNDO LABEL "Alinhamento":U.

    ex:Cells(i, coluna) = valor NO-ERROR.

    IF alinhamento <> 0 THEN
        ex:Cells(i, coluna):HorizontalAlignment = alinhamento.

END PROCEDURE.

PROCEDURE addCellDec:
    DEF INPUT PARAM coluna AS CHAR NO-UNDO LABEL "Coluna":U.
    DEF INPUT PARAM valor  AS DECIMAL NO-UNDO LABEL "Valor celula":U.
    DEF INPUT PARAM alinhamento AS INTEGER NO-UNDO LABEL "Alinhamento":U.

    ex:Cells(i, coluna) = valor NO-ERROR.

    IF alinhamento <> 0 THEN
        ex:Cells(i, coluna):HorizontalAlignment = alinhamento.

END PROCEDURE.

PROCEDURE addLine: /* TODO: transformar em fun‡Æo */
    ASSIGN i = i + 1.
    IF i > ex:Rows:COUNT THEN DO:
        ex:ActiveSheet:NAME = "Pagina " + STRING(sheet_number).
        ASSIGN i = 2
               sheet_number = sheet_number + 1.
        ex:Sheets:ADD.
        ex:ActiveSheet:NAME = "Pagina " + STRING(sheet_number).
    END.
END PROCEDURE.

PROCEDURE addLines: /* wtf? */
    DEF INPUT PARAM n AS INTEGER NO-UNDO.
    DEF VAR aux AS INT NO-UNDO.
    DO aux = 1 TO n:
        RUN addLine.
    END.
END PROCEDURE.
/**********************************************
 *  Dialogo de abertura de arquivos XLS    
 **********************************************/
FUNCTION showOpen RETURNS CHAR:
    DEF VAR file_name AS CHAR NO-UNDO LABEL "Arquivo":U.
    SYSTEM-DIALOG GET-FILE file_name
                  FILTERS "Microsoft Excel":U "*.xls|*.xlsx":U 
                  MUST-EXIST.
    RETURN file_name.
END .

PROCEDURE borda-grossa:
    DEF INPUT PARAM range AS CHAR NO-UNDO.
    RUN borda-grossa-externa(range).
    ex:Range(range):Borders(11):Weight = 3.
    ex:Range(range):Borders(12):Weight = 3.
END PROCEDURE.

PROCEDURE borda-grossa-externa:
    DEF INPUT PARAM range AS CHAR NO-UNDO.
    ex:Range(range):Borders(7):Weight = 3.
    ex:Range(range):Borders(8):Weight = 3.
    ex:Range(range):Borders(9):Weight = 3.
    ex:Range(range):Borders(10):Weight = 3.
END PROCEDURE.

PROCEDURE borda-grossa-cor:
    DEF INPUT PARAM range AS CHAR NO-UNDO.
    DEF INPUT PARAM TintAndShade AS DECIMAL NO-UNDO.
    RUN borda-grossa-externa-cor(range, TintAndShade).
    ex:Range(range):Borders(11):LineStyle = 1.
    ex:Range(range):Borders(12):LineStyle = 1.
    ex:Range(range):Borders(11):Weight = 3.
    ex:Range(range):Borders(12):Weight = 3.
    ex:Range(range):Borders(11):ThemeColor = 1.
    ex:Range(range):Borders(12):ThemeColor = 1.
    ex:Range(range):Borders(11):TintAndShade = TintAndShade.
    ex:Range(range):Borders(12):TintAndShade = TintAndShade.
END PROCEDURE.

PROCEDURE borda-grossa-externa-cor:
    DEF INPUT PARAM range AS CHAR NO-UNDO.
    DEF INPUT PARAM TintAndShade AS DECIMAL NO-UNDO.
    ex:Range(range):Borders(7):LineStyle = 1.
    ex:Range(range):Borders(8):LineStyle = 1.
    ex:Range(range):Borders(9):LineStyle = 1.
    ex:Range(range):Borders(10):LineStyle = 1.
    ex:Range(range):Borders(7):Weight = 3.
    ex:Range(range):Borders(8):Weight = 3.
    ex:Range(range):Borders(9):Weight = 3.
    ex:Range(range):Borders(10):Weight = 3.
    ex:Range(range):Borders(7):ThemeColor = 1.
    ex:Range(range):Borders(8):ThemeColor = 1.
    ex:Range(range):Borders(9):ThemeColor = 1.
    ex:Range(range):Borders(10):ThemeColor = 1.
    ex:Range(range):Borders(7):TintAndShade = TintAndShade.
    ex:Range(range):Borders(8):TintAndShade = TintAndShade.
    ex:Range(range):Borders(9):TintAndShade = TintAndShade.
    ex:Range(range):Borders(10):TintAndShade = TintAndShade.
END PROCEDURE.


PROCEDURE borda-fina:
    DEF INPUT PARAM range AS CHAR NO-UNDO.
    RUN borda-fina-externa(range).
    ex:Range(range):Borders(11):Weight = 2.
    ex:Range(range):Borders(12):Weight = 2.
END PROCEDURE.

PROCEDURE borda-fina-externa:
    DEF INPUT PARAM range AS CHAR NO-UNDO.
    ex:Range(range):Borders(7):Weight = 2.
    ex:Range(range):Borders(8):Weight = 2.
    ex:Range(range):Borders(9):Weight = 2.
    ex:Range(range):Borders(10):Weight = 2.
END PROCEDURE.

PROCEDURE borda-fina-cor:
    DEF INPUT PARAM range AS CHAR NO-UNDO.
    DEF INPUT PARAM TintAndShade AS DECIMAL NO-UNDO.
    RUN borda-fina-externa-cor(range, TintAndShade).
    ex:Range(range):Borders(11):LineStyle = 1.
    ex:Range(range):Borders(12):LineStyle = 1.
    ex:Range(range):Borders(11):Weight = 2.
    ex:Range(range):Borders(12):Weight = 2.
    ex:Range(range):Borders(11):ThemeColor = 1.
    ex:Range(range):Borders(12):ThemeColor = 1.
    ex:Range(range):Borders(11):TintAndShade = TintAndShade.
    ex:Range(range):Borders(12):TintAndShade = TintAndShade.
END PROCEDURE.

PROCEDURE borda-fina-externa-cor:
    DEF INPUT PARAM range AS CHAR NO-UNDO.
    DEF INPUT PARAM TintAndShade AS DECIMAL NO-UNDO.
    ex:Range(range):Borders(7):LineStyle = 1.
    ex:Range(range):Borders(8):LineStyle = 1.
    ex:Range(range):Borders(9):LineStyle = 1.
    ex:Range(range):Borders(10):LineStyle = 1.
    ex:Range(range):Borders(7):Weight = 2.
    ex:Range(range):Borders(8):Weight = 2.
    ex:Range(range):Borders(9):Weight = 2.
    ex:Range(range):Borders(10):Weight = 2.
    ex:Range(range):Borders(7):ThemeColor = 1.
    ex:Range(range):Borders(8):ThemeColor = 1.
    ex:Range(range):Borders(9):ThemeColor = 1.
    ex:Range(range):Borders(10):ThemeColor = 1.
    ex:Range(range):Borders(7):TintAndShade = TintAndShade.
    ex:Range(range):Borders(8):TintAndShade = TintAndShade.
    ex:Range(range):Borders(9):TintAndShade = TintAndShade.
    ex:Range(range):Borders(10):TintAndShade = TintAndShade.
END PROCEDURE.

PROCEDURE merge-cells:
    DEF INPUT PARAM range AS CHAR NO-UNDO.
    ex:Range(range):Merge.
    RUN center-content(range).    
    ex:Range(range):FONT:Bold = TRUE.
END PROCEDURE.

PROCEDURE center-content:
    DEF INPUT PARAM range AS CHAR NO-UNDO.
    ex:Range(range):HorizontalAlignment = -4108. /* xlCenter*/
    ex:Range(range):VerticalAlignment = -4108. /* xlCenter*/
    ex:Range(range):WrapText = TRUE.
    ex:Range(range):ShrinkToFit = TRUE.
END PROCEDURE.

PROCEDURE configura-pagina-fit-paisagem:
    ex:PrintCommunication = FALSE NO-ERROR.
    ex:ActiveSheet:PageSetup:Zoom = FALSE.
    ex:ActiveSheet:PageSetup:LeftMargin = 18. /*Application.InchesToPoints(0.25)*/
    ex:ActiveSheet:PageSetup:RightMargin = 18. 
    ex:ActiveSheet:PageSetup:TopMargin = 54.   /*Application.InchesToPoints(0.75)*/
    ex:ActiveSheet:PageSetup:BottomMargin = 54. 
    ex:ActiveSheet:PageSetup:HeaderMargin = 21.6.  /*Application.InchesToPoints(0.3)*/
    ex:ActiveSheet:PageSetup:FooterMargin = 21.6.  
    ex:ActiveSheet:PageSetup:Orientation = 2. /* xlLandscape */
    ex:ActiveSheet:PageSetup:FitToPagesWide = 1.
    ex:ActiveSheet:PageSetup:FitToPagesTall = 1.
    ex:PrintCommunication = TRUE NO-ERROR.
END PROCEDURE.

PROCEDURE configura-pagina-fit-retrato-largura:
    ex:PrintCommunication = FALSE NO-ERROR.
    ex:ActiveSheet:PageSetup:Zoom = FALSE.
    ex:ActiveSheet:PageSetup:LeftMargin = 18. /*Application.InchesToPoints(0.25)*/
    ex:ActiveSheet:PageSetup:RightMargin = 18. 
    ex:ActiveSheet:PageSetup:TopMargin = 54.   /*Application.InchesToPoints(0.75)*/
    ex:ActiveSheet:PageSetup:BottomMargin = 54. 
    ex:ActiveSheet:PageSetup:HeaderMargin = 21.6.  /*Application.InchesToPoints(0.3)*/
    ex:ActiveSheet:PageSetup:FooterMargin = 21.6.  
    ex:ActiveSheet:PageSetup:Orientation = 1. /* retrato */
    ex:ActiveSheet:PageSetup:FitToPagesWide = 1.
    ex:ActiveSheet:PageSetup:FitToPagesTall = 32767.
    ex:PrintCommunication = TRUE NO-ERROR.
END PROCEDURE.

&ENDIF
