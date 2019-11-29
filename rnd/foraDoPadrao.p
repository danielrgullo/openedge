{include/i-freeac.i}

DEFINE NEW SHARED STREAM arqcatnf.

DEF BUFFER notafiscal FOR nota-fiscal.


PROCEDURE geraCancelamentoEmail:
    DEF INPUT PARAM cod-estabel LIKE nota-fiscal.cod-estabel NO-UNDO.
    DEF INPUT PARAM nr-nota     LIKE nota-fiscal.nr-nota-fis NO-UNDO.
    DEF INPUT PARAM serie       LIKE nota-fiscal.serie       NO-UNDO.

    DEF VAR ctrl    AS INTEGER NO-UNDO.
    DEF VAR nlinhas AS INTEGER NO-UNDO.
    DEF VAR arq     AS CHAR NO-UNDO.
    DEF VAR cli   AS INTEGER NO-UNDO.
    DEF VAR ccs   AS CHAR NO-UNDO.
    DEF VAR cgc   AS CHAR NO-UNDO.
    DEF VAR pasta AS CHAR NO-UNDO.
    DEF VAR nome-abrev AS CHAR NO-UNDO.

    DEF VAR c-serie AS CHAR NO-UNDO.

    ASSIGN c-serie = serie.
    IF c-serie = "" THEN c-serie = "0".


    FOR EACH nota-fiscal NO-LOCK 
            WHERE nota-fiscal.cod-estabel = cod-estabel 
              AND nota-fiscal.nr-nota-fis = nr-nota  
              AND nota-fiscal.serie       = serie :
        
        IF nota-fiscal.dt-cancel  = ? THEN NEXT.
        
        ASSIGN cli = nota-fiscal.cod-emitente.
        IF cli = 10000158 THEN 
            cli = 10000159.

        FIND FIRST emitente NO-LOCK WHERE emitente.cod-emitente = cli NO-ERROR.
        IF NOT AVAIL emitente THEN LEAVE.

        ASSIGN cgc        = emitente.cgc
               nome-abrev = SUBSTR(emitente.nome-emit,1,25).

        ccs = "Q9994G0 ".
        IF cli = 10002155 then
            ccs = "00034857".
         IF cli = 10009063 THEN 
            ccs = "00008499".

        IF cli = 10000124 OR cli = 10005825 THEN
            pasta = "\\192.0.0.149\ediccs$\AV\".
        ELSE IF cli = 10002395 OR cli = 10005276 OR cli = 10005705 OR cli = 10005725 THEN        
            pasta = "\\\ediccs$\sintel\snd\".
        ELSE
            pasta = "\\192.0.0.149\ediccs$\EDI\".

        arq = pasta + cgc + "_CNF_EMAIL_00964350000178_" +
              STRING(YEAR(TODAY)) + STRING(MONTH(TODAY),"99") + STRING(DAY(TODAY),"99") +
              REPLACE(STRING(TIME,"hh:mm:ss"),":","") + ".TXT".

        FILE-INFO:FILE-NAME = arq.

        DO WHILE FILE-INFO:PATHNAME <> ?:

            arq = pasta + cgc + "_CNF_EMAIL_00964350000178_" +
                  STRING(YEAR(TODAY)) + STRING(MONTH(TODAY),"99") + STRING(DAY(TODAY),"99") +
                  STRING(INTEGER(REPLACE(STRING(TIME,"hh:mm:ss"),":","")) + 1, "999999")
                  + ".TXT".

            FILE-INFO:FILE-NAME = arq.
        END.

        OUTPUT STREAM arqcatnf TO VALUE(arq).

        PUT STREAM arqcatnf UNFORMATTED
                "Cancelamento de Nota fiscal CCS TENOLOGIA INDUSTRIAL " SKIP.
    
        PUT STREAM arqcatnf UNFORMATTED
                "Numero NF   : " STRING(INT(nota-fiscal.nr-nota-fis), "999999") SKIP
                "Serie       : " c-serie FORMAT "x(4)"    SKIP
                "Data NF     : " nota-fiscal.dt-emis-nota SKIP
                "Data Cancel.: " nota-fiscal.dt-cancela SKIP
                SKIP.
    
        OUTPUT STREAM arqcatnf CLOSE.

        FIND FIRST edi-ccs WHERE edi-ccs.nr-nota-fis  = nota-fiscal.nr-nota-fis
                             AND edi-ccs.serie        = nota-fiscal.serie NO-ERROR.
        IF NOT AVAIL edi-ccs THEN DO:
            /* teoricamente isso nunca vai acontecer */
            CREATE edi-ccs.
            ASSIGN edi-ccs.nr-nota-fis  = nota-fiscal.nr-nota-fis
                   edi-ccs.serie        = nota-fiscal.serie
                   edi-ccs.dt-emis-nota = nota-fiscal.dt-emis-nota
                   edi-ccs.hora         = STRING(TIME, "HH:MM").
        END.
        ASSIGN edi-ccs.dt-cancel = nota-fiscal.dt-cancel.
    END.
END PROCEDURE.

/**********************/
PROCEDURE gera_EDI_HTML:
    DEF INPUT PARAM cod-estabel LIKE nota-fiscal.cod-estabel NO-UNDO.
    DEF INPUT PARAM nr-nota     LIKE nota-fiscal.nr-nota-fis NO-UNDO.
    DEF INPUT PARAM serie       LIKE nota-fiscal.serie NO-UNDO.

    DEF VAR cli LIKE emitente.cod-emitente NO-UNDO.

    DEF VAR ccs AS CHAR NO-UNDO.

    DEF VAR dtvenc  AS CHAR FORMAT "x(6)" INITIAL "AAMMDD" NO-UNDO.
    DEF VAR it-cliente   AS CHAR NO-UNDO.
    DEF VAR ponbr   AS CHAR NO-UNDO.
    DEF VAR ctrl    AS INTEGER NO-UNDO.
    DEF VAR arq     AS CHAR NO-UNDO.

    DEF VAR cgc   AS CHAR NO-UNDO.
    DEF VAR pasta AS CHAR NO-UNDO.

    DEF VAR nome-abrev AS CHAR NO-UNDO.

    DEF VAR c-serie AS CHAR NO-UNDO.

    ASSIGN c-serie = serie.
    IF c-serie = "" THEN c-serie = "0".

    FIND FIRST nota-fiscal NO-LOCK WHERE nota-fiscal.cod-estabel = cod-estabel 
                                     AND nota-fiscal.nr-nota-fis = nr-nota  
                                     AND nota-fiscal.serie       = serie NO-ERROR.
    IF NOT AVAIL nota-fiscal THEN
        LEAVE.
    ELSE
        cli = nota-fiscal.cod-emitente.

    IF cli = 10000158 THEN 
        cli = 10000159.

    FIND FIRST emitente NO-LOCK WHERE emitente.cod-emitente = cli NO-ERROR.
    IF NOT AVAIL emitente THEN LEAVE.

    ASSIGN cgc = emitente.cgc
           nome-abrev = SUBSTR(emitente.nome-emit,1,25).

    pasta = "\\192.0.0.149\ediccs$\EDI\".

    arq = pasta + cgc + "_EMAIL_00964350000178_" +
          STRING(YEAR(TODAY)) + STRING(MONTH(TODAY),"99") + STRING(DAY(TODAY),"99") +
          REPLACE(STRING(TIME,"hh:mm:ss"),":","") + ".htm".

    FILE-INFO:FILE-NAME = arq.
    DO WHILE FILE-INFO:PATHNAME <> ?:
        arq = pasta + cgc + "_EMAIL_00964350000178_" +
              STRING(YEAR(TODAY)) + STRING(MONTH(TODAY),"99") + STRING(DAY(TODAY),"99") +
              STRING(INTEGER(REPLACE(STRING(TIME,"hh:mm:ss"),":","")) + 1, "999999")
              + ".htm".
        FILE-INFO:FILE-NAME = arq.
    END.
    
    OUTPUT STREAM arqcatnf TO VALUE(arq).
    
    PUT STREAM arqcatnf UNFORMATTED '<!doctype html>'.
    PUT STREAM arqcatnf UNFORMATTED '<html lang="pt_br">'.
    PUT STREAM arqcatnf UNFORMATTED '<head>'.
    PUT STREAM arqcatnf UNFORMATTED '<title>Itens faturados CCS</title>'.
    PUT STREAM arqcatnf UNFORMATTED '</head>'.
    PUT STREAM arqcatnf UNFORMATTED '<body>'.
    PUT STREAM arqcatnf UNFORMATTED '<h2>CCS Tecnologia e Servi&ccedil;os S.A.</h2>'.
    PUT STREAM arqcatnf UNFORMATTED '<br /><br />'.
    PUT STREAM arqcatnf UNFORMATTED 'Itens enviados em : <b>'  TODAY '</b>'.
    PUT STREAM arqcatnf UNFORMATTED '<br /><br />'.

    FOR EACH nota-fiscal NO-LOCK WHERE nota-fiscal.cod-estabel = cod-estabel 
                                   AND nota-fiscal.nr-nota-fis = nr-nota  
                                   AND nota-fiscal.serie       = serie :
        
        PUT STREAM arqcatnf UNFORMATTED 'Nota Fiscal/Serie : <b>'  nota-fiscal.nr-nota-fis  '/'  c-serie '</b>'.
        PUT STREAM arqcatnf UNFORMATTED '<br /><br />'.
        PUT STREAM arqcatnf UNFORMATTED 'Data emiss&atilde;o : <b>'  nota-fiscal.dt-emis-nota '</b>'.
        PUT STREAM arqcatnf UNFORMATTED '<br /><br />'.
        PUT STREAM arqcatnf UNFORMATTED '<table border="1" width="100%">'.
        PUT STREAM arqcatnf UNFORMATTED '<tr>'.
        PUT STREAM arqcatnf UNFORMATTED '<th>Item CCS</th>'.
        PUT STREAM arqcatnf UNFORMATTED '<th>Item Cliente</th>'.
        PUT STREAM arqcatnf UNFORMATTED '<th>Pedido</th>'.
        PUT STREAM arqcatnf UNFORMATTED '<th>Qtd Faturada</th>'.
        PUT STREAM arqcatnf UNFORMATTED '</tr>'.

        /*FIND FIRST fat-duplic WHERE fat-duplic.cod-estabel = nota-fiscal.cod-estabel 
                                AND fat-duplic.serie       = nota-fiscal.serie  
                                AND fat-duplic.nr-fatura   = nota-fiscal.nr-nota-fis NO-LOCK NO-ERROR.
        IF NOT AVAIL fat-duplic THEN
            dtvenc = "000000".
        ELSE DO:
            dtvenc = STRING(DAY(fat-duplic.dt-venciment),"99") + STRING(MONTH(fat-duplic.dt-venciment),"99") + SUBSTRING(STRING(YEAR(fat-duplic.dt-venciment)),3,2) .
        END.
    
        PUT STREAM arqcatnf UNFORMATTED  
                  " Emissao : "  nota-fiscal.dt-emis-nota 
                  " Vencimento : " dtvenc SKIP.*/
    
       FOR EACH it-nota-fisc NO-LOCK WHERE it-nota-fisc.cod-estabel = nota-fiscal.cod-estabel
                                        AND it-nota-fisc.nr-nota-fis = nota-fiscal.nr-nota-fis
                                        AND it-nota-fisc.serie       = nota-fiscal.serie :
    
            IF it-nota-fisc.it-codigo = "" THEN NEXT.    
            FIND FIRST ITEM OF it-nota-fisc NO-LOCK NO-ERROR.
            IF NOT AVAIL ITEM THEN NEXT.

            ponbr = "".
            FOR FIRST ped-item NO-LOCK WHERE ped-item.nr-pedcli = it-nota-fisc.nr-pedcli ,
                FIRST ped-ent OF ped-item NO-LOCK :
                    ponbr = TRIM(REPLACE(ped-ent.observacao, "OC", "")).
            END.
            IF ponbr = "" THEN
                ponbr = it-nota-fisc.nr-pedcli.
            
            IF cli = 10002155 AND NOT ponbr BEGINS "#" THEN ponbr = "5500008945".
            ASSIGN ponbr = REPLACE(ponbr, "#","").

            PUT STREAM arqcatnf UNFORMATTED '<tr>'.
            PUT STREAM arqcatnf UNFORMATTED '<td>'  it-nota-fisc.it-codigo  '</td>'.
            PUT STREAM arqcatnf UNFORMATTED '<td>'  ITEM.codigo-refer  '</td>'.
            PUT STREAM arqcatnf UNFORMATTED '<td>'  ponbr  '</td>'.
            PUT STREAM arqcatnf UNFORMATTED '<td>'  it-nota-fisc.qt-faturada[1]  '</td>'.
            PUT STREAM arqcatnf UNFORMATTED '</tr>'.

            /*PUT STREAM arqcatnf UNFORMATTED  
                     " Item : " ITEM.codigo-refer " Item CCS : " it-nota-fisc.it-codigo SKIP
                     " Pedido : " UPPER(ponbr) SKIP
                     " Qtd : " it-nota-fisc.qt-faturada[1] " Valor Unit. : " it-nota-fisc.vl-preuni SKIP.*/            

       END.

       PUT STREAM arqcatnf UNFORMATTED '</table>'.
       PUT STREAM arqcatnf UNFORMATTED '<br /><br />'.
       PUT STREAM arqcatnf UNFORMATTED '</body>'.
       PUT STREAM arqcatnf UNFORMATTED '</html>'.

       /* realmente grava aqui ? */
       FIND FIRST edi-ccs NO-LOCK WHERE edi-ccs.nr-nota-fis  = nota-fiscal.nr-nota-fis
                                    AND edi-ccs.serie        = nota-fiscal.serie NO-ERROR.
        IF NOT AVAIL edi-ccs THEN DO:
            CREATE edi-ccs.
            ASSIGN edi-ccs.nr-nota-fis  = nota-fiscal.nr-nota-fis
                   edi-ccs.serie        = nota-fiscal.serie
                   edi-ccs.dt-emis-nota = nota-fiscal.dt-emis-nota
                   edi-ccs.hora         = STRING(TIME, "HH:MM").
        END.

    END. /* nota-fiscal */        

    OUTPUT STREAM arqcatnf CLOSE.

END PROCEDURE.
