{include/i-freeac.i}

FUNCTION getCodCCS RETURNS CHAR (cod-emitente AS INTEGER) FORWARD.
FUNCTION getQtdEDIEmitente RETURNS INTEGER (cod-emitente AS INTEGER ) FORWARD.
FUNCTION formataData RETURNS CHAR (data AS DATE) FORWARD.
FUNCTION formataDataHoje RETURNS CHAR () FORWARD.
FUNCTION formataHora RETURNS CHAR (hora AS INTEGER) FORWARD.
FUNCTION formataHoraAgora RETURNS CHAR () FORWARD.

FUNCTION getNomeArquivoEDI RETURNS LOGICAL () FORWARD.
FUNCTION getPastaDoCliente RETURNS CHAR () FORWARD.
FUNCTION clienteUsaWidelog RETURNS LOGICAL () FORWARD.
FUNCTION clienteUsaSintel  RETURNS LOGICAL () FORWARD.

DEFINE VARIABLE arquivo AS CLASS fileUtils.

DEF TEMP-TABLE tt-cn16 NO-UNDO
    FIELD nr-nota-fis    LIKE nota-fiscal.nr-nota-fis
    FIELD serie          LIKE nota-fiscal.serie
    FIELD dt-emis-nota   AS CHAR
    FIELD cod-emitente   LIKE emitente.cod-emitente
    FIELD nome-abrev     LIKE emitente.nome-abrev
    FIELD nome-abrev-ccs AS CHAR INIT "CCS TENOLOGIA INDUSTRIAL"
    FIELD cgc            LIKE emitente.cgc
    FIELD cgc-ccs        AS CHAR INIT "00964350000178"
    FIELD cod-ccs        AS CHAR
    FIELD qtdEDIData     AS INTEGER
    FIELD data           AS CHAR
    FIELD hora           AS CHAR
    .

PROCEDURE geraCancelamento:
    DEF INPUT PARAM cod-estabel LIKE nota-fiscal.cod-estabel NO-UNDO.
    DEF INPUT PARAM nr-nota-fis LIKE nota-fiscal.nr-nota-fis NO-UNDO.
    DEF INPUT PARAM serie       LIKE nota-fiscal.serie       NO-UNDO.

    EMPTY TEMP-TABLE tt-cn16.

    FOR EACH nota-fiscal FIELDS (cod-estabel nr-nota-fis serie dt-cancel cod-emitente)
                         NO-LOCK WHERE nota-fiscal.cod-estabel = cod-estabel 
                                   AND nota-fiscal.nr-nota-fis = nr-nota-fis  
                                   AND nota-fiscal.serie       = serie 
                                   AND nota-fiscal.dt-cancel   = ?,
        FIRST emitente FIELDS (cod-emitente nome-abrev cgc) OF nota-fiscal NO-LOCK:
        
        IF serie = "" THEN serie = "0".

        CREATE tt-cn16.
        ASSIGN tt-cn16.nr-nota-fis  = nr-nota-fis
               tt-cn16.serie        = serie
               tt-cn16.dt-emis-nota = formataData(nota-fiscal.dt-emis-nota)
               tt-cn16.cod-emitente = emitente.cod-emitente
               tt-cn16.nome-abrev   = fn-free-accent(emitente.nome-emit)
               tt-cn16.cgc          = emitente.cgc
               tt-cn16.cod-ccs      = getCodCCS(emitente.cod-emitente)
               tt-cn16.qtdEDIData   = getQtdEDIEmitente(emitente.cod-emitente)
               tt-cn16.data         = formataDataHoje()
               tt-cn16.hora         = formataHoraAgora()
               .
    END.

    FIND FIRST tt-cn16 NO-LOCK NO-ERROR.
    IF AVAIL tt-cn16 THEN DO:
        arquivo = NEW FileUtils().
        IF NOT getNomeArquivoEDI() THEN RETURN "NOK".
        arquivo:openStream().
        RUN aponta-ediccs(nr-nota-fis, serie).
        arquivo:closeStream().
    END.
    RETURN "OK".
END PROCEDURE.

PROCEDURE geraCancelamento00:

    DEF VAR linha   AS CHAR NO-UNDO.
    DEF VAR nlinhas AS INTEGER NO-UNDO.

    DEF VAR nome-abrev   AS CHAR NO-UNDO.
    DEF VAR tamanhoNome  AS INTEGER INIT 25 NO-UNDO.
    DEF VAR tamanhoSerie AS INTEGER INIT 4 NO-UNDO.

    FOR EACH tt-cn16 NO-LOCK:
        /*
           ITP
        */
        nome-abrev = SUBSTR(tt-cn16.nome-abrev,1,tamanhoNome).
        linha = "ITP" +
                "01600" +
                STRING(tt-cn16.qtdEDIData, "99999") +
                tt-cn16.data +
                tt-cn16.hora +
                tt-cn16.cgc-ccs +
                tt-cn16.cgc +
                tt-cn16.cod-ccs +
                "        " +
                tt-cn16.nome-abrev-ccs +
                nome-abrev + FILL(" ", tamanhoNome - LENGTH(nome-abrev)) +
                "         "
                .
        arquivo:outPutLine(linha).
        /*
            CN1 
        */
        linha = "CN1" +
                STRING(INT(tt-cn16.nr-nota-fis), "999999") +
                tt-cn16.serie  + FILL(" ", tamanhoSerie - LENGTH(tt-cn16.serie)) +
                FILL(" ", 115)
                .
        arquivo:outPutLine(linha).
        /*
            FTP
        */
        linha = "FTP" +
                STRING(tt-cn16.qtdEDIData, "99999") +
                STRING(arquivo:getNumLinhas(), "999999999") +
                FILL("0", 17) +
                "D" +
                FILL(" ", 93) /* filler */
                .
        arquivo:outPutLine(linha).
    END.
END PROCEDURE.

PROCEDURE geraCancelamento01:
    
    DEF VAR linha   AS CHAR NO-UNDO.
    DEF VAR nlinhas AS INTEGER NO-UNDO.

    DEF VAR nome-abrev   AS CHAR NO-UNDO.
    DEF VAR tamanhoNome  AS INTEGER INIT 25 NO-UNDO.
    DEF VAR tamanhoSerie AS INTEGER INIT 4 NO-UNDO.

    FOR EACH tt-cn16 NO-LOCK:
        /*
           ITP
        */
        nome-abrev = SUBSTR(tt-cn16.nome-abrev,1,tamanhoNome).
        linha = "ITP" +
                "01601" +
                STRING(tt-cn16.qtdEDIData, "99999") +
                tt-cn16.data +
                tt-cn16.hora +
                tt-cn16.cgc-ccs +
                tt-cn16.cgc +
                tt-cn16.cod-ccs +
                "        " +
                tt-cn16.nome-abrev-ccs +
                nome-abrev + FILL(" ", tamanhoNome - LENGTH(nome-abrev)) +
                FILL(" ",9)
                .
        arquivo:outPutLine(linha).
        /*
            CN1 
        */
        linha = "CN1"  +
                STRING(INT(tt-cn16.nr-nota-fis), "999999") +
                tt-cn16.serie  + FILL(" ", tamanhoSerie - LENGTH(tt-cn16.serie)) +
                tt-cn16.dt-emis-nota +
                FILL(" ", 103)
                .
        arquivo:outPutLine(linha).
        /*
            FTP
        */
        linha = "FTP" +
                STRING(tt-cn16.qtdEDIData, "99999") +
                STRING(arquivo:getNumLinhas(), "999999999") +
                FILL("0", 17) +
                "D" +
                FILL(" ", 93) /* filler */
                .
        arquivo:outPutLine(linha).
    END.
END PROCEDURE.


PROCEDURE aponta-ediccs:
    DEF INPUT PARAM nr-nota-fis AS CHAR NO-UNDO.
    DEF INPUT PARAM serie AS CHAR NO-UNDO.
    
    FIND FIRST edi-ccs WHERE edi-ccs.nr-nota-fis  = nr-nota-fis
                         AND edi-ccs.serie    = serie NO-ERROR.
    IF NOT AVAIL edi-ccs THEN DO:
        CREATE edi-ccs.
        ASSIGN edi-ccs.nr-nota-fis  = nr-nota-fis
               edi-ccs.serie        = serie                   
               edi-ccs.hora         = STRING(TIME, "HH:MM")
               edi-ccs.dt-emis-nota = ?.
    END.
    ASSIGN edi-ccs.dt-cancel = TODAY.
END PROCEDURE .

FUNCTION getCodCCS RETURNS CHAR (cod-emitente AS INTEGER):
    DEF VAR cod-ccs AS CHAR NO-UNDO INIT "Q9994G0 ".
    CASE cod-emitente:
        WHEN 10006602 THEN cod-ccs = "Q9994G6 ".
        WHEN 10002155 THEN cod-ccs = "00034857".
    END CASE.
    RETURN cod-ccs.
END FUNCTION.

FUNCTION getQtdEDIEmitente RETURNS INTEGER (cod-emitente AS INTEGER ):
    DEF VAR qtdEDIEmitente AS INTEGER NO-UNDO INIT 1.
    DEF BUFFER notafiscal FOR nota-fiscal.
    FOR EACH notafiscal NO-LOCK WHERE notafiscal.dt-emis-nota = TODAY
                                  AND notafiscal.cod-emitente = cod-emitente:
        ASSIGN qtdEDIEmitente = qtdEDIEmitente + 1.
    END.
    RETURN qtdEDIEmitente.
END FUNCTION.

FUNCTION formataData RETURNS CHAR(data AS DATE):
    RETURN SUBSTRING(STRING(YEAR(data)),3,2) +
           STRING(MONTH(data),"99") +
           STRING(DAY(data),"99") .
END FUNCTION.

FUNCTION formataDataHoje RETURNS CHAR():
    RETURN formataData(TODAY).
END FUNCTION.

FUNCTION formataHora RETURNS CHAR (hora AS INTEGER):
    RETURN REPLACE(STRING(hora,"hh:mm:ss"),":","").
END FUNCTION.

FUNCTION formataHoraAgora RETURNS CHAR ():
    RETURN formataHora(TIME).
END FUNCTION.

FUNCTION getNomeArquivoEDI RETURNS LOGICAL () :
    DEF VAR inFixoNomeArquivo AS CHAR INIT "_RND01600_" NO-UNDO.

    DEF VAR pasta AS CHAR NO-UNDO.
    DEF VAR nomeArquivo AS CHAR NO-UNDO.
    DEF VAR tentativas AS INTEGER NO-UNDO.

    pasta = getPastaDoCliente().
    IF pasta = "" THEN RETURN FALSE.

    DO WHILE arquivo:existeArquivo():

            ASSIGN nomeArquivo = pasta +
                                 tt-cn16.cgc +
                                 inFixoNomeArquivo +
                                 tt-cn16.cgc-ccs +
                                 formataDataHoje() +
                                 formataHora(TIME + tentativas).

            arquivo:setNomeArquivo(nomeArquivo).

            ASSIGN tentativas = tentativas + 1.
    END.
    RETURN TRUE.
END FUNCTION.

FUNCTION getPastaDoCliente RETURNS CHAR ():
    DEF VAR pasta AS CHAR NO-UNDO INIT "\\192.0.0.149\ediccs$\EDI\".

    IF clienteUsaWidelog() THEN
        pasta = "\\192.0.0.149\ediccs$\AV\".
    ELSE IF clienteUsaSintel() THEN
        pasta = "\\192.0.0.149\ediccs$\sintel\snd\".
        
    RETURN pasta.
END FUNCTION.

FUNCTION clienteUsaWidelog RETURNS LOGICAL ():
    RETURN (tt-cn16.cod-emitente = 10000124 OR 
            tt-cn16.cod-emitente = 10005825 OR 
            tt-cn16.cod-emitente = 10006602).
END.

FUNCTION clienteUsaSintel RETURNS LOGICAL ():
    RETURN (tt-cn16.cod-emitente = 10002395 OR 
            tt-cn16.cod-emitente = 10005276 OR 
            tt-cn16.cod-emitente = 10005705 OR 
            tt-cn16.cod-emitente = 10005725).
END.

