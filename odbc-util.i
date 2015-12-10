{cdp/cd0666.i}

&SCOPED-DEFINE ODBC_ADD_DSN 1
&SCOPED-DEFINE ODBC_CONFIG_DSN 2
&SCOPED-DEFINE ODBC_REMOVE_DSN 3

&SCOPED-DEFINE MYSQL-DRIVER-NAME "MySQL ODBC 5.1 Driver"
&SCOPED-DEFINE MSSQL-DRIVER-NAME "SQL Server"
&SCOPED-DEFINE MSACCESS-DRIVER-NAME "Microsoft Access driver (*.mdb)"
&SCOPED-DEFINE PROGRESS-DRIVER-NAME "Progress OpenEdge 10.1A driver"

DEF TEMP-TABLE tt-drivers-odbc NO-UNDO
    FIELD driver-name AS CHAR FORMAT "x(70)" 
                              LABEL "ODBC Driver Name"
                              COLUMN-LABEL "ODBC!Driver Name".

FUNCTION create-datasource RETURNS INTEGER 
    (request-type AS INTEGER,
     driverName AS CHAR,
     c-attributes AS CHAR):

    DEF VAR attributes AS MEMPTR NO-UNDO.
    DEF VAR drv-name AS MEMPTR NO-UNDO.
    DEF VAR ret AS INTEGER NO-UNDO.

    /* força terminador de string do C, caso não tenha sido adicionado. */
    ASSIGN driverName   = driverName + CHR(0)
           c-attributes = c-attributes + CHR(0).
    
    SET-SIZE(attributes) = LENGTH(c-attributes).
    PUT-STRING(attributes, 1, LENGTH(c-attributes)) = c-attributes.

    SET-SIZE(drv-name) = LENGTH(driverName).
    PUT-STRING(drv-name, 1, LENGTH(driverName)) = driverName.
    
    RUN SQLConfigDataSource( INPUT 0, 
                             INPUT request-type, 
                             INPUT drv-name, 
                             INPUT attributes,
                             OUTPUT ret).
    RETURN ret.
END FUNCTION.

FUNCTION add-odbc RETURNS LOGICAL (driverName AS CHAR,
                                   c-attributes AS CHAR):
    RETURN create-datasource( {&ODBC_ADD_DSN} , driverName, c-attributes) = 1.
END FUNCTION.


FUNCTION remove-odbc RETURNS LOGICAL (driverName AS CHAR,
                                      dsn AS CHAR):

    DEF VAR c-attributes AS CHAR NO-UNDO.

    ASSIGN c-attributes = "DSN=" + dsn.

    RETURN create-datasource ({&ODBC_REMOVE_DSN}, driverName, c-attributes) = 1.
END FUNCTION.

FUNCTION existe-odbc RETURNS LOGICAL (dsn AS CHAR):
    DEF VAR ret AS INTEGER NO-UNDO.
    RUN SQLGetPrivateProfileString(INPUT "ODBC Data Sources", 
                                   INPUT dsn, 
                                   INPUT "", 
                                   INPUT "", 
                                   INPUT 200, 
                                   INPUT "odbc.ini",
                                   OUTPUT ret).
    
    RETURN ret <> 0.
END FUNCTION.

PROCEDURE ja-existe-odbc:
    DEF INPUT PARAM dsn AS CHAR NO-UNDO.
    IF existe-odbc(dsn) THEN
        RETURN "OK".
    ELSE
        RETURN "NOK".
END PROCEDURE.

FUNCTION create-odbc-attributes RETURNS CHAR (dsn AS CHAR,
                                               descr AS CHAR,
                                               host AS CHAR,
                                               db AS CHAR,
                                               uid AS CHAR,
                                               pwd AS CHAR):
    DEF VAR c-attributes AS CHAR NO-UNDO
        INIT "DSN=$dsn;Description=$descr;Server=$host;Database=$db;Uid=$uid;pwd=$pwd".
    ASSIGN c-attributes = REPLACE(c-attributes, "$dsn", dsn)
           c-attributes = REPLACE(c-attributes, "$descr", descr)
           c-attributes = REPLACE(c-attributes, "$host", host)
           c-attributes = REPLACE(c-attributes, "$db", db)
           c-attributes = REPLACE(c-attributes, "$uid", uid)
           c-attributes = REPLACE(c-attributes, "$pwd", pwd)
           .
    RETURN c-attributes.
END FUNCTION.

PROCEDURE get-drivers:
    DEF OUTPUT PARAM TABLE FOR tt-drivers-odbc.

    DEF VAR driver-name AS CHAR NO-UNDO.

    DEF VAR lpszBuf   AS MEMPTR NO-UNDO.
    DEF VAR cbBufMax  AS INTEGER NO-UNDO INIT 5000.
    DEF VAR pcbBufOut AS INTEGER NO-UNDO.
    
    DEF VAR ret   AS INTEGER NO-UNDO.
    DEF VAR c AS INTEGER NO-UNDO INIT 1.
    
    SET-SIZE(lpszBuf) = cbBufMax.
    
    RUN SQLGetInstalledDrivers(OUTPUT lpszBuf,
                               INPUT  cbBufMax,
                               OUTPUT pcbBufOut,
                               OUTPUT ret).
    
    DO WHILE c <= pcbBufOut:
    
        ASSIGN driver-name = GET-STRING(lpszBuf, c)
               c = c + LENGTH(driver-name) + 1.
    
        CREATE tt-drivers-odbc.
        ASSIGN tt-drivers-odbc.driver-name = driver-name.
    END.
    SET-SIZE(lpszBuf) = 0.

    RETURN STRING(ret).
END PROCEDURE.

PROCEDURE add-mysql-odbc:
    DEF INPUT PARAM dsn AS CHAR NO-UNDO.
    DEF INPUT PARAM descr AS CHAR NO-UNDO.
    DEF INPUT PARAM host AS CHAR NO-UNDO.
    DEF INPUT PARAM db AS CHAR NO-UNDO.
    DEF INPUT PARAM uid AS CHAR NO-UNDO.
    DEF INPUT PARAM pwd AS CHAR NO-UNDO.
    
    IF add-odbc({&MYSQL-DRIVER-NAME}, create-odbc-attributes(dsn, descr, host, db, uid, pwd)) THEN
        RETURN "OK".
    ELSE
        RETURN "NOK".
END PROCEDURE.

PROCEDURE add-any-odbc:
    DEF INPUT PARAM driver-name AS CHAR NO-UNDO.
    DEF INPUT PARAM dsn AS CHAR NO-UNDO.
    DEF INPUT PARAM descr AS CHAR NO-UNDO.
    DEF INPUT PARAM host AS CHAR NO-UNDO.
    DEF INPUT PARAM db AS CHAR NO-UNDO.
    DEF INPUT PARAM uid AS CHAR NO-UNDO.
    DEF INPUT PARAM pwd AS CHAR NO-UNDO.
    
    IF add-odbc(driver-name, create-odbc-attributes(dsn, descr, host, db, uid, pwd)) THEN
        RETURN "OK".
    ELSE
        RETURN "NOK".
END PROCEDURE.

PROCEDURE del-mysql-odbc:
    DEF INPUT PARAM dsn AS CHAR NO-UNDO.

    IF remove-odbc({&MYSQL-DRIVER-NAME}, dsn) THEN
        RETURN "OK".
    ELSE
        RETURN "NOK".

END PROCEDURE.

PROCEDURE get-odbc-error:
    /*DEF OUTPUT PARAM TABLE FOR tt-erro.*/

    DEF VAR iError AS INTEGER NO-UNDO.
    DEF VAR ErrorCode AS INTEGER NO-UNDO.
    DEF VAR ErrorMsg AS MEMPTR NO-UNDO.
    DEF VAR lenErrorMsg AS INTEGER NO-UNDO.
    DEF VAR ErrorMsgMax AS INTEGER NO-UNDO INIT 512.
    DEF VAR res AS INTEGER NO-UNDO.

    DEF VAR c-errorMsg AS CHAR NO-UNDO.

    loooop:
    DO iError = 1 TO 8:
        
        SET-SIZE(ErrorMsg) = ErrorMsgMax.
        RUN SQLInstallerError(iError,             /* 1 to 8 */
                              OUTPUT ErrorCode,     
                              OUTPUT ErrorMsg,  
                              INPUT  ErrorMsgMax, /* max leng error buffer */
                              OUTPUT lenErrorMsg, /* total num of bytes in msg */
                              OUTPUT res). 

        ASSIGN c-errorMsg = GET-STRING(ErrorMsg, 1, lenErrorMsg).

        IF res <> 0 OR c-errorMsg = "" THEN DO:
            SET-SIZE(ErrorMsg) = 0.
            LEAVE loooop.
        END.
        
        CREATE tt-erro.
        ASSIGN tt-erro.i-sequen = iError
               tt-erro.cd-erro  = ErrorCode
               tt-erro.mensagem = c-errorMsg.
        
        SET-SIZE(ErrorMsg) = 0.
    END.
END PROCEDURE.

PROCEDURE SQLConfigDataSource EXTERNAL "ODBCCP32.dll" PERSISTENT:
    DEFINE INPUT PARAM hwndParent AS LONG.
    DEFINE INPUT PARAM fRequest AS SHORT.
    DEFINE INPUT PARAM lpszDriver AS MEMPTR.
    DEFINE INPUT PARAM lpszc-attributes AS MEMPTR.
    DEFINE RETURN PARAM res AS LONG.
END PROCEDURE.

PROCEDURE SQLGetPrivateProfileString EXTERNAL "ODBCCP32.dll" PERSISTENT:
    DEFINE INPUT PARAM lpszSection AS CHAR.
    DEFINE INPUT PARAM lpszEntry AS CHAR.
    DEFINE INPUT PARAM lpszDefault AS CHAR.
    DEFINE INPUT PARAM RetBuffer AS CHAR.
    DEFINE INPUT PARAM cbRetBuffer AS SHORT.
    DEFINE INPUT PARAM lpszFilename AS CHAR.
    DEFINE RETURN PARAM res AS LONG.
END PROCEDURE.

PROCEDURE SQLGetInstalledDrivers EXTERNAL "ODBCCP32.dll" PERSISTENT:
    DEFINE OUTPUT PARAM lpszBuf AS MEMPTR.
    DEFINE INPUT PARAM cbBufMax AS UNSIGNED-SHORT.
    DEFINE OUTPUT PARAM pcbBufOut AS UNSIGNED-SHORT.
    DEFINE RETURN PARAM res AS LONG.
END PROCEDURE.

PROCEDURE SQLInstallerError EXTERNAL "ODBCCP32.dll" PERSISTENT:
    DEFINE INPUT  PARAM iError        AS UNSIGNED-SHORT.
    DEFINE OUTPUT PARAM pfErrorCode   AS SHORT.
    DEFINE OUTPUT PARAM lpszErrorMsg  AS MEMPTR.
    DEFINE INPUT  PARAM cbErrorMsgMax AS SHORT.
    DEFINE OUTPUT PARAM pcbErrorMsg   AS SHORT.
    DEFINE RETURN PARAM res AS SHORT.
END PROCEDURE.
