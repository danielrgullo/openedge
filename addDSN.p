DEF INPUT PARAM dsn AS CHAR NO-UNDO.
DEF INPUT PARAM hostname AS CHAR NO-UNDO.
DEF INPUT PARAM database-name AS CHAR NO-UNDO.

RUN addODBCDsn (INPUT dsn, 
                INPUT "Banco de dados MySQL", 
                INPUT hostname, 
                INPUT database-name).

PROCEDURE addODBCDsn:
    DEF INPUT PARAM dsn AS CHAR NO-UNDO.
    DEF INPUT PARAM dsn-description AS CHAR NO-UNDO.
    DEF INPUT PARAM hostname AS CHAR NO-UNDO.
    DEF INPUT PARAM database-name AS CHAR NO-UNDO.

    DEF VAR ODBC_ADD_DSN    AS INTEGER NO-UNDO INIT 1.
    DEF VAR ODBC_CONFIG_DSN AS INTEGER NO-UNDO INIT 2.
    DEF VAR ODBC_REMOVE_DSN AS INTEGER NO-UNDO INIT 3 .
    
    DEF VAR driverName AS CHAR NO-UNDO.    
    DEF VAR attributes AS MEMPTR NO-UNDO.
    DEF VAR c-attributes AS CHAR NO-UNDO.
    DEF VAR ret AS INTEGER NO-UNDO.
    
    ASSIGN driverName = "MySQL ODBC 5.1 Driver"
           c-attributes = "DSN=" + dsn + ";Description=" + dsn-description +
                          ";Server=" + hostname + ";Database=" + database-name + ";Uid=ccs;pwd=ccstec".
    
    SET-SIZE(attributes) = LENGTH(c-attributes).
    PUT-STRING(attributes, 1, LENGTH(c-attributes)) = c-attributes.
    
    RUN SQLConfigDataSource( INPUT 0, 
                             INPUT ODBC_ADD_DSN, 
                             INPUT driverName, 
                             INPUT attributes,
                             OUTPUT ret).
    IF ret = 0 THEN DO:
        RUN utp/ut-msgs.p (INPUT "show":U, 
                           INPUT 17006, 
                           INPUT "Erro~~Erro configurando ODBC ! Contate o departamento de TI."
                          ).
        STOP.
    END.
END.

PROCEDURE SQLConfigDataSource EXTERNAL "ODBCCP32.dll" PERSISTENT:
    DEFINE INPUT PARAM hwndParent AS LONG.
    DEFINE INPUT PARAM fRequest AS LONG.
    DEFINE INPUT PARAM lpszDriver AS CHAR.
    DEFINE INPUT PARAM lpszc-attributes AS MEMPTR.
    DEFINE RETURN PARAM res AS LONG.
END PROCEDURE.

