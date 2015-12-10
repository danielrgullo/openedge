{windows.i}

&SCOPED-DEFINE  REG_SZ 1
&SCOPED-DEFINE  REG_MULTI_SZ  7

PROCEDURE RegOpenKey{&A} EXTERNAL {&ADVAPI} :
  DEFINE INPUT  PARAMETER hkey       AS LONG.
  DEFINE INPUT  PARAMETER lpszSubKey AS CHAR.
  DEFINE OUTPUT PARAMETER phkResult  AS LONG.
  DEFINE RETURN PARAMETER lpResult   AS LONG.
END PROCEDURE.

PROCEDURE RegCloseKey EXTERNAL {&ADVAPI} :
  DEFINE INPUT  PARAMETER hkey     AS LONG.
  DEFINE RETURN PARAMETER lpresult AS LONG.
END PROCEDURE.

PROCEDURE RegCreateKey{&A} EXTERNAL {&ADVAPI} :
  DEFINE INPUT  PARAMETER hkey      AS LONG.
  DEFINE INPUT  PARAMETER lpSubKey  AS CHAR.
  DEFINE OUTPUT PARAMETER phkResult AS LONG.
  DEFINE RETURN PARAMETER lpresult  AS LONG.
END PROCEDURE.

PROCEDURE RegSetValueEx{&A} EXTERNAL {&ADVAPI} :
  DEFINE INPUT  PARAMETER hkey        AS LONG.
  DEFINE INPUT  PARAMETER lpValueName AS CHAR.
  DEFINE INPUT  PARAMETER Reserved    AS LONG.
  DEFINE INPUT  PARAMETER dwType      AS LONG.
  DEFINE INPUT  PARAMETER lpData      AS MEMPTR.
  DEFINE INPUT  PARAMETER cbData      AS LONG.
  DEFINE RETURN PARAMETER lpresult    AS LONG.
END PROCEDURE.


PROCEDURE add-mssql-odbc:
    DEF INPUT PARAM dsn AS CHAR NO-UNDO.
    DEF INPUT PARAM descr AS CHAR NO-UNDO.
    DEF INPUT PARAM host AS CHAR NO-UNDO.
    DEF INPUT PARAM db AS CHAR NO-UNDO.
    DEF INPUT PARAM trusted-connection AS LOGICAL NO-UNDO.
    
    DEF VAR ret AS INTEGER NO-UNDO.
    DEF VAR k   AS INTEGER NO-UNDO.
    DEF VAR k2   AS INTEGER NO-UNDO.
    
    RUN RegOpenKey{&A}( {&HKEY_CURRENT_USER},
                     "Software\ODBC\ODBC.INI",
                     OUTPUT k,
                     OUTPUT ret).

    IF ret <> {&ERROR_SUCCESS} THEN
        RETURN "NOK".

    RUN RegCreateKey{&A}(k,
                         dsn,
                         OUTPUT k2,
                         OUTPUT ret).

    IF ret <> {&ERROR_SUCCESS} THEN
        RETURN "NOK".
    
    RUN RegCreateValue(k2, "Database", db).    
    RUN RegCreateValue(k2, "Description", descr).
    RUN RegCreateValue(k2, "Server", host).
    RUN RegCreateValue(k2, "Trusted_Connection", STRING(trusted-connection)).
    RUN RegCreateValue(k2, "Driver", "c:\windows\system32\sqlsrv32.dll").
    
    RUN RegCloseKey( k2, OUTPUT ret).
    RUN RegCloseKey( k, OUTPUT ret).
    
    RUN RegOpenKey{&A}( {&HKEY_CURRENT_USER},
                     "Software\ODBC\ODBC.INI\ODBC Data Sources",
                     OUTPUT k,
                     OUTPUT ret).

    IF ret <> {&ERROR_SUCCESS} THEN
        RETURN "NOK".
    
    RUN RegCreateValue(k, dsn, "SQL Server").
    IF ret <> {&ERROR_SUCCESS} THEN
        RETURN "NOK".
    
    RUN RegCloseKey( k, OUTPUT ret).

    RETURN "OK".
END PROCEDURE.

PROCEDURE RegCreateValue:
    DEF INPUT PARAM hkey  AS INT  NO-UNDO.
    DEF INPUT PARAM chave AS CHAR NO-UNDO.
    DEF INPUT PARAM valor AS CHAR NO-UNDO.

    DEF VAR ret AS INTEGER NO-UNDO.
    DEF VAR p-valor AS MEMPTR NO-UNDO.
    
    SET-SIZE(p-valor) = LENGTH(valor).
    PUT-STRING(p-valor, 1, LENGTH(valor)) = valor.
    
    RUN RegSetValueEx{&A}(hkey,
                          chave,
                          0,
                          {&REG_SZ},
                          p-valor,
                          LENGTH(valor),
                          OUTPUT ret).
    SET-SIZE(p-valor) = 0.
    RETURN STRING(ret).
END PROCEDURE.
