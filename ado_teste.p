DEFINE VARIABLE rec AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE db  AS COM-HANDLE NO-UNDO.

DEFINE VARIABLE query-sql  AS CHARACTER NO-UNDO.

DEFINE VARIABLE ODBC-STATUS AS CHARACTER NO-UNDO.

DEFINE VARIABLE ODBC-RECCOUNT AS INTEGER NO-UNDO.
DEFINE VARIABLE ODBC-CURSOR   AS INTEGER NO-UNDO.
DEFINE VARIABLE ODBC-NULL AS CHARACTER NO-UNDO.


FUNCTION getConnection RETURNS COM-HANDLE ( dsn AS CHAR, host AS CHAR, username AS CHAR, password AS CHAR):

    DEF VAR con AS COM-HANDLE NO-UNDO.
    DEF VAR constr AS CHAR NO-UNDO.
    ASSIGN constr = "data source=" + dsn + ";server=" + host.

    CREATE "ADODB.Connection" con.

    con:OPEN ( constr, username, password, 0 ) NO-ERROR.

    ODBC-STATUS = "".
    If ( ERROR-STATUS:NUM-MESSAGES > 0 ) THEN
        ODBC-STATUS = "Error: Could not establish connection.".
    ELSE DO:
        con:CursorLocation = 3. /* adUseClient */
    END.
        
    RETURN con.

END FUNCTION.

FUNCTION getRecordSet RETURNS COM-HANDLE( con AS COM-HANDLE, query-string AS CHAR ):

    DEF VAR cmd AS COM-HANDLE NO-UNDO.
    DEF VAR rs  AS COM-HANDLE NO-UNDO.
    DEF VAR records-affected AS INTEGER NO-UNDO.

    CREATE "ADODB.RecordSet" rs.
    CREATE "ADODB.Command" cmd.

    ASSIGN  cmd:ActiveConnection = con
            cmd:CommandText = query-string
            cmd:CommandType = 1 /* adCmdText */
            rs:CursorType = 3 /* adOpenStatic */
            rs:LockType = 1 /*adLockReadOnly*/
            .

    ASSIGN rs = cmd:EXECUTE ( OUTPUT records-affected, "", 32 ) NO-ERROR.
    
    If ( ERROR-STATUS:NUM-MESSAGES > 0 ) THEN
        ODBC-STATUS = "Error: Could not execute query : " + query-string.
    ELSE DO:
        ODBC-STATUS = "".
        IF records-affected > 0 THEN rs:MoveFirst NO-ERROR.
    END.

    RELEASE OBJECT cmd NO-ERROR.
    ASSIGN cmd = ?.

    RETURN rs.
END FUNCTION.

db = getConnection("forn_db", "localhost", "ccs", "ccstec").

If  ODBC-STATUS = "" THEN DO:

    query-sql = "SELECT * FROM fornecedor WHERE codfor=1000124;".

    rec = getRecordSet(db, query-sql).
    DISP ODBC-STATUS rec=?.
    IF ODBC-STATUS = "" THEN DO:
        loooop:
        DO WHILE NOT rec:EOF :

            DISP rec:FIELDS("codfor"):VALUE
                 rec:FIELDS("nome"):VALUE FORMAT "x(40)"
                WITH FRAME a STREAM-IO DOWN.
            DOWN 1 WITH FRAME a.

            rec:MoveNext NO-ERROR.
        END.
    END.

    
    db:CLOSE NO-ERROR.
END.


RELEASE OBJECT db NO-ERROR.
RELEASE OBJECT rec NO-ERROR.

ASSIGN db = ?       
       rec = ?.


/* 
DEFINE VARIABLE ObjRecordSet  AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE ObjConnection AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE ObjCommand    AS COM-HANDLE NO-UNDO.

DEFINE VARIABLE ODBC-DSN    AS CHARACTER NO-UNDO.
DEFINE VARIABLE ODBC-SERVER AS CHARACTER NO-UNDO.
DEFINE VARIABLE ODBC-USERID AS CHARACTER NO-UNDO.
DEFINE VARIABLE ODBC-PASSWD AS CHARACTER NO-UNDO.
DEFINE VARIABLE ODBC-QUERY  AS CHARACTER NO-UNDO.
DEFINE VARIABLE ODBC-STATUS AS CHARACTER NO-UNDO.
DEFINE VARIABLE ODBC-RECCOUNT AS INTEGER NO-UNDO.
DEFINE VARIABLE ODBC-CURSOR   AS INTEGER NO-UNDO.
DEFINE VARIABLE ODBC-NULL AS CHARACTER NO-UNDO.

CREATE "ADODB.Connection" ObjConnection.    /* Create the connection object for the link to Oracle */
CREATE "ADODB.RecordSet" ObjRecordSet.      /* Create a recordset object ready to return the data */
CREATE "ADODB.Command" ObjCommand.          /* Create a command object for sending the SQL statement */

ObjConnection:OPEN ( "data source=" + ODBC-DSN + ";server=" + ODBC-SERVER, ODBC-USERID, ODBC-PASSWD, 0 ) NO-ERROR.

    ODBC-QUERY = "SELECT * FROM fornecedor".

    ASSIGN  ObjCommand:ActiveConnection = ObjConnection
            ObjCommand:CommandText = ODBC-QUERY
            ObjCommand:CommandType = 1 /* adCmdText */
            ObjConnection:CursorLocation = 3 /* adUseClient */
            ObjRecordSet:CursorType = 3 /* adOpenStatic */
            ObjRecordSet = ObjCommand:EXECUTE ( OUTPUT ODBC-NULL, "", 32 )
            ODBC-RECCOUNT = ObjRecordSet:RecordCount.

    /*DISP ODBC-NULL ODBC-RECCOUNT. são iguais*/

    IF ( ODBC-RECCOUNT > 0 ) AND NOT ( ODBC-RECCOUNT = ? ) THEN DO:
    
        ObjRecordSet:MoveFirst NO-ERROR.
       
        DO WHILE ODBC-CURSOR < ODBC-RECCOUNT:
    
        
            DISP ObjRecordSet:FIELDS("codfor"):VALUE
                 ObjRecordSet:FIELDS("nome"):VALUE FORMAT "x(40)"
                WITH FRAME a STREAM-IO DOWN.
            DOWN 1 WITH FRAME a.
    
            ASSIGN ODBC-CURSOR = ODBC-CURSOR + 1.
            ObjRecordSet:MoveNext NO-ERROR.
    
        END.
    END. ELSE DO:
        ASSIGN ODBC-STATUS = "No records found.".
    END.
    
    ObjConnection:CLOSE NO-ERROR.


RELEASE OBJECT ObjConnection NO-ERROR.
RELEASE OBJECT ObjCommand NO-ERROR.
RELEASE OBJECT ObjRecordSet NO-ERROR.

ASSIGN ObjConnection = ?
       ObjCommand = ?
       ObjRecordSet = ?.
*/
