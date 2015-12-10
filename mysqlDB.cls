CLASS ccs.mysql.mysqlDB:

    DEFINE PRIVATE VARIABLE con AS COM-HANDLE NO-UNDO.
    DEFINE PRIVATE VARIABLE cmd AS COM-HANDLE NO-UNDO.
    DEFINE PRIVATE VARIABLE rs  AS COM-HANDLE.
    
    DEFINE PRIVATE VARIABLE dsn AS CHAR NO-UNDO.
    DEFINE PRIVATE VARIABLE host AS CHAR NO-UNDO.
    DEFINE PRIVATE VARIABLE username AS CHAR NO-UNDO.
    DEFINE PRIVATE VARIABLE password AS CHAR NO-UNDO.

    DEFINE PROTECTED VARIABLE conectado AS LOGICAL NO-UNDO.
    DEFINE PUBLIC    VARIABLE str-erro AS CHAR NO-UNDO.

    CONSTRUCTOR PUBLIC mysqlDB():
        ASSIGN con = ?
               dsn = ""
               host = "localhost"
               username = "ccs"
               password = "ccstec"
               conectado = FALSE.
        CREATE "ADODB.Connection" con.
    END CONSTRUCTOR .

    DESTRUCTOR PUBLIC mysqlDB():
        con:CLOSE() NO-ERROR.
        RELEASE OBJECT con NO-ERROR.
        RELEASE OBJECT cmd NO-ERROR.
        ASSIGN con = ?       
               cmd = ?.
    END DESTRUCTOR.

    METHOD PUBLIC LOGICAL connect_db ():
        DEF VAR con-str AS CHAR NO-UNDO.
        ASSIGN con-str = "data source=" + dsn + ";server=" + host.
            
        RELEASE OBJECT con NO-ERROR.
        CREATE "ADODB.Connection" con.

        DEF VAR t AS INTEGER NO-UNDO.
        
        DO t = 1 TO 2:
            con:OPEN ( con-str, username, password, 0 ) NO-ERROR.
            IF ERROR-STATUS:NUM-MESSAGES = 0 THEN DO:
                ASSIGN str-erro = "".
                LEAVE.
            END.

            /* registra odbc/dsn */
            RUN ccs/mysql/addDSN.p (INPUT dsn, host, dsn ) NO-ERROR.

            ASSIGN str-erro = "Error: Could not establish connection.".
        END.

        IF str-erro = "" THEN DO:
            CREATE "ADODB.Command" cmd.
            ASSIGN con:CursorLocation = 3 /* 2 = adUseServer; 3 = adUseClient */
                   cmd:ActiveConnection = con
                   cmd:CommandType = 1 /* adCmdText */
                   cmd:PREPARED = NO
                   conectado = TRUE.

            CREATE "ADODB.RecordSet" rs.
            ASSIGN rs:CursorType = 0 /*0 adOpenForwardOnly ;  3 adOpenStatic */
                   rs:LockType = 3 /*2 adLockPessimistic ; 3 adLockOptimistic*/
                   .
        END.

        RETURN str-erro = "".
    END METHOD.

    METHOD PUBLIC COM-HANDLE getRecordSet ( INPUT query-string AS CHAR ):

        DEF VAR records-affected AS INTEGER NO-UNDO.
    
        IF con:State = 0 THEN connect_db().
        /*IF NOT conectado THEN connect_db().*/

        /*CREATE "ADODB.RecordSet" rs.*/
    
        rs:CLOSE() NO-ERROR.
        ASSIGN cmd:CommandText = query-string
               rs = cmd:EXECUTE ( OUTPUT records-affected, "", 64 ) NO-ERROR.

        ASSIGN str-erro = "".
        If ( ERROR-STATUS:ERROR ) THEN DO:
            str-erro = "Error: Could not execute query : " + query-string.

            /*DEF VAR i AS INTEGER NO-UNDO.
            DO i = 1 TO ERROR-STATUS:NUM-MESSAGES :
                PUT ERROR-STATUS:GET-MESSAGE(i) SKIP.
            END.*/
        END.

        RETURN rs.
    END METHOD.

    METHOD PUBLIC VOID executeQuery ( query-string AS CHAR ):

        DEF VAR records-affected AS INTEGER NO-UNDO.
        DEF VAR i AS INTEGER NO-UNDO.
    
        IF con:State = 0 THEN connect_db().
        /*IF NOT conectado THEN connect_db().*/

        /*CREATE "ADODB.RecordSet" rs.*/
    
        rs:CLOSE() NO-ERROR.
        ASSIGN cmd:CommandText = query-string                
               rs = cmd:EXECUTE ( OUTPUT records-affected, "", 128 ) NO-ERROR.
        
        ASSIGN str-erro = "".
        If ( ERROR-STATUS:NUM-MESSAGES > 0 ) THEN DO:
            str-erro = "Error: Could not execute query : ".
            DO i = 1 TO ERROR-STATUS:NUM-MESSAGES:
                 str-erro =  str-erro + ERROR-STATUS:GET-MESSAGE(i) + "|".
            END.
        END.

        rs:CLOSE() NO-ERROR.
        ASSIGN rs = ?.

    END METHOD.

    METHOD PUBLIC INT estado():
        RETURN con:State.
    END METHOD.

    METHOD PUBLIC VOID setDSN (new-dsn AS CHAR):
        ASSIGN dsn = new-dsn.
    END METHOD.

    METHOD PUBLIC VOID setHost (new-host AS CHAR):
        ASSIGN host = new-host.
    END METHOD.

    METHOD PUBLIC VOID setUserName (new-username AS CHAR):
        ASSIGN username = new-username.
    END METHOD.

    METHOD PUBLIC VOID setPassword(new-password AS CHAR):
        ASSIGN password = new-password.
    END METHOD.

    METHOD PUBLIC CHAR getLastError ():
        RETURN str-erro.
    END METHOD.

END CLASS.

