&SCOPED-DEFINE WSADATA_LENGTH 403 
&SCOPED-DEFINE HOSTENT_ADDR_LIST 13 /* CHAR**(4) */ 
&SCOPED-DEFINE sizeof_IP_ADAPTER_INFO 760
&SCOPED-DEFINE Adapter_AddressLength 401
&SCOPED-DEFINE Adapter_Address 405

FUNCTION get-hostname    RETURNS CHARACTER FORWARD.
FUNCTION get-ip-address  RETURNS CHARACTER (host-name AS CHAR) FORWARD.
FUNCTION get-mac-address RETURNS CHARACTER FORWARD.

FUNCTION fi-dec2hex      RETURNS CHARACTER ( n AS INTEGER ) FORWARD.
FUNCTION fi-letra        RETURNS CHARACTER ( n AS INTEGER ) FORWARD.


/* teste ******
DISP get-hostname()                 FORMAT "x(20)"
     get-ip-address(get-hostname()) FORMAT "x(20)"
     get-mac-address()                    FORMAT "x(22)"
     .
*/

FUNCTION get-hostname-from-environment RETURNS CHAR:
    DEF VAR c-host-name AS CHAR NO-UNDO.

    ASSIGN c-host-name = OS-GETENV("COMPUTERNAME").
    IF c-host-name = '' THEN
        ASSIGN c-host-name = get-hostname().
    RETURN c-host-name.
END FUNCTION.

FUNCTION get-hostname RETURNS CHAR :

    DEFINE VARIABLE ptr-WsaData AS MEMPTR  NO-UNDO.
    DEFINE VARIABLE v-Return    AS INTEGER NO-UNDO.
    DEFINE VARIABLE w-TcpName   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE w-Length    AS INTEGER   NO-UNDO INIT 100.

    DO
        ON ERROR UNDO, LEAVE
        ON STOP  UNDO, LEAVE:

        /* Ask Win32 for winsock usage */ 
        SET-SIZE(ptr-WsaData) = {&WSADATA_LENGTH}. /* MALLOC */
    
        RUN WSAStartup ( INPUT 257, /* requested version 1.1 */ 
                         INPUT GET-POINTER-VALUE(ptr-WsaData), 
                        OUTPUT v-Return) NO-ERROR. 
    
        IF v-Return = 0 THEN DO: 
    
            ASSIGN w-TcpName = FILL(" ", w-Length).
    
            RUN gethostname (OUTPUT w-TcpName, 
                              INPUT w-Length, 
                             OUTPUT v-Return). 
    
            IF v-Return = 0 THEN DO: 
                 ASSIGN w-TcpName = ENTRY(1, w-TcpName, CHR(0)). /* !! null-terminated str */ 
            END.
    
            RUN WSACleanup (OUTPUT v-Return) NO-ERROR. 
        END.
        SET-SIZE(ptr-WsaData) = 0. /* FREE */
    END.
    
    RETURN w-TcpName.         
END FUNCTION.

FUNCTION get-ip-address RETURNS CHAR (host-name AS CHAR):

    /*RETURN "127.0.0.1".*/
    DEFINE VARIABLE v-Return       AS INTEGER NO-UNDO.
    DEFINE VARIABLE ptr-Hostent    AS MEMPTR NO-UNDO.
    DEFINE VARIABLE ptr-AddrString AS MEMPTR NO-UNDO. 
    DEFINE VARIABLE ptr-AddrList   AS MEMPTR NO-UNDO. 
    DEFINE VARIABLE ptr-ListEntry  AS MEMPTR NO-UNDO. 
    DEFINE VARIABLE w-TcpLong      AS INTEGER NO-UNDO. 

    DEF VAR ip-addr AS CHAR NO-UNDO.

    DO
        ON ERROR UNDO, LEAVE
        ON STOP  UNDO, LEAVE:
    
        RUN getHostByName (INPUT  host-name, 
                           OUTPUT v-Return) NO-ERROR. 
    
        IF v-Return = 0 THEN 
            RETURN "".
    
        /* Set pointer to HostEnt data structure */ 
        SET-POINTER-VALUE(ptr-Hostent) = v-Return. 
    
        /* "Chase" pointers to get to first address list entry */ 
        SET-POINTER-VALUE(ptr-AddrList)  = GET-LONG(ptr-Hostent, {&HOSTENT_ADDR_LIST}). 
        SET-POINTER-VALUE(ptr-ListEntry) = GET-LONG(ptr-AddrList, 1). 
        ASSIGN w-TcpLong = GET-LONG(ptr-ListEntry, 1). 
    
        RUN inet_ntoa( INPUT w-TcpLong, 
                       OUTPUT ptr-AddrString) NO-ERROR.
    
        ASSIGN ip-addr = GET-STRING(ptr-AddrString, 1).
    
        SET-SIZE(ptr-Hostent) = 0.
        SET-SIZE(ptr-AddrString) = 0.
        SET-SIZE(ptr-AddrList) = 0.
        SET-SIZE(ptr-ListEntry) = 0.

    END.

    RETURN ip-addr.
END FUNCTION.

FUNCTION get-mac-address RETURNS CHAR :
    /*RETURN "FF:FF:FF:FF:FF:FF:FF:FF".*/
    
    DEF VAR c-mac         AS CHAR    NO-UNDO INIT "".
    /** parametros GetAdaptersInfo **/
    DEF VAR p-AdapterInfo AS MEMPTR  NO-UNDO.
    DEF VAR i-buf         AS INTEGER NO-UNDO INIT {&sizeof_IP_ADAPTER_INFO}.
    DEF VAR v-Return      AS INTEGER NO-UNDO INIT 0.
    /** **/
    DEF VAR i-len         AS INTEGER NO-UNDO INIT 0.
    DEF VAR i             AS INTEGER NO-UNDO INIT 0.

    DO
        ON ERROR UNDO, LEAVE
        ON STOP  UNDO, LEAVE:
        
        SET-SIZE(p-AdapterInfo) = i-buf * 3. /* MALLOC */
    
        RUN GetAdaptersInfo(INPUT-OUTPUT p-AdapterInfo, 
                            INPUT-OUTPUT i-buf, 
                            OUTPUT v-Return ).
        
        IF v-Return <> 0 THEN DO:
            /* tamanho errado !!! duas ou mais placas de rede ?
               usa o tamanho retornado e tenta novamente. */
            SET-SIZE(p-AdapterInfo) = i-buf.  /* MALLOC */
            RUN GetAdaptersInfo(INPUT-OUTPUT p-AdapterInfo, 
                                INPUT-OUTPUT i-buf, 
                                      OUTPUT v-Return ).
            IF v-Return <> 0 THEN DO:
                SET-SIZE(p-AdapterInfo) = 0. /* FREE */
                RETURN "". /* nÆo deu novamente? desiste !!! */
            END.
        END.
        
        /* tamanho do endere‡o fisico */
        ASSIGN i-len = GET-BYTE(p-AdapterInfo, {&Adapter_AddressLength}).
        
        /* pega bytes com endere‡os */
        DO i = 0 TO i-len - 1:
            ASSIGN c-mac = c-mac + "-" + fi-dec2hex(GET-BYTE(p-AdapterInfo, {&Adapter_Address} + i)).
        END.
    
        SET-SIZE(p-AdapterInfo) = 0. /* FREE */
    END.
    
    /* retorna tirando primeiro tra‡o */
    RETURN SUBSTR(c-mac, 2, LENGTH(c-mac) - 1).
END.

/****************************************************************************************
    fi-dec2hex - decimal para hexadecimal
*/
FUNCTION fi-dec2hex RETURNS CHAR ( n AS INT ):
    DEF VAR resto AS INT NO-UNDO.
    DEF VAR div   AS INT  NO-UNDO.
    IF n < 10 THEN 
        RETURN STRING(n, "99").
    ASSIGN resto = n MODULO 16
           div   = TRUNCATE(n / 16, 0).
    RETURN fi-letra(div) + fi-letra(resto).
END FUNCTION.
/****************************************************************************************
    fi-letra - transforma numeros em caracteres (+notacao hexa)
*/
FUNCTION fi-letra RETURNS CHAR ( n AS INT ) :
    IF n > 15 THEN 
        RETURN fi-dec2hex(n).
    IF n < 10 THEN 
        RETURN STRING(n).
    CASE n:
        WHEN 10 THEN RETURN "A".
        WHEN 11 THEN RETURN "B".
        WHEN 12 THEN RETURN "C".
        WHEN 13 THEN RETURN "D".
        WHEN 14 THEN RETURN "E".
        WHEN 15 THEN RETURN "F".
    END CASE.
END FUNCTION.

/* windows api ****************************/
PROCEDURE gethostname EXTERNAL "wsock32.dll": 
   DEFINE OUTPUT PARAMETER p-Hostname AS CHARACTER. 
   DEFINE INPUT PARAMETER p-Length AS LONG. 
   DEFINE RETURN PARAMETER p-Return AS LONG. 
END PROCEDURE. 

PROCEDURE gethostbyname EXTERNAL "wsock32.dll": 
   DEFINE INPUT PARAMETER p-Name AS CHARACTER. 
   DEFINE RETURN PARAMETER p-Hostent AS LONG. 
END PROCEDURE. 

PROCEDURE getremotemacaddress EXTERNAL "wsock32.dll":
   DEFINE INPUT PARAMETER p-RemoteIP AS CHARACTER.
   DEFINE RETURN PARAMETER p-RemoteMacAddress AS CHARACTER.
END PROCEDURE.

PROCEDURE inet_ntoa EXTERNAL "wsock32.dll" : 
   DEFINE INPUT PARAMETER p-AddrStruct AS LONG. 
   DEFINE RETURN PARAMETER p-AddrString AS MEMPTR. 
END PROCEDURE. 

PROCEDURE WSAStartup EXTERNAL "wsock32.dll" : 
   DEFINE INPUT PARAMETER p-VersionReq AS SHORT. 
   DEFINE INPUT PARAMETER ptr-WsaData AS LONG. 
   DEFINE RETURN PARAMETER p-Return AS LONG. 
END PROCEDURE. 

PROCEDURE WSACleanup EXTERNAL "wsock32": 
  DEFINE RETURN PARAMETER p-Return AS LONG. 
END PROCEDURE.

PROCEDURE GetAdaptersInfo EXTERNAL "iphlpapi":
    DEF INPUT-OUTPUT PARAM pAdapterInfo AS MEMPTR.
    DEF INPUT-OUTPUT PARAM pOuti-bufLen   AS LONG.
    DEFINE RETURN PARAMETER p-Return AS LONG. 
END PROCEDURE.
