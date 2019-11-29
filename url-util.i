/**************************************************************************** 
Description: Encodes unsafe characters in a URL as per RFC 1738 section 2.2. 
<URL:http://ds.internic.net/rfc/rfc1738.txt>, 2.2 

Input Parameters: Character string to encode, Encoding option where "query", 
"cookie", "default" or any specified string of characters are valid. 

In addition, all characters specified in the variable cUnsafe plus ASCII values 0 <= x <= 31 and 127 <= x <= 255 are considered unsafe. 

Returns: Encoded string (unkown value is returned as blank) 
Variables: cUnsafe, cReserved 
****************************************************************************/ 
FUNCTION urlencode RETURNS CHAR (cValue AS CHARACTER):
    
    DEFINE VARIABLE cHex        AS CHARACTER   NO-UNDO INITIAL "0123456789ABCDEF":U. 
    DEFINE VARIABLE cEncodeList AS CHARACTER   NO-UNDO INITIAL "query".
    DEFINE VARIABLE cEnctype    AS CHARACTER   NO-UNDO.
    
    DEFINE VARIABLE iCounter    AS INTEGER     NO-UNDO. 
    DEFINE VARIABLE cChar       AS INTEGER     NO-UNDO. 
    
    /* Unsafe characters that must be encoded in URL's.  See RFC 1738 Sect 2.2. */
    DEFINE VARIABLE cUnsafe   AS CHARACTER NO-UNDO INITIAL " <>~"#%~{}|~\^~~[]`":U.
    
    /* Reserved characters that normally are not encoded in URL's */
    DEFINE VARIABLE cReserved AS CHARACTER NO-UNDO INITIAL "~;/?:@=&":U.
    
    /* Don't bother with blank or unknown */ 
    IF LENGTH(cValue) EQ 0 OR  cValue         EQ ? THEN 
      RETURN "". 
    
    /* What kind of encoding should be used? */ 
    CASE cEnctype: 
      WHEN "query":U THEN /* QUERY_STRING name=value parts */ 
        cEncodeList = cUnsafe + cReserved + "+":U. 
      WHEN "cookie":U THEN /* Persistent Cookies */ 
        cEncodeList = cUnsafe + " ,~;":U. 
      WHEN "default":U OR WHEN "" THEN /* Standard URL encoding */ 
        cEncodeList = cUnsafe. 
      OTHERWISE 
        cEncodeList = cUnsafe + cEnctype. /* user specified ... */ 
    END CASE. 
    
    /* Loop through entire input string */ 
    iCounter = 0. 
    DO WHILE TRUE: 
      ASSIGN iCounter = iCounter + 1 
             /* ASCII value of character using single byte codepage */ 
             cChar = ASC(SUBSTRING(cValue, iCounter, 1, "RAW":U), 
                         "1252":U, 
                         "1252":U). 
      IF cChar LE 31  OR cChar GE 127 OR  INDEX(cEncodeList, CHR(cChar)) GT 0 THEN DO: 
    
        /* Replace character with %hh hexidecimal triplet */ 
        SUBSTRING(cValue, iCounter, 1, "RAW":U) = "%":U + 
          SUBSTRING(cHex, INTEGER(TRUNCATE(cChar / 16, 0)) + 1, 1, "RAW":U) + /* high */ 
          SUBSTRING(cHex, cChar MODULO 16 + 1, 1, "RAW":U). /* low digit */ 
    
        ASSIGN iCounter = iCounter + 2. /* skip over hex triplet just inserted */ 
      END. 
      IF iCounter EQ LENGTH(cValue, "RAW":U) THEN 
          LEAVE. 
    END. 

    RETURN cValue.
END FUNCTION.

/*
DEF VAR msg AS CHAR NO-UNDO.

ASSIGN msg = CHR(91) + CHR(41) + CHR(62) + CHR(30) + 
             '06' + 
             CHR(29) + '1JDE1231231212' + 
             CHR(29) + 'P189023890123a' + 
             CHR(29) + 'Q2' + 
             CHR(29) + '4LBR' + 
             CHR(30) +
             CHR(4).

ASSIGN CLIPBOARD:VALUE = urlencode(msg).

MESSAGE urlencode(msg)
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
*/
