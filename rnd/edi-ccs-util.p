DEFINE TEMP-TABLE RowErros NO-UNDO 
       FIELD errorSequence    AS INTEGER
       FIELD errorNumber      AS INTEGER
       FIELD errorDescription AS CHARACTER 
       FIELD errorParameters  AS CHARACTER
       FIELD errorType        AS CHARACTER
       FIELD errorHelp        AS CHARACTER
       FIELD errorsubtype     AS CHARACTER.

/**
 *
 ****************************************************************************************************/
PROCEDURE pi-procura-emitente :
    DEF INPUT PARAM cod-emitente AS INTEGER NO-UNDO.
    DEF INPUT PARAM nr-pedcli    AS CHAR NO-UNDO.

    /*
     * A Caterpillar sempre envia o arquivo no CNPJ de Piracicaba :(
     * O pedido que define o destino ...
     */

    IF nr-pedcli BEGINS "QEST" OR nr-pedcli BEGINS "HETZ" THEN
        FIND FIRST emitente NO-LOCK 
            WHERE emitente.cod-emitente = 10005825 NO-ERROR.
    ELSE IF nr-pedcli BEGINS "QAPD" OR nr-pedcli BEGINS "QHPD" THEN 
        FIND FIRST emitente NO-LOCK 
            WHERE emitente.cod-emitente = 10006602 NO-ERROR.
    ELSE
        FIND FIRST emitente NO-LOCK 
            WHERE emitente.cod-emitente = cod-emitente NO-ERROR.

    IF NOT AVAIL emitente THEN
        STOP.

END PROCEDURE.

/**
 *
 ****************************************************************************************************/
PROCEDURE pi-log-erros:

    OUTPUT TO "c:\temp\erros.edi.txt" APPEND.
    FOR EACH RowErros NO-LOCK:
        /* relatorio de erros */
        PUT UNFORMATTED 
            STRING(TODAY, "99/99/9999") " " STRING(TIME, "HH:MM:SS") ": "
            RowErros.errorDescription  
            " Tipo :" tt-edi-ccs.c-tipo " - " tt-edi-ccs.nr-pedcli 
            " Item :" tt-edi-ccs.it-codigo  " " tt-edi-ccs.dt-entrega SKIP.
    END.
    OUTPUT CLOSE.

END PROCEDURE.


/*******************************************************************************************************/

FUNCTION fi-gera-nome-arquivo RETURNS CHAR
  ( raiz AS CHAR, extensao AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    ASSIGN raiz = raiz + "-" +
                  STRING(YEAR(TODAY)) + STRING(MONTH(TODAY)) + STRING(DAY(TODAY)) + "-" +
                  REPLACE(STRING(TIME, "HH:MM:SS"), ":", "") +
                  "." + extensao.
    RETURN raiz.
END FUNCTION.
