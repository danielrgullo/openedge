/**********************************************************************
 *
 * funções diversos uteis para as rotinas de importação do EDI
 *
 **********************************************************************/

FUNCTION naoExisteArquivo RETURNS LOGICAL (nomeArquivo AS CHARACTER):
    FILE-INFO:FILE-NAME = nomeArquivo.
    RETURN FILE-INFO:FULL-PATHNAME = ?.
END FUNCTION.

FUNCTION fi-conv-date RETURNS DATE (dateString AS CHAR):
    ASSIGN dateString = TRIM(dateString).
    DEF VAR dt AS DATE NO-UNDO.
    ASSIGN dt = DATE( INT(SUBSTR(dateString, 3, 2)),
                      INT(SUBSTR(dateString, 5, 2)),
                      INT("20" + SUBSTR(dateString, 1, 2))) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN ?.
    RETURN dt.
END.

PROCEDURE pi-localizaEmitente:
    DEF INPUT-OUTPUT PARAM cod-emitente AS INTEGER NO-UNDO.
    DEF INPUT        PARAM nr-pedcli    AS CHAR    NO-UNDO.
    
    FIND FIRST emitente NO-LOCK 
        WHERE emitente.cod-emitente = cod-emitente NO-ERROR.

    IF cod-emitente = 10000124 THEN DO: /* exceção CAT (troca de codigo) */
        IF nr-pedcli BEGINS "QEST" THEN DO:

            FIND FIRST emitente NO-LOCK 
                WHERE emitente.cod-emitente = 10005825 NO-ERROR.

        END. ELSE IF nr-pedcli BEGINS "QAPD" OR nr-pedcli BEGINS "QHPD" THEN DO:

            FIND FIRST emitente NO-LOCK 
                WHERE emitente.cod-emitente = 10006602 NO-ERROR.
        END.        
    END.

    IF AVAIL emitente AND cod-emitente <> emitente.cod-emitente THEN 
        ASSIGN cod-emitente = emitente.cod-emitente.
END.

FUNCTION fi-getInicioItemCCS RETURNS CHAR (cod-emitente AS INTEGER):
    IF cod-emitente = 0 THEN
        RETURN "8.".
    ELSE IF cod-emitente = 10000159 THEN   /* 10000159 == item 8.0158 */
        RETURN "8.0158.".
    /*ELSE IF cod-emitente = 10007613 THEN /* 10007613 == item 8.6577 */
        RETURN "8.6577.".*/
    ELSE 
        RETURN "8." + STRING(cod-emitente - 10000000, "9999").
END FUNCTION.

FUNCTION fi-clienteUsaUltimaRevisao RETURNS LOGICAL (cod-emitente AS INTEGER):
    /* TODO: isso deveria ser configurado em alguma tela */
    RETURN cod-emitente = 10002155 OR cod-emitente = 10005725 OR
           cod-emitente = 10005705 OR cod-emitente = 10006052 OR
           cod-emitente = 10006441 OR cod-emitente = 10006891 OR
           cod-emitente = 10007129 OR cod-emitente = 10000326.
END FUNCTION.

FUNCTION fi-get-it-codigo RETURNS CHAR (cod-emitente AS INTEGER,
                                        nr-pedcli AS CHAR,
                                        codigo-cliente AS CHAR,
                                        alt-tec AS CHAR):
    
    DEF VAR codigo     AS CHAR NO-UNDO INIT "".
    DEF VAR codigo-ccs AS CHAR NO-UNDO INIT "8.".

    IF alt-tec <> "" THEN
        ASSIGN codigo-cliente = codigo-cliente + "#" + alt-tec.

    RUN pi-localizaEmitente(INPUT-OUTPUT cod-emitente, 
                            INPUT nr-pedcli).

    ASSIGN codigo-ccs = fi-getInicioItemCCS(cod-emitente).

    IF fi-clienteUsaUltimaRevisao(cod-emitente) THEN DO:
        FIND LAST ITEM NO-LOCK 
            WHERE ITEM.it-codigo  BEGINS codigo-ccs
              AND ITEM.codigo-refer  BEGINS codigo-cliente
              AND NOT ITEM.it-codigo MATCHES "8*CST"
              AND NOT ITEM.it-codigo MATCHES "8*FER"
              AND ITEM.cod-obsoleto <= 1 NO-ERROR.
        
    END.  ELSE DO:
        FIND FIRST ITEM NO-LOCK 
            WHERE ITEM.it-codigo BEGINS codigo-ccs
              AND ITEM.codigo-refer  = codigo-cliente
              AND NOT ITEM.it-codigo MATCHES "8*CST"
              AND NOT ITEM.it-codigo MATCHES "8*FER"
              AND ITEM.cod-obsoleto <= 1 NO-ERROR.
    END.
    IF AVAIL ITEM THEN DO:
        ASSIGN codigo = ITEM.it-codigo.
    END. ELSE DO:
        FIND LAST ped-item NO-LOCK 
            WHERE ped-item.nome-abrev = emitente.nome-abrev
              AND ped-item.nr-pedcli  = nr-pedcli
              AND ped-item.cod-sit-item <= 3 NO-ERROR.
        IF AVAIL ped-item THEN DO:
            FIND FIRST ITEM OF ped-item NO-LOCK 
                WHERE ITEM.cod-obsoleto <= 1
                  AND ITEM.codigo-refer BEGINS codigo-cliente NO-ERROR.
            IF AVAIL ITEM THEN
                ASSIGN codigo = ITEM.it-codigo.
        END.
    END.
    RETURN codigo.
END.
