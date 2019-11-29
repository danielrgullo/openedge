{utp/ut-api.i}
{utp/ut-api-utils.i}

{include/i-prgvrs.i ccs-itens 1.00.00.000} 

{utp/ut-api-action.i pi-get-estoque GET /estoque~* codigo=~*,estab=~* }
/*{utp/ut-api-action.i pi-get-itens GET / codigoini=~*,codigofim=~*,offset=~*,limite=~* }*/
{utp/ut-api-action.i pi-get-item GET / codigo=~* }
{utp/ut-api-action.i pi-default GET }
{utp/ut-api-notfound.i}

{ccs/api/util.i}
{ccs/api/v1/itens.i}

PROCEDURE pi-default:
    DEF INPUT PARAM jsonInput AS JSONObject NO-UNDO.
    DEF OUTPUT PARAM jsonOutput AS JSONObject NO-UNDO.

    DEF VAR oResponse   AS JSONAPIResponse NO-UNDO.

    oResponse = NEW JsonAPIResponse(NEW JSONObject()).
    oResponse:setHasNext(FALSE).
    oResponse:setStatus(404).
    jsonOutput = oResponse:createJsonResponse().
END.

PROCEDURE pi-get-item:
    DEF INPUT  PARAM jsonInput  AS JSONObject NO-UNDO.
    DEF OUTPUT PARAM jsonOutput AS JSONObject NO-UNDO.

    DEF VAR it-codigo AS CHAR NO-UNDO.
    DEF VAR oResponse AS JSONApiResponse NO-UNDO.
    
    ASSIGN it-codigo = get-queryParam-Character(jsonInput, 'codigo').

    FIND FIRST ITEM NO-LOCK
        WHERE ITEM.it-codigo = it-codigo NO-ERROR.
    IF NOT AVAIL ITEM THEN DO:

        SET oResponse = NEW JsonAPIResponse(NEW JSONObject()).

        oResponse:setStatus(404).
        oResponse:setError(NEW Progress.Lang.AppError('Item n∆o encontrado')).
        oResponse:setHasNext(FALSE).

        jsonOutput = oResponse:createJsonResponse().

    END. ELSE DO:
    
        ASSIGN jsonOutput = fi-get-item(it-codigo).

    END.

END PROCEDURE.

PROCEDURE pi-get-itens:
    DEF INPUT  PARAM jsonInput  AS JSONObject NO-UNDO.
    DEF OUTPUT PARAM jsonOutput AS JSONObject NO-UNDO.

    /*DEF VAR it-codigo-ini AS CHAR NO-UNDO.
    DEF VAR it-codigo-fim AS CHAR NO-UNDO.
    DEF VAR offset AS INTEGER NO-UNDO.
    DEF VAR limit  AS INTEGER NO-UNDO.

    DEF VAR qItem AS HANDLE NO-UNDO.

    ASSIGN it-codigo-ini = get-queryParam-Character(jsonInput, 'codigoini')
           it-codigo-fim = get-queryParam-Character(jsonInput, 'codigofim')
           offset = get-queryParam-Integer(jsonInput, 'offset')
           limit = get-queryParam-Integer(jsonInput, 'limit').

    IF offset = ? OR offset = 0 THEN
        ASSIGN offset = 1.
    IF limit = ? OR limit = 0 THEN
        ASSIGN limit = 25.


    CREATE QUERY qItem.
    qItem:SET-BUFFERS(BUFFER ITEM:HANDLE).
    qItem:QUERY-PREPARE('PRESELECT EACH item ').
    qItem:QUERY-OPEN().
    qItem:REPOSITION-TO-ROW(offset).
    qItem:GET-NEXT().

    /*DISP qItem:NUM-RESULTS.*/
     
    DO i = 1 TO limit: 
        IF qItem:QUERY-OFF-END THEN
            LEAVE.

        /* do stuff */

        qItem:GET-NEXT().
    END.
     
    qItem:QUERY-CLOSE().
    DELETE OBJECT qItem.*/

END PROCEDURE.

PROCEDURE pi-get-estoque:
    DEF INPUT PARAM jsonInput AS JSONObject NO-UNDO.
    DEF OUTPUT PARAM jsonOutput AS JSONObject NO-UNDO.

    DEF VAR it-codigo AS CHAR NO-UNDO.
    DEF VAR cod-estab AS CHAR NO-UNDO.
    DEF VAR oResponse AS JSONApiResponse NO-UNDO.
    
    FIND FIRST ITEM NO-LOCK
        WHERE ITEM.it-codigo = it-codigo NO-ERROR.
    IF NOT AVAIL ITEM THEN DO:

        SET oResponse = NEW JsonAPIResponse(NEW JSONObject()).

        oResponse:setStatus(404).
        oResponse:setError(NEW Progress.Lang.AppError('Item n∆o encontrado')).
        oResponse:setHasNext(FALSE).

        jsonOutput = oResponse:createJsonResponse().

    END. ELSE DO:

        ASSIGN jsonOutput = NEW JSONObject().

        ASSIGN it-codigo = get-queryParam-Character(jsonInput, 'codigo')
               cod-estab = get-queryParam-Character(jsonInput, 'estab').

        jsonOutput:ADD('codigo', it-codigo).
        jsonOutput:ADD('estabelecimento', cod-estab).
        jsonOutput:ADD('estoque', fi-get-estoque(cod-estab, it-codigo)).
    END.

END PROCEDURE.
