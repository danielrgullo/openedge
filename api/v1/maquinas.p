{utp/ut-api.i}
{utp/ut-api-utils.i}

{include/i-prgvrs.i maquinas 1.00.00.000} 

{utp/ut-api-action.i pi-get-paradas    GET /~*/paradas }
{utp/ut-api-action.i pi-get-indicador  GET /indicador/~* }
{utp/ut-api-action.i pi-get-situacao   GET /situacao~* }
{utp/ut-api-action.i pi-get-maquinas   GET /~* }
{utp/ut-api-action.i pi-get-maquina    GET / maquina=~* }
{utp/ut-api-action.i pi-default GET }
{utp/ut-api-notfound.i}
   
{ccs/api/util.i}

PROCEDURE pi-get-paradas:
    DEF INPUT PARAM jsonInput AS JsonObject NO-UNDO.
    DEF OUTPUT PARAM jsonOutput AS JsonObject NO-UNDO.

    jsonOutput = NEW JSONObject().
    jsonOutput:ADD("method", "GET").
    jsonOutput:ADD("procedure", "pi-get-paradas").
    jsonOutput:ADD("description", "Test").
END.


PROCEDURE pi-get-indicador:
    DEF INPUT PARAM jsonInput AS JsonObject NO-UNDO.
    DEF OUTPUT PARAM jsonOutput AS JsonObject NO-UNDO.

    jsonOutput = NEW JSONObject().
    jsonOutput:ADD("method", "GET").
    jsonOutput:ADD("procedure", "pi-get-indicador").
    jsonOutput:ADD("description", "Test").
END.


PROCEDURE pi-get-situacao:
    DEF INPUT PARAM jsonInput AS JsonObject NO-UNDO.
    DEF OUTPUT PARAM jsonOutput AS JsonObject NO-UNDO.

    jsonOutput = NEW JSONObject().
    jsonOutput:ADD("method", "GET").
    jsonOutput:ADD("procedure", "pi-get-situacao").
    jsonOutput:ADD("description", "Test").
END.


FUNCTION fi-get-grupo-maquina RETURNS jsonObject (gm-codigo AS CHAR):

    DEF VAR jMaq AS JsonObject.

    ASSIGN jMaq = NEW JsonObject().

    jMaq:ADD('codigo', gm-codigo).

    FIND FIRST grup-maquin NO-LOCK
        WHERE grup-maquin.gm-codigo = gm-codigo NO-ERROR.
    IF NOT AVAIL grup-maquin THEN DO:
        jMaq:ADD('error', 'Grupo de M†quinas n∆o encontrado!').
        RETURN jMaq.
    END.

    jMaq:ADD('descricao', grup-maquin.descricao).
    jMaq:ADD('reportado', grup-maquina.log-pto-control).
    jMaq:ADD('numero_maquinas', grup-maquina.num-maquinas).

    FIND FIRST ccs-grup-maquina NO-LOCK
        WHERE ccs-grup-maquina.gm-codigo = gm-codigo NO-ERROR.
    jMaq:ADD('lead_time', IF AVAIL(ccs-grup-maquina) THEN ccs-grup-maquina.gm-leadtime ELSE 0).

    jMaq:ADD('area_producao', grup-maquina.cod-area-produc).
    FIND FIRST area-produc OF grup-maquina NO-LOCK NO-ERROR.
    jMaq:ADD('descricao_area_producao', IF AVAIL(area-produc) THEN TRIM(area-produc.des-area-produc) ELSE '').

    IF AVAIL area-produc THEN DO:
        FIND FIRST usuar_mestre NO-LOCK 
            WHERE usuar_mestre.cod_usuario = area-produc.usuar-resp NO-ERROR.
        jMaq:ADD('responsavel_area_producao', IF AVAIL usuar_mestre THEN usuar_mestre.nom_usuario ELSE '').
    END. ELSE DO:
        jMaq:ADD('responsavel_area_producao', '').
    END.

    CASE grup-maquina.ind-reporte-mod:
        WHEN 1 THEN jMaq:ADD('reporte_mod', 'NAO REPORTA').
        WHEN 2 THEN jMaq:ADD('reporte_mod', 'REPORTA OPERADOR').
        WHEN 3 THEN jMaq:ADD('reporte_mod', 'REPORTA EQUIPE').
    END CASE.

    RETURN jMaq.
END FUNCTION.


PROCEDURE pi-get-maquina:
    DEF INPUT PARAM jsonInput AS JsonObject NO-UNDO.
    DEF OUTPUT PARAM jsonOutput AS JsonObject NO-UNDO.

    DEF VAR cod-ctrab AS CHAR NO-UNDO.


    ASSIGN cod-ctrab = get-queryParam-Character(jsonInput, 'maquina')

    jsonOutput = NEW JSONObject().

    DEF VAR maquina AS JsonObject.
    DEF VAR maquinas AS JsonArray.
    
    SET maquinas = NEW JsonArray().
    
    FOR EACH ctrab NO-LOCK 
            WHERE ctrab.cod-ctrab = cod-ctrab:
    
        SET maquina = NEW JsonObject().
        maquina:ADD('codigo', ctrab.cod-ctrab).
        maquina:ADD('descricao', ctrab.des-ctrab).
        maquina:ADD('grupo_maquina', fi-get-grupo-maquina(ctrab.gm-codigo)).
    
        maquinas:ADD(maquina).
    END.

    jsonOutput:ADD('maquinas', maquinas).
END.

PROCEDURE pi-get-maquinas:
    DEF INPUT PARAM jsonInput AS JsonObject NO-UNDO.
    DEF OUTPUT PARAM jsonOutput AS JsonObject NO-UNDO.

    jsonOutput = NEW JSONObject().

    DEF VAR maquina AS JsonObject.
    DEF VAR maquinas AS JsonArray.
    
    SET maquinas = NEW JsonArray().
    
    FOR EACH ctrab NO-LOCK 
            WHERE ctrab.log-possui-prepar = YES:
    
        SET maquina = NEW JsonObject().
        maquina:ADD('codigo', ctrab.cod-ctrab).
        maquina:ADD('descricao', ctrab.des-ctrab).
        maquina:ADD('grupo_maquina', fi-get-grupo-maquina(ctrab.gm-codigo)).
    
        maquinas:ADD(maquina).
    END.

    jsonOutput:ADD('maquinas', maquinas).
END.


PROCEDURE pi-default:
    DEF INPUT PARAM jsonInput AS JsonObject NO-UNDO.
    DEF OUTPUT PARAM jsonOutput AS JsonObject NO-UNDO.

    DEF VAR oJsonObject AS JSONObject NO-UNDO.
    DEF VAR oResponse   AS JsonAPIResponse NO-UNDO.

    ASSIGN oJsonObject = NEW JSONObject().
 
    oResponse = NEW JsonAPIResponse(oJsonObject).
    oResponse:setHasNext(FALSE).
    oResponse:setStatus(404).
    /*oResponse:setStatus(500).
    oResponse:setRowErrors(JsonAPIUtils:convertTempTableToJsonObject(TEMP-TABLE RowErrors:HANDLE):getJsonArray("RowErrors")).*/
     
    jsonOutput = oResponse:createJsonResponse().
END.


/*
DEFINE OUTPUT PARAM jsonOutput AS JsonObject NO-UNDO.
 
DEFINE VARIABLE oJsonObject  AS JsonObject      NO-UNDO.
DEFINE VARIABLE oResponse    AS JsonAPIResponse NO-UNDO.
 
ASSIGN oJsonObject = NEW JSONObject().
 
jsonOutput = JsonAPIResponseBuilder:ok(oJsonObject)
jsonOutput = JsonAPIResponseBuilder:asError(oJsonObject)
jsonOutput = JsonAPIResponseBuilder:empty(?)
*/
