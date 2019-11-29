
FUNCTION get-queryParam-JSONArray RETURNS JSONArray (json AS JSONObject, param-name AS CHAR):

    RETURN json:getJSONObject('queryParams')
               :getJSONArray(param-name).

END FUNCTION.

FUNCTION get-queryParam-Character RETURNS CHARACTER  (json AS JSONObject, param-name AS CHAR):
    RETURN get-queryParam-JSONArray(json, param-name):getCharacter(1).
END.

FUNCTION get-queryParam-Integer RETURNS INTEGER (json AS JSONObject, param-name AS CHAR):
    RETURN get-queryParam-JSONArray(json, param-name):getInteger(1).
END.

FUNCTION get-queryParam-Decimal RETURNS DECIMAL (json AS JSONObject, param-name AS CHAR):
    RETURN get-queryParam-JSONArray(json, param-name):getDecimal(1).
END.

FUNCTION get-queryParam-Date RETURNS DATE (json AS JSONObject, param-name AS CHAR):
    RETURN get-queryParam-JSONArray(json, param-name):getDate(1).
END.

FUNCTION get-queryParam-Object RETURNS JSONObject (json AS JSONObject, param-name AS CHAR):
    RETURN get-queryParam-JSONArray(json, param-name):getJSONObject(1).
END.

