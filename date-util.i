/* Funções para Datas 
 *************************************************/
FUNCTION primeiroDiaMes RETURNS DATE (data AS DATE):
    RETURN DATE(MONTH(data), 1, YEAR(data)).
END FUNCTION.

FUNCTION ultimoDiaMes RETURNS DATE ( data AS DATE ):
    IF MONTH(data) = 12 THEN
        RETURN DATE(12, 
                    31, 
                    YEAR(data)).    
    ELSE
        RETURN DATE(MONTH(data) + 1, 
                    1, 
                    YEAR(data)) - 1.
END FUNCTION.

FUNCTION proximoMes RETURNS DATE (data AS DATE):
    RETURN ultimoDiaMes(data) + 1.
END.

FUNCTION fim-de-semana RETURNS LOGICAL (data AS DATE):
    RETURN WEEKDAY(data) = 1 OR WEEKDAY(data) = 7.
END FUNCTION.

FUNCTION diferenca-dias-uteis RETURNS INTEGER (data-ini AS DATE, data-fim AS DATE):
    DEF VAR tot-dias AS INT NO-UNDO INIT 1.

    DO WHILE data-ini < data-fim :
        IF NOT fim-de-semana(data-ini) THEN
            ASSIGN tot-dias = tot-dias + 1.
        ASSIGN data-ini = data-ini + 1.
    END.
    RETURN tot-dias.
END FUNCTION.

FUNCTION conta-dias-uteis-ate-data RETURNS INTEGER (data AS DATE):
    RETURN diferenca-dias-uteis(primeiroDiaMes(data), 
                                data).
END FUNCTION.

FUNCTION conta-dias-uteis-mes RETURNS INTEGER (data AS DATE):
    RETURN diferenca-dias-uteis(primeiroDiaMes(data), 
                                ultimoDiaMes(data)).
END FUNCTION.

