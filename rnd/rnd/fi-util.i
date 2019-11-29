FUNCTION fi-menos-diasuteis RETURNS DATE (data AS DATE, n-dias AS INTEGER) :
    IF n-dias < 0 THEN
        RETURN data.
    DEF VAR tot-dias AS INT NO-UNDO INIT 0.
    looop:
    DO WHILE TRUE:
        ASSIGN data = data - 1.
        IF WEEKDAY(data) <> 1 AND WEEKDAY(data) <> 7 THEN DO:
            tot-dias = tot-dias + 1.
        END.
        IF n-dias <= tot-dias THEN LEAVE looop.
    END.
    RETURN data.
END FUNCTION.
