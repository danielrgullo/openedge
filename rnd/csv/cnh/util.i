FUNCTION fi-converte-data RETURNS DATE (dt AS CHAR):
    IF dt = "" THEN
        RETURN ?.
    RETURN DATE(int(SUBSTRING(dt,5,2)),
                int(SUBSTRING(dt,7,2)),
                int(SUBSTRING(dt,1,4))).
END FUNCTION.

FUNCTION fi-converte-int RETURNS INTEGER (val AS CHAR):
    RETURN INT(REPLACE(val, ".000","")).
END FUNCTION.

FUNCTION fi-tipo-entrega RETURNS INTEGER (tipo AS CHAR):
    ASSIGN tipo = TRIM(tipo).
    CASE tipo:
        WHEN "Firm" THEN RETURN 1.
        WHEN "Planning" THEN RETURN 4.
        /*WHEN "Past Due" THEN RETURN -1 THEN*/
        OTHERWISE RETURN -1.
    END CASE.
END FUNCTION.

FUNCTION fi-get-cnpj RETURNS CHAR (cnpj AS CHAR) :
    ASSIGN cnpj = REPLACE(cnpj, 'CNPJ:', '')
           cnpj = REPLACE(cnpj, '-', '')
           cnpj = REPLACE(cnpj, '/', '')
           cnpj = REPLACE(cnpj, '.', '')
           cnpj = REPLACE(cnpj, ' ', '').
    RETURN cnpj.
END FUNCTION.

FUNCTION fi-emitente-da-planta RETURNS INTEGER (planta AS CHAR):
    CASE TRIM(planta):
        WHEN 'BH' THEN RETURN 10008693.
        WHEN 'BR' THEN RETURN 10008697.
        WHEN 'PI' THEN RETURN 10008694.
        WHEN 'SO' THEN RETURN 10008695.
        WHEN 'CT' THEN RETURN 10008696.
        WHEN '73' THEN RETURN 10007286.
    END CASE.
    RETURN 0.
END FUNCTION.
