
FUNCTION fi-get-item RETURNS JSONObject (it-codigo AS CHAR):

    DEF VAR json AS JSONObject.
    DEF VAR c-estado AS CHAR NO-UNDO.

    IF NOT AVAIL ITEM OR ITEM.it-codigo <> it-codigo THEN DO:
        FIND FIRST ITEM WHERE item.it-codigo = it-codigo NO-LOCK NO-ERROR.
        IF NOT AVAIL ITEM THEN DO:
            STOP.
        END.
    END.

    SET json = NEW JSONObject().

    json:ADD('codigo', it-codigo).
    json:ADD('descricao', ITEM.desc-item).
    json:ADD('codigo_cliente', ITEM.codigo-refer).
    
    ASSIGN c-estado = ''.
    CASE ITEM.cod-obsoleto:
        WHEN 1 THEN ASSIGN c-estado = 'ATIVO'.
        WHEN 2 THEN ASSIGN c-estado = 'OBSOLETO ORDENS AUTOMµTICAS'.
        WHEN 3 THEN ASSIGN c-estado = 'OBSOLETO TODAS AS ORDENS'.
        WHEN 4 THEN ASSIGN c-estado = 'TOTALMENTE OBSOLETO'.
    END CASE.
    json:ADD('estado', c-estado).
    
    json:ADD('familia', ITEM.fm-codigo).
    json:ADD('unidade', ITEM.un).
    json:ADD('narrativa', TRIM(ITEM.narrativa)).
    
    json:ADD('estabelecimento_padrao', ITEM.cod-estabel).
    
    json:ADD('data_implantacao', ITEM.data-implant).
    json:ADD('deposito_padrao', ITEM.deposito-pad).

    RETURN json.

END FUNCTION.


FUNCTION fi-get-estoque RETURNS JSONArray (cod-estabel AS CHAR, it-codigo AS CHAR):

    DEF VAR JSONEstoqArray AS JSONArray NO-UNDO.
    DEF VAR JSONEstoq AS JSONObject NO-UNDO.

    SET JSONEstoqArray = NEW JSONArray().

    FOR EACH saldo-estoq NO-LOCK
            WHERE saldo-estoq.cod-estabel = cod-estabel
              AND saldo-estoq.it-codigo = it-codigo
              AND saldo-estoq.qtidade-atu <> 0:

        SET JSONEstoq = NEW JSONObject().

        JSONEstoq:ADD('estabelecimento', saldo-estoq.cod-estabel ).
        JSONEstoq:ADD('item', saldo-estoq.it-codigo ).
        JSONEstoq:ADD('deposito', saldo-estoq.cod-depos ).
        JSONEstoq:ADD('localizacao', saldo-estoq.cod-local ).
        JSONEstoq:ADD('referencia', saldo-estoq.cod-refer ).
        JSONEstoq:ADD('lote', saldo-estoq.lote ).
        JSONEstoq:ADD('validade_lote', saldo-estoq.dt-vali-lote ).
        JSONEstoq:ADD('quantidade', saldo-estoq.qtidade-atu ).
        JSONEstoq:ADD('quantidade_alocada', saldo-estoq.dec-2 ).

        JSONEstoqArray:ADD(JSONEstoq).
    END.

    RETURN JSONEstoqArray.

END FUNCTION.
