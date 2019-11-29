{utp/ut-api.i}
{utp/ut-api-utils.i}

{include/i-prgvrs.i aprov-compras 1.00.00.000} 

{utp/ut-api-action.i pi-pedidos-abertos GET /~* }
{utp/ut-api-action.i pi-default GET }
{utp/ut-api-notfound.i}
   
DEF TEMP-TABLE tt-pedidos NO-UNDO
    /*FIELD primeira-vez AS LOGICAL COLUMN-LABEL "Prim?" FORMAT "Sim/N∆o"*/
    FIELD cod-estabel  LIKE pedido-compr.cod-estabel
    FIELD num-pedido   LIKE pedido-compr.num-pedido
    FIELD data-pedido  LIKE pedido-compr.data-pedido COLUMN-LABEL "Data"
    FIELD cod-emitente LIKE emitente.cod-emitente
    FIELD nome-abrev   LIKE emitente.nome-abrev FORMAT "x(30)"
    FIELD descr-pagto  AS CHAR FORMAT "x(5)" COLUMN-LABEL "Cond.Pagto"
    FIELD valor        AS DECIMAL FORMAT "->>,>>>,>>9.99" COLUMN-LABEL "Total Pedido"    
    FIELD nome-comprador LIKE comprador.nome
    INDEX id num-pedido.

DEF TEMP-TABLE tt-ordens NO-UNDO
    FIELD numero-ordem  LIKE ordem-compra.numero-ordem
    FIELD it-codigo     LIKE ITEM.it-codigo FORMAT "x(18)"
    FIELD desc-item     AS CHAR FORMAT "x(17)" COLUMN-LABEL "Descriá∆o"
    FIELD qt-ordem      AS DECIMAL FORMAT "->>>,>>9.99" COLUMN-LABEL "Qtd!Ordem"
    FIELD qt-saldo      AS DECIMAL FORMAT "->>>,>>9.99" COLUMN-LABEL "Qtd!Saldo"
    FIELD media-consumo AS DECIMAL FORMAT "->>>,>>9.99"  COLUMN-LABEL "Media!Consumo"    
    FIELD num-parcelas  AS INTEGER FORMAT "->>9"        COLUMN-LABEL "Num.!Parc."
    FIELD prim-entrega  AS DATE    FORMAT "99/99/99"    COLUMN-LABEL "Prim.!Entrega"
    FIELD valor         AS DECIMAL FORMAT "->>>,>>9.99" COLUMN-LABEL "Valor!Unit†rio"
    FIELD ult-valor     AS DECIMAL FORMAT "->>>,>>9.99" COLUMN-LABEL "Valor!Ult.Compra"
    FIELD porcent-ultv  AS DECIMAL FORMAT "->>9.9"      COLUMN-LABEL "% Ult.!Compra"    
    /*FIELD valor-cotado AS DECIMAL FORMAT "->>>,>>9.99" COLUMN-LABEL "Valor!Cotado"
    FIELD porcent-cot  AS DECIMAL FORMAT "->>9.9"      COLUMN-LABEL "% Val!Cotado"*/    
    INDEX id numero-ordem it-codigo.


PROCEDURE pi-pedidos-abertos:
    DEF INPUT PARAM jsonInput AS JsonObject NO-UNDO.
    DEF OUTPUT PARAM jsonOutput AS JsonObject NO-UNDO.

    DEF VAR jArray AS JSONArray.
    DEF VAR json AS JSONObject.

    RUN pi-get-pendentes.
    
    SET jArray =  NEW JSONArray().
    
    FOR EACH tt-pedidos NO-LOCK:

        SET json = NEW JSONObject().

        json:ADD('estabelecimento', tt-pedidos.cod-estabel).
        json:ADD('numero', tt-pedidos.num-pedido).
        json:ADD('data', tt-pedidos.data-pedido).
        json:ADD('codigoFornecedor', tt-pedidos.cod-emitente).
        json:ADD('nomeFornecedor', tt-pedidos.nome-abrev).
        json:ADD('condPagto', tt-pedidos.descr-pagto).
        json:ADD('valorTotal', tt-pedidos.valor).
        json:ADD('comprador', tt-pedidos.nome-comprador).

        jArray:ADD(json).
    END.

    SET jsonOutput = JsonAPIResponseBuilder:ok(jArray, FALSE).

    jsonOutput:ADD('Content-Type', 'application/json').

    DEF VAR c AS CHAR NO-UNDO.
    jsonOutput:WRITE(c, TRUE).
    OUTPUT TO c:\temp\saida_json.txt.
    PUT UNFORMATTED c SKIP(2).
    OUTPUT CLOSE.

END.


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-get-pendentes w-livre 
PROCEDURE pi-get-pendentes :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

EMPTY TEMP-TABLE tt-pedidos.
EMPTY TEMP-TABLE tt-ordens.

FOR EACH doc-pend-aprov NO-LOCK 
        WHERE (doc-pend-aprov.ind-tip-doc = 4 OR 
               doc-pend-aprov.ind-tip-doc = 6)
          AND doc-pend-aprov.ind-situacao = 1,
    FIRST ordem-compra OF doc-pend-aprov NO-LOCK ,
    FIRST pedido-compr OF ordem-compra NO-LOCK,
    FIRST comprador OF ordem-compra NO-LOCK:

    FIND FIRST tt-pedidos 
        WHERE tt-pedidos.num-pedido = doc-pend-aprov.num-pedido NO-ERROR.
    IF AVAIL tt-pedidos THEN DO:
        /*tt-pedidos.valor = tt-pedidos.valor + (ordem-compra.pre-unit-for * ordem-compra.qt-solic). da errado quando tem fator de convers∆o */
        /* ordem-compra.preco-fornec */ 
        tt-pedidos.valor = tt-pedidos.valor + (ordem-compra.preco-unit * ordem-compra.qt-solic).
        NEXT.
    END.

    FIND FIRST emitente OF doc-pend-aprov NO-LOCK NO-ERROR.

    CREATE tt-pedidos.
    ASSIGN tt-pedidos.cod-estabel  = pedido-compr.cod-estabel
           tt-pedidos.num-pedido   = doc-pend-aprov.num-pedido
           tt-pedidos.data-pedido  = pedido-compr.data-pedido
           tt-pedidos.cod-emitente = doc-pend-aprov.cod-emitente
           tt-pedidos.nome-abrev   = emitente.nome-abrev
           tt-pedidos.nome-comprador = comprador.nome
           /*tt-pedidos.valor        = (ordem-compra.pre-unit-for * ordem-compra.qt-solic)*/
        /* ordem-compra.preco-fornec */
            tt-pedidos.valor         = (ordem-compra.preco-unit * ordem-compra.qt-solic).
           /*tt-pedidos.valor = fi-soma-pedidos(doc-pend-aprov.num-pedido). */
           /*tt-pedidos.primeira-vez = NOT doc-pend-aprov.sit-aprov*/
           .
     FIND FIRST cond-pagto OF ordem-compra NO-LOCK NO-ERROR.
     IF AVAIL cond-pagto THEN
         ASSIGN tt-pedidos.descr-pagto = cond-pagto.descricao.
     ELSE
         ASSIGN tt-pedidos.descr-pagto = "Especifica".
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME





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
