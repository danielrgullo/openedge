{esp/date-util.i}

DEFINE NEW GLOBAL SHARED VARIABLE gh-acomp AS HANDLE NO-UNDO.

FUNCTION item-ferramental RETURNS LOGICAL (it-codigo AS CHAR):
    RETURN it-codigo MATCHES "8.*.FER".
END FUNCTION.

FUNCTION item-vendavel RETURNS LOGICAL (it-codigo AS CHAR):
    RETURN INDEX("78", SUBSTRING(it-codigo,1,1)) <> 0.
END FUNCTION.

FUNCTION pedido-da-nota-existe RETURNS LOGICAL :
    DEF VAR nr-pedcli AS CHAR NO-UNDO.

    IF NOT AVAIL(nota-fiscal) OR NOT AVAIL(it-nota-fisc) THEN RETURN FALSE.

    ASSIGN nr-pedcli = REPLACE(it-nota-fisc.nr-pedcli, " ", "").
    IF nr-pedcli <> "" THEN DO:
        FIND FIRST ped-venda NO-LOCK 
            WHERE ped-venda.cod-estabel = nota-fiscal.cod-estabel
              AND ped-venda.nome-abrev  = nota-fiscal.nome-ab-cli
              AND ped-venda.nr-pedcli   = it-nota-fisc.nr-pedcli NO-ERROR.
        RETURN AVAIL(ped-venda).
    END.
    RETURN TRUE.

END FUNCTION.

FUNCTION nota-cancelada RETURNS LOGICAL:
    RETURN nota-fiscal.dt-cancela <> ? AND nota-fiscal.dt-cancela > ultimoDiaMes(nota-fiscal.dt-emis-nota).
END FUNCTION.

FUNCTION valor-nota-fiscal RETURNS DECIMAL:
    IF it-nota-fisc.cd-trib-ipi = 1 OR it-nota-fisc.cd-trib-ipi = 4 THEN
        RETURN it-nota-fisc.vl-merc-liq + it-nota-fisc.vl-ipi-it.
    ELSE
        RETURN it-nota-fisc.vl-merc-liq.
END FUNCTION.

FUNCTION valor-nota-fiscal-sem-imposto RETURNS DECIMAL:
    DEF VAR valor AS DECIMAL NO-UNDO.
    ASSIGN valor = it-nota-fisc.vl-merc-liq.
    IF it-nota-fisc.aliquota-icm <> 0 THEN
        ASSIGN valor = valor - it-nota-fisc.vl-icms-it.
    IF it-nota-fisc.aliquota-ipi <> 0 THEN
        ASSIGN valor = valor - it-nota-fisc.vl-ipi-it.
    RETURN valor.
END FUNCTION.

PROCEDURE faturamento-por-cliente:
    DEF INPUT PARAM cod-estabel     AS CHAR NO-UNDO.
    DEF INPUT PARAM dt-inicio       AS DATE NO-UNDO.
    DEF INPUT PARAM dt-fim          AS DATE NO-UNDO.
    DEF INPUT PARAM cod-emitente    AS INTEGER NO-UNDO.
    DEF INPUT PARAM sem-ferramental AS LOGICAL NO-UNDO.

    DEF VAR acompanhamento AS LOGICAL NO-UNDO.

    ASSIGN acompanhamento = VALID-HANDLE(gh-acomp).
    
    FOR EACH nota-fiscal NO-LOCK 
            WHERE nota-fiscal.cod-estabel = cod-estabel
              AND nota-fiscal.dt-emis-nota >= dt-inicio 
              AND nota-fiscal.dt-emis-nota <= dt-fim 
              AND (cod-emitente = 0 OR nota-fiscal.cod-emitente = cod-emitente),
        EACH it-nota-fisc OF nota-fiscal NO-LOCK 
            WHERE it-nota-fisc.emite-duplic = TRUE,
        FIRST natur-oper OF it-nota-fisc NO-LOCK 
            WHERE natur-oper.tipo <> 1 :
    
        IF acompanhamento THEN
            RUN pi-acompanhar IN gh-acomp (INPUT "Faturamentos: " + nota-fiscal.nr-nota-fis).

        IF NOT item-vendavel(it-nota-fisc.it-codigo) THEN NEXT.
        IF sem-ferramental AND item-ferramental(it-nota-fisc.it-codigo) THEN NEXT.
        IF nota-fiscal.dt-confirma = ? OR nota-cancelada() THEN NEXT.
        IF NOT pedido-da-nota-existe() THEN NEXT.
        
        /*&IF DEFINED(PI-SOMA-FATURAMENTO) &THEN */
            RUN pi-soma-faturamento.
        /*&ENDIF*/
    END.
END PROCEDURE.

FUNCTION valor-devolucao RETURNS DECIMAL:
    IF it-nota-fisc.cd-trib-ipi = 1 OR it-nota-fisc.cd-trib-ipi = 4 THEN
        RETURN devol-cli.vl-devol.
    ELSE
        RETURN (it-nota-fisc.vl-tot-item / it-nota-fisc.qt-faturada[1]) * devol-cli.qt-devol.
END FUNCTION.

FUNCTION valor-devolucao-sem-imposto RETURNS DECIMAL:
    DEF VAR valor AS DECIMAL NO-UNDO.
    ASSIGN valor = it-nota-fisc.vl-merc-liq.
    IF it-nota-fisc.aliquota-icm <> 0 THEN
        ASSIGN valor = valor - it-nota-fisc.vl-icms-it.
    IF it-nota-fisc.aliquota-ipi <> 0 THEN
        ASSIGN valor = valor - it-nota-fisc.vl-ipi-it.
    RETURN (valor / it-nota-fisc.qt-faturada[1]) * devol-cli.qt-devol.
END FUNCTION.

PROCEDURE devolucao-por-cliente:
    DEF INPUT PARAM cod-estabel     AS CHAR NO-UNDO.
    DEF INPUT PARAM dt-inicio       AS DATE NO-UNDO.
    DEF INPUT PARAM dt-fim          AS DATE NO-UNDO.
    DEF INPUT PARAM cod-emitente    AS INTEGER NO-UNDO.
    DEF INPUT PARAM sem-ferramental AS LOGICAL NO-UNDO.

    DEF VAR acompanhamento AS LOGICAL NO-UNDO.
    ASSIGN acompanhamento = VALID-HANDLE(gh-acomp).

    FOR EACH devol-cli NO-LOCK 
            WHERE devol-cli.cod-estabel = cod-estabel
              AND devol-cli.dt-devol >= dt-inicio
              AND devol-cli.dt-devol <= dt-fim  :
    
        IF acompanhamento THEN
            RUN pi-acompanhar IN gh-acomp (INPUT "Devoluções: " + devol-cli.nr-nota-fis).

        IF NOT item-vendavel(devol-cli.it-codigo) THEN NEXT.
        IF sem-ferramental AND item-ferramental(devol-cli.it-codigo) THEN NEXT.
    
        FIND FIRST nota-fiscal NO-LOCK 
            WHERE nota-fiscal.cod-estabel  = devol-cli.cod-estabel
              AND nota-fiscal.nr-nota-fis  = devol-cli.nr-nota-fis 
              AND nota-fiscal.serie        = devol-cli.serie
              AND nota-fiscal.emite-duplic = TRUE NO-ERROR.
        IF NOT AVAIL nota-fiscal THEN NEXT.
    
        FIND it-nota-fisc OF nota-fiscal 
            WHERE it-nota-fisc.nr-seq-fat = devol-cli.nr-sequencia
              AND it-nota-fisc.it-codigo  = devol-cli.it-codigo
              AND it-nota-fisc.cod-ref    = devol-cli.cod-ref NO-LOCK NO-ERROR.
        IF NOT AVAIL it-nota-fisc THEN NEXT.
    
        FIND FIRST natur-oper OF it-nota-fisc NO-LOCK NO-ERROR.
        IF NOT AVAIL natur-oper THEN NEXT.
    
        IF NOT pedido-da-nota-existe() THEN NEXT.

        /*&IF DEFINED(PI-SOMA-DEVOLUCAO) &THEN */
            RUN pi-soma-devolucao.
        /*&ENDIF*/
        
    END.
END PROCEDURE.

FUNCTION fi-valor-pedido-qtd RETURNS DECIMAL (qtd AS DECIMAL, cd-trib-ipi AS INTEGER):
    DEF VAR valor-pedido AS DECIMAL NO-UNDO.
    ASSIGN valor-pedido = qtd * ped-item.vl-preuni.
    IF cd-trib-ipi = 1 THEN
        ASSIGN valor-pedido = valor-pedido + (valor-pedido * ped-item.aliquota-ipi / 100).
    RETURN valor-pedido.
END FUNCTION.

FUNCTION valor-pedido RETURNS DECIMAL:
    RETURN fi-valor-pedido-qtd(ped-ent.qt-pedida, 
                               natur-oper.cd-trib-ipi).
END FUNCTION.

FUNCTION valor-pedido-aberto RETURNS DECIMAL:
    RETURN fi-valor-pedido-qtd(ped-ent.qt-pedida - ped-ent.qt-atendida,
                               natur-oper.cd-trib-ipi).
END FUNCTION.

FUNCTION valor-pedido-sem-imposto RETURNS DECIMAL:
    DEF VAR valor AS DECIMAL NO-UNDO.
    ASSIGN valor = fi-valor-pedido-qtd(ped-ent.qt-pedida, 999).
    IF ped-item.aliquota-ipi <> 0 THEN
        ASSIGN valor = valor - (valor * ped-item.val-ipi  / 100).
    RETURN valor.
END FUNCTION.

PROCEDURE pedido-por-cliente:
    DEF INPUT PARAM cod-estabel     AS CHAR NO-UNDO.
    DEF INPUT PARAM dt-inicio       AS DATE NO-UNDO.
    DEF INPUT PARAM dt-fim          AS DATE NO-UNDO.
    DEF INPUT PARAM cod-emitente    AS INTEGER NO-UNDO.
    DEF INPUT PARAM sem-ferramental AS LOGICAL NO-UNDO.

    DEF VAR valor-pedido AS DECIMAL NO-UNDO.

    DEF VAR acompanhamento AS LOGICAL NO-UNDO.

    ASSIGN acompanhamento = VALID-HANDLE(gh-acomp).

    FOR EACH ped-ent NO-LOCK 
            WHERE ped-ent.dt-entrega >= dt-inicio
              AND ped-ent.dt-entrega <= dt-fim
              AND ped-ent.cod-sit-ent < 4,
        FIRST ped-item OF ped-ent NO-LOCK,
        FIRST ped-venda OF ped-item NO-LOCK 
            WHERE (cod-emitente = 0 OR ped-venda.cod-emitente = cod-emitente)
              AND ped-venda.cod-estabel = cod-estabel,
        FIRST natur-oper OF ped-venda NO-LOCK:
    
        IF acompanhamento THEN
            RUN pi-acompanhar IN gh-acomp (INPUT "Pedidos: " + ped-venda.nr-pedcli).

        IF sem-ferramental AND item-ferramental(ped-item.it-codigo) THEN NEXT.
    
        /*&IF DEFINED(PI-SOMA-PEDIDO) &THEN */
            RUN pi-soma-pedido.
        /*&ENDIF*/

    END.
END PROCEDURE.

FUNCTION fi-valor-edi-qtd RETURNS DECIMAL (qtd AS INTEGER):
    FIND FIRST preco-item NO-LOCK 
        WHERE preco-item.nr-tabpre = STRING("1000" + SUBSTRING(edi-ped-ent.it-codigo, 3, 4))
          AND preco-item.it-codigo = edi-ped-ent.it-codigo
          AND preco-item.situacao = 1 NO-ERROR.
    IF NOT AVAIL preco-item THEN
        RETURN 0.0.
    RETURN (qtd * preco-item.preco-venda).
END FUNCTION.
FUNCTION valor-edi RETURNS DECIMAL:
    RETURN fi-valor-edi-qtd(edi-ped-ent.qtd-pedida).
END FUNCTION.

FUNCTION fi-last-edi-date RETURNS DATE (cod-emitente AS INTEGER ):
    DEF VAR dt AS DATE NO-UNDO.
    FOR EACH edi-ped-venda NO-LOCK 
            WHERE edi-ped-venda.cod-emitente = cod-emitente :
        IF dt < edi-ped-venda.dt-arquivo OR dt = ? THEN
            ASSIGN dt = edi-ped-venda.dt-arquivo.
    END.
    RETURN dt.
END.

FUNCTION fi-cliente-EDI-semana RETURN LOGICAL (cod-emitente AS INTEGER):
    RETURN cod-emitente = 10000000 OR  /* colocar os clientes .. enquanto não fazer de uma forma melhor */
           cod-emitente = 10000001  .
END FUNCTION.

PROCEDURE edi-por-cliente:
    DEF INPUT PARAM cod-estabel     AS CHAR NO-UNDO.
    DEF INPUT PARAM dt-inicio       AS DATE NO-UNDO.
    DEF INPUT PARAM dt-fim          AS DATE NO-UNDO.
    DEF INPUT PARAM cod-emitente    AS INTEGER NO-UNDO.
    DEF INPUT PARAM sem-ferramental AS LOGICAL NO-UNDO.

    DEF VAR valor-edi  AS DECIMAL NO-UNDO.
    DEF VAR dt-arquivo AS DATE NO-UNDO.

    DEF VAR acompanhamento AS LOGICAL NO-UNDO.

    ASSIGN acompanhamento = VALID-HANDLE(gh-acomp).

    ASSIGN dt-arquivo = fi-last-edi-date(cod-emitente).
    IF fi-cliente-EDI-semana(cod-emitente) THEN
        ASSIGN dt-arquivo = dt-arquivo - 7.

    FOR EACH edi-ped-vendas NO-LOCK 
            WHERE edi-ped-vendas.cod-emitente = cod-emitente
              AND edi-ped-venda.dt-arquivo >= dt-arquivo,
        EACH edi-ped-ent   NO-LOCK 
            WHERE edi-ped-ent.cod-emitente = edi-ped-vendas.cod-emitente
              AND edi-ped-ent.dt-arquivo   = edi-ped-vendas.dt-arquivo
              AND edi-ped-ent.nr-pedcli    = edi-ped-vendas.nr-pedcli
              AND edi-ped-ent.id-programa  = edi-ped-vendas.id-programa
              AND edi-ped-ent.codigo-refer = edi-ped-vendas.codigo-refer
              AND edi-ped-ent.dt-janela >= dt-inicio
              AND edi-ped-ent.dt-janela <= dt-fim :
    
        IF acompanhamento THEN
            RUN pi-acompanhar IN gh-acomp (INPUT "EDI : " + edi-ped-venda.nr-pedcli + " " + STRING(edi-ped-ent.dt-entrega)).

        IF edi-ped-ent.it-codigo = "" THEN NEXT.

        /* ignora previsão que já foi inserida ! */
        FIND FIRST ped-ent NO-LOCK 
            WHERE ped-ent.nr-pedcli  = edi-ped-ent.nr-pedcli
              AND ped-ent.it-codigo  = edi-ped-ent.it-codigo
              AND ped-ent.dt-entrega = edi-ped-ent.dt-janela NO-ERROR.
        IF AVAIL ped-ent THEN NEXT.

        IF sem-ferramental AND item-ferramental(edi-ped-ent.it-codigo) THEN NEXT.

        /*&IF DEFINED(PI-SOMA-EDI) &THEN */
            RUN pi-soma-edi.
        /*&ENDIF*/

    END.
END PROCEDURE.

