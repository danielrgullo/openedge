{esp/date-util.i}

DEFINE NEW GLOBAL SHARED VARIABLE gh-acomp AS HANDLE NO-UNDO.

DEF BUFFER b-natur-oper FOR natur-oper.

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
    ASSIGN valor = it-nota-fisc.vl-tot-item.
    IF it-nota-fisc.aliquota-icm <> 0 AND (it-nota-fisc.cd-trib-icm = 1 OR it-nota-fisc.cd-trib-icm = 4) THEN
        ASSIGN valor = valor - it-nota-fisc.vl-icms-it.
    IF it-nota-fisc.aliquota-ipi <> 0 AND (it-nota-fisc.cd-trib-ipi = 1 OR it-nota-fisc.cd-trib-ipi = 4) THEN
        ASSIGN valor = valor - it-nota-fisc.vl-ipi-it.

    FIND FIRST it-doc-fisc NO-LOCK
        WHERE it-doc-fisc.nr-doc-fis = it-nota-fisc.nr-nota-fis
          AND it-doc-fisc.serie = it-nota-fisc.serie
          AND it-doc-fisc.nr-seq-doc = it-nota-fisc.nr-seq-fat
          AND it-doc-fisc.it-codigo = it-nota-fisc.it-codigo NO-ERROR.
    IF AVAIL it-doc-fisc THEN DO:
        FIND FIRST b-natur-oper OF it-doc-fisc NO-LOCK NO-ERROR.

        IF it-doc-fisc.aliq-pis <> 0 OR (AVAIL(b-natur-oper) AND b-natur-oper.perc-PIS[1] <> 0) THEN
            ASSIGN valor = valor - it-doc-fisc.val-pis.

        IF it-doc-fisc.aliq-cofins <> 0 OR (AVAIL(b-natur-oper) AND b-natur-oper.per-fin-soc[1] <> 0) THEN
            ASSIGN valor = valor - it-doc-fisc.val-cofins.
    END.

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
    ASSIGN valor = it-nota-fisc.vl-tot-item.
    IF it-nota-fisc.aliquota-icm <> 0 AND (it-nota-fisc.cd-trib-icm = 1 OR it-nota-fisc.cd-trib-icm = 4) THEN
        ASSIGN valor = valor - it-nota-fisc.vl-icms-it.
    IF it-nota-fisc.aliquota-ipi <> 0 AND (it-nota-fisc.cd-trib-ipi = 1 OR it-nota-fisc.cd-trib-ipi = 4) THEN
        ASSIGN valor = valor - it-nota-fisc.vl-ipi-it.
    
    FIND FIRST it-doc-fisc NO-LOCK
        WHERE it-doc-fisc.nr-doc-fis = it-nota-fisc.nr-nota-fis
          AND it-doc-fisc.serie = it-nota-fisc.serie
          AND it-doc-fisc.nr-seq-doc = it-nota-fisc.nr-seq-fat
          AND it-doc-fisc.it-codigo = it-nota-fisc.it-codigo NO-ERROR.
    IF AVAIL it-doc-fisc THEN DO:
        FIND FIRST b-natur-oper OF it-doc-fisc NO-LOCK NO-ERROR.
    
        IF it-doc-fisc.aliq-pis <> 0 OR (AVAIL(b-natur-oper) AND b-natur-oper.perc-PIS[1] <> 0) THEN
            ASSIGN valor = valor - it-doc-fisc.val-pis.
    
        IF it-doc-fisc.aliq-cofins <> 0 OR (AVAIL(b-natur-oper) AND b-natur-oper.per-fin-soc[1] <> 0) THEN
            ASSIGN valor = valor - it-doc-fisc.val-cofins.
    END.
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
              AND (cod-emitente = 0 OR devol-cli.cod-emitente = cod-emitente)
              AND devol-cli.dt-devol >= dt-inicio
              AND devol-cli.dt-devol <= dt-fim  :
    
        IF acompanhamento THEN
            RUN pi-acompanhar IN gh-acomp (INPUT "Devolu‡äes: " + devol-cli.nr-nota-fis).

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
              AND (cod-estabel = '' OR ped-venda.cod-estabel = cod-estabel),
        FIRST natur-oper OF ped-venda NO-LOCK
        BY ped-ent.nr-pedcli 
        BY ped-ent.it-codigo 
        BY ped-ent.dt-entrega:
    
        IF acompanhamento THEN
            RUN pi-acompanhar IN gh-acomp (INPUT "Pedidos: " + ped-venda.nr-pedcli).

        IF sem-ferramental AND item-ferramental(ped-item.it-codigo) THEN NEXT.
    
        /*&IF DEFINED(PI-SOMA-PEDIDO) &THEN */
            RUN pi-soma-pedido.
        /*&ENDIF*/

    END.
END PROCEDURE.

PROCEDURE pedido-por-cliente2:
    DEF INPUT PARAM cod-estabel     AS CHAR NO-UNDO.
    DEF INPUT PARAM dt-inicio       AS DATE NO-UNDO.
    DEF INPUT PARAM dt-fim          AS DATE NO-UNDO.
    DEF INPUT PARAM cod-emitente    AS INTEGER NO-UNDO.
    DEF INPUT PARAM sem-ferramental AS LOGICAL NO-UNDO.

    DEF VAR valor-pedido AS DECIMAL NO-UNDO.

    DEF VAR acompanhamento AS LOGICAL NO-UNDO.

    ASSIGN acompanhamento = VALID-HANDLE(gh-acomp).

    FOR EACH ped-venda NO-LOCK 
        WHERE (cod-emitente = 0 OR ped-venda.cod-emitente = cod-emitente)
          AND (cod-estabel = '' OR ped-venda.cod-estabel = cod-estabel),
    EACH ped-item OF ped-venda NO-LOCK,
    FIRST ITEM OF ped-item NO-LOCK,
    EACH ped-ent OF ped-item NO-LOCK
        WHERE ped-ent.dt-entrega >= dt-inicio
          AND ped-ent.dt-entrega <= dt-fim
          AND ped-ent.cod-sit-ent < 4
        BY ped-ent.nr-pedcli 
        BY ped-ent.it-codigo 
        BY ped-ent.dt-entrega:
    
        IF acompanhamento THEN
            RUN pi-acompanhar IN gh-acomp (INPUT "Pedidos: " + ped-item.it-codigo + ' ' + STRING(ped-ent.dt-entrega, '99/99/9999')).

        IF sem-ferramental AND item-ferramental(ped-item.it-codigo) THEN NEXT.
    
        /*&IF DEFINED(PI-SOMA-PEDIDO) &THEN */
            RUN pi-soma-pedido.
        /*&ENDIF*/

    END.
END PROCEDURE.

PROCEDURE pedido-por-cliente-aberto:
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
              AND ped-ent.cod-sit-ent < 3
              AND ped-ent.qt-pedida > ped-ent.qt-atendida,
        FIRST ped-item OF ped-ent NO-LOCK,
        FIRST ped-venda OF ped-item NO-LOCK 
            WHERE (cod-emitente = 0 OR ped-venda.cod-emitente = cod-emitente)
              AND (cod-estabel = '' OR ped-venda.cod-estabel = cod-estabel),
        FIRST natur-oper OF ped-venda NO-LOCK
        BY ped-ent.nr-pedcli 
        BY ped-ent.it-codigo 
        BY ped-ent.dt-entrega:
    
        IF acompanhamento THEN
            RUN pi-acompanhar IN gh-acomp (INPUT "Pedidos: " + ped-venda.nr-pedcli).

        IF sem-ferramental AND item-ferramental(ped-item.it-codigo) THEN NEXT.
    
        /*&IF DEFINED(PI-SOMA-PEDIDO) &THEN */
            RUN pi-soma-pedido.
        /*&ENDIF*/

    END.
END PROCEDURE.

PROCEDURE pedido-por-range-cliente-aberto:
    DEF INPUT PARAM cod-estabel      AS CHAR NO-UNDO.
    DEF INPUT PARAM dt-inicio        AS DATE NO-UNDO.
    DEF INPUT PARAM dt-fim           AS DATE NO-UNDO.
    DEF INPUT PARAM cod-emitente-ini AS INTEGER NO-UNDO.
    DEF INPUT PARAM cod-emitente-fim AS INTEGER NO-UNDO.
    DEF INPUT PARAM sem-ferramental  AS LOGICAL NO-UNDO.

    DEF VAR valor-pedido AS DECIMAL NO-UNDO.

    DEF VAR acompanhamento AS LOGICAL NO-UNDO.

    ASSIGN acompanhamento = VALID-HANDLE(gh-acomp).

    FOR EACH ped-ent NO-LOCK 
            WHERE ped-ent.dt-entrega >= dt-inicio
              AND ped-ent.dt-entrega <= dt-fim
              AND ped-ent.cod-sit-ent < 3
              AND ped-ent.qt-pedida > ped-ent.qt-atendida,
        FIRST ped-item OF ped-ent NO-LOCK,
        FIRST ped-venda OF ped-item NO-LOCK 
            WHERE ped-venda.cod-emitente >= cod-emitente-ini
              AND ped-venda.cod-emitente <= cod-emitente-fim
              AND (cod-estabel = '' OR ped-venda.cod-estabel = cod-estabel),
        FIRST natur-oper OF ped-venda NO-LOCK
        BY ped-ent.nr-pedcli 
        BY ped-ent.it-codigo 
        BY ped-ent.dt-entrega:
    
        IF acompanhamento THEN
            RUN pi-acompanhar IN gh-acomp (INPUT "Pedidos: " + ped-venda.nr-pedcli).

        IF sem-ferramental AND item-ferramental(ped-item.it-codigo) THEN NEXT.
    
        /*&IF DEFINED(PI-SOMA-PEDIDO) &THEN */
            RUN pi-soma-pedido.
        /*&ENDIF*/

    END.
END PROCEDURE.

PROCEDURE pedido-por-nome-cliente-aberto:
    DEF INPUT PARAM cod-estabel     AS CHAR NO-UNDO.
    DEF INPUT PARAM dt-inicio       AS DATE NO-UNDO.
    DEF INPUT PARAM dt-fim          AS DATE NO-UNDO.
    DEF INPUT PARAM nome-abrev-ini  AS CHARACTER NO-UNDO.
    DEF INPUT PARAM nome-abrev-fim  AS CHARACTER NO-UNDO.
    DEF INPUT PARAM sem-ferramental AS LOGICAL NO-UNDO.

    DEF VAR valor-pedido AS DECIMAL NO-UNDO.

    DEF VAR acompanhamento AS LOGICAL NO-UNDO.

    ASSIGN acompanhamento = VALID-HANDLE(gh-acomp).

    FOR EACH ped-ent NO-LOCK 
            WHERE ped-ent.dt-entrega >= dt-inicio
              AND ped-ent.dt-entrega <= dt-fim
              AND ped-ent.cod-sit-ent < 3
              AND ped-ent.qt-pedida > ped-ent.qt-atendida,
        FIRST ped-item OF ped-ent NO-LOCK,
        FIRST ped-venda OF ped-item NO-LOCK 
            WHERE ped-venda.nome-abrev >= nome-abrev-ini
              AND ped-venda.nome-abrev <= nome-abrev-fim
              AND (cod-estabel = '' OR 
                   ped-venda.cod-estabel = cod-estabel),
        FIRST natur-oper OF ped-venda NO-LOCK
        BY ped-ent.nr-pedcli 
        BY ped-ent.it-codigo 
        BY ped-ent.dt-entrega:
    
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

FUNCTION fi-last-edi-date-estab RETURNS DATE (cod-emitente AS INTEGER, cod-estab AS CHAR ):
    DEF VAR dt AS DATE NO-UNDO.

    IF cod-estab = '' THEN
        ASSIGN cod-estab = '100'.

    FOR EACH edi-ped-venda NO-LOCK 
            WHERE edi-ped-venda.cod-emitente = cod-emitente
              AND edi-ped-venda.cod-estabel = cod-estab :
        IF dt < edi-ped-venda.dt-arquivo OR dt = ? THEN
            ASSIGN dt = edi-ped-venda.dt-arquivo.
    END.
    RETURN dt.
END.

FUNCTION fi-cliente-EDI-semana RETURN LOGICAL (cod-emitente AS INTEGER):
    RETURN cod-emitente = 10000124 OR 
           cod-emitente = 10005725  .
END FUNCTION.

FUNCTION fi-cliente-EDI-acumula RETURN LOGICAL (cod-emitente AS INTEGER):
    RETURN cod-emitente = 10005705 OR /* catalao */
           cod-emitente = 10007129 OR /* indaiatuba */
           cod-emitente = 10006052 OR /* horizontina */
           cod-emitente = 10006441 OR /* campinas */
           cod-emitente = 10008330 OR /* montenegro */
           cod-emitente = 10006891 /* outra jd */
           .

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

    IF cod-estabel = '' THEN
        ASSIGN dt-arquivo = fi-last-edi-date(cod-emitente).
    ELSE
        ASSIGN dt-arquivo = fi-last-edi-date-estab(cod-emitente, cod-estabel).
    IF fi-cliente-EDI-acumula(cod-emitente) THEN
        ASSIGN dt-arquivo = dt-arquivo - 5.
    /*IF fi-cliente-EDI-semana(cod-emitente) THEN
        ASSIGN dt-arquivo = dt-arquivo - 7.*/

    FOR EACH edi-ped-vendas NO-LOCK 
            WHERE (cod-estabel = '' OR edi-ped-venda.cod-estabel = cod-estabel)
              AND edi-ped-vendas.cod-emitente = cod-emitente
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

        /* ignora previsÆo que j  foi inserida ! */
        FIND FIRST ped-ent NO-LOCK 
            WHERE ped-ent.nr-pedcli  = edi-ped-ent.nr-pedcli
              AND ped-ent.it-codigo  = edi-ped-ent.it-codigo
              AND ped-ent.dt-entrega = edi-ped-ent.dt-janela NO-ERROR.
        IF AVAIL ped-ent THEN NEXT.

        FIND FIRST ped-ent NO-LOCK 
            WHERE ped-ent.nr-pedcli  = TRIM(edi-ped-ent.nr-pedcli)
              AND ped-ent.it-codigo  = edi-ped-ent.it-codigo
              AND ped-ent.dt-entrega = edi-ped-ent.dt-janela NO-ERROR.
        IF AVAIL ped-ent THEN NEXT.

        IF sem-ferramental AND item-ferramental(edi-ped-ent.it-codigo) THEN NEXT.

        /*&IF DEFINED(PI-SOMA-EDI) &THEN */
            RUN pi-soma-edi.
        /*&ENDIF*/

    END.
END PROCEDURE.

PROCEDURE edi-por-cliente-tudo:
    DEF INPUT PARAM cod-estabel     AS CHAR NO-UNDO.
    DEF INPUT PARAM dt-inicio       AS DATE NO-UNDO.
    DEF INPUT PARAM dt-fim          AS DATE NO-UNDO.
    DEF INPUT PARAM cod-emitente    AS INTEGER NO-UNDO.
    DEF INPUT PARAM sem-ferramental AS LOGICAL NO-UNDO.

    DEF VAR valor-edi  AS DECIMAL NO-UNDO.
    DEF VAR dt-arquivo AS DATE NO-UNDO.

    DEF VAR cliente-acumula AS LOGICAL NO-UNDO.

    DEF BUFFER b-edi-ped-ent FOR edi-ped-ent.

    DEF VAR acompanhamento AS LOGICAL NO-UNDO.

    ASSIGN acompanhamento = VALID-HANDLE(gh-acomp).

    IF cod-estabel = '' THEN
        ASSIGN dt-arquivo = fi-last-edi-date(cod-emitente).
    ELSE
        ASSIGN dt-arquivo = fi-last-edi-date-estab(cod-emitente, cod-estabel).

    ASSIGN cliente-acumula = fi-cliente-EDI-acumula(cod-emitente).
    IF cliente-acumula THEN
        ASSIGN dt-arquivo = dt-arquivo - 5.

    FOR EACH edi-ped-ent   NO-LOCK 
            WHERE edi-ped-ent.cod-emitente = cod-emitente
              AND edi-ped-ent.dt-arquivo >= dt-arquivo
              AND edi-ped-ent.dt-janela >= dt-inicio
              AND edi-ped-ent.dt-janela <= dt-fim
        BY edi-ped-ent.cod-emitente
        BY edi-ped-ent.nr-pedcli
        BY edi-ped-ent.id-programa
        BY edi-ped-ent.it-codigo
        BY edi-ped-ent.dt-janela :
    
        IF acompanhamento THEN
            RUN pi-acompanhar IN gh-acomp (INPUT "EDI : " + edi-ped-ent.it-codigo + " " + STRING(edi-ped-ent.dt-entrega)).
                
        IF sem-ferramental AND item-ferramental(edi-ped-ent.it-codigo) THEN NEXT.
        
        FIND FIRST edi-ped-venda NO-LOCK
            WHERE edi-ped-venda.cod-emitente = edi-ped-ent.cod-emitente
              AND edi-ped-venda.nr-pedcli = edi-ped-ent.nr-pedcli
              AND edi-ped-venda.codigo-refer = edi-ped-ent.codigo-refer
              AND edi-ped-venda.dt-arquivo = edi-ped-ent.dt-arquivo
              AND (cod-estabel = '' OR edi-ped-venda.cod-estabel = cod-estabel) NO-ERROR.
        IF NOT AVAIL edi-ped-venda THEN
            NEXT.

        FIND FIRST b-edi-ped-ent NO-LOCK
            WHERE b-edi-ped-ent.cod-emitente = edi-ped-ent.cod-emitente
              AND b-edi-ped-ent.dt-arquivo = edi-ped-ent.dt-arquivo
              AND b-edi-ped-ent.id-programa > edi-ped-ent.id-programa
              AND b-edi-ped-ent.nr-pedcli = edi-ped-ent.nr-pedcli
              AND b-edi-ped-ent.codigo-refer = edi-ped-ent.codigo-refer
              /*AND b-edi-ped-ent.dt-entrega = edi-ped-ent.dt-entrega*/ NO-ERROR.
        IF AVAIL b-edi-ped-ent THEN
            NEXT.
        
        FIND FIRST b-edi-ped-ent NO-LOCK
            WHERE b-edi-ped-ent.cod-emitente = edi-ped-ent.cod-emitente
              AND b-edi-ped-ent.dt-arquivo = edi-ped-ent.dt-arquivo
              AND b-edi-ped-ent.id-programa > edi-ped-ent.id-programa
              AND b-edi-ped-ent.nr-pedcli = TRIM(edi-ped-ent.nr-pedcli)
              AND b-edi-ped-ent.codigo-refer = edi-ped-ent.codigo-refer
              /*AND b-edi-ped-ent.dt-entrega = edi-ped-ent.dt-entrega*/ NO-ERROR.
        IF AVAIL b-edi-ped-ent THEN
            NEXT.        

        IF cliente-acumula THEN DO: /* se existe um depois ignora registro */
            FIND FIRST b-edi-ped-ent NO-LOCK
                WHERE b-edi-ped-ent.cod-emitente = edi-ped-ent.cod-emitente
                  AND b-edi-ped-ent.dt-arquivo > edi-ped-ent.dt-arquivo
                  AND b-edi-ped-ent.nr-pedcli = edi-ped-ent.nr-pedcli
                  AND b-edi-ped-ent.codigo-refer = edi-ped-ent.codigo-refer NO-ERROR.
            IF AVAIL b-edi-ped-ent THEN
                NEXT.
        END.
               

        /*&IF DEFINED(PI-SOMA-EDI) &THEN */
            RUN pi-soma-edi.
        /*&ENDIF*/

    END.
END PROCEDURE.
