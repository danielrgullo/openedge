DEFINE VARIABLE hbodi018     AS HANDLE NO-UNDO.
DEFINE VARIABLE hbodi159     AS HANDLE NO-UNDO.
DEFINE VARIABLE hbodi159sdf  AS HANDLE NO-UNDO.
DEFINE VARIABLE hbodi159com  AS HANDLE NO-UNDO.

DEFINE VARIABLE hbodi149     AS HANDLE NO-UNDO.
DEFINE VARIABLE hbodi149can  AS HANDLE NO-UNDO.

DEFINE VARIABLE hbodi154     AS HANDLE NO-UNDO.
DEFINE VARIABLE hbodi154sdf  AS HANDLE NO-UNDO.
DEFINE VARIABLE hbodi154cal  AS HANDLE NO-UNDO.

DEFINE VARIABLE hbodi157     AS HANDLE NO-UNDO.

DEF VAR c-representante      AS CHAR INIT "CCS Tecnolog" NO-UNDO.

DEF STREAM st-erro.

FUNCTION get-hr-entrega    RETURNS CHAR (nr-pedcli AS CHAR, it-codigo AS CHAR, dt-entrega AS DATE, c-hora AS CHAR) FORWARD.
/**********************************************************/
DEFINE TEMP-TABLE tt-ped-venda NO-UNDO LIKE ped-venda
    FIELD r-rowid AS ROWID.

DEFINE TEMP-TABLE tt-ped-item  NO-UNDO LIKE ped-item
    FIELD r-rowid AS ROWID.

DEFINE TEMP-TABLE tt-ped-ent NO-UNDO LIKE ped-ent
    FIELD r-rowid AS ROWID .
  
DEFINE TEMP-TABLE RowErros NO-UNDO 
    FIELD errorSequence    AS INTEGER
    FIELD errorNumber      AS INTEGER
    FIELD errorDescription AS CHARACTER 
    FIELD errorParameters  AS CHARACTER
    FIELD errorType        AS CHARACTER
    FIELD errorHelp        AS CHARACTER
    FIELD errorsubtype     AS CHARACTER.

DEFINE TEMP-TABLE tt-log-apiped NO-UNDO
    FIELD nr-ped LIKE ped-venda.nr-pedcli
    FIELD erro   AS CHARACTER
    INDEX ind-cod nr-ped.
/*****************************************************************/
PROCEDURE adicionaPedido:
    DEF INPUT PARAM cod-estabel  AS CHAR NO-UNDO INIT "100".
    DEF INPUT PARAM c-nome-abrev AS CHAR NO-UNDO.
    DEF INPUT PARAM c-ped        AS CHAR NO-UNDO.
    DEF INPUT PARAM c-tabpre     AS CHAR NO-UNDO. /* emitente.nr-tabpre !!! */
    DEF INPUT PARAM i-esp-ped    AS INT  NO-UNDO INIT 2. /* 1 = pedido simples,  2 = programacao entrega */
    DEF OUTPUT PARAM TABLE FOR RowErros .

    DEF VAR c-nat-operacao AS CHAR NO-UNDO INIT "5101".
    /*DEF VAR cod-estabel    AS CHAR NO-UNDO INIT "100".*/

    /* Crio Instancia da DBO */
    IF NOT VALID-HANDLE(hbodi159) THEN
        RUN dibo/bodi159.p    PERSISTENT SET hbodi159.
    
    IF NOT VALID-HANDLE(hbodi159sdf) THEN
        RUN dibo/bodi159sdf.p PERSISTENT SET hbodi159sdf.
    
    IF VALID-HANDLE(hbodi159) THEN
        RUN openQueryStatic IN hbodi159 (INPUT "ChPedido":U).
    ELSE DO:
        MESSAGE "Erro Inesperado !!! Não carregou bo !!! " VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RUN destroyAll.
        RETURN "NOK".
    END.
    
    FIND FIRST emitente WHERE emitente.nome-abrev = c-nome-abrev NO-LOCK NO-ERROR.

    DO TRANSACTION:
    
        RUN newRecord IN hbodi159.
        RUN getRecord IN hbodi159 (OUTPUT TABLE tt-ped-venda).
    
        FIND FIRST tt-ped-venda NO-ERROR.
    
        ASSIGN tt-ped-venda.nome-abrev = emitente.nome-abrev.
        RUN INPUTTable IN hbodi159sdf (INPUT TABLE tt-ped-venda).
        RUN setDefaultCustomerName IN hbodi159sdf.
        RUN setDefaultCentralSales IN hbodi159sdf.
        RUN OUTPUTTable IN hbodi159sdf (OUTPUT TABLE tt-ped-venda).
               
        FIND FIRST tt-ped-venda NO-ERROR.        
        /*
        == alterado para corrigir problema da CAT e natureza de operação ==
        == 2012.05.31 ==
        IF emitente.nat-operacao <> "" THEN
            c-nat-operacao = emitente.nat-operacao.*/
        RUN pega-natureza-operacao( INPUT cod-estabel,
                                    INPUT c-ped, 
                                    INPUT-OUTPUT c-nat-operacao ).

        IF c-ped BEGINS "QYPP" OR c-ped BEGINS "QYPD" THEN
            ASSIGN i-esp-ped = 1.

        FIND FIRST natur-oper NO-LOCK
            WHERE natur-oper.nat-oper = c-nat-operacao NO-ERROR.

        /* TODO: trocar cod-estabel se for para pedidos do PR */

        ASSIGN  tt-ped-venda.nr-pedcli       = c-ped /*STRING(tt-ped-venda.nr-pedido)*/
                tt-ped-venda.cod-estabel     = cod-estabel
                tt-ped-venda.nr-pedrep       = ""
                tt-ped-venda.contato         = ""
                tt-ped-venda.cod-gr-cli      = emitente.cod-gr-cli
                tt-ped-venda.no-ab-reppri    = c-representante
                tt-ped-venda.cod-mensagem    = natur-oper.cod-mensagem
                tt-ped-venda.esp-ped         = i-esp-ped /* 1 = pedido simples,  2 = programacao entrega */
                tt-ped-venda.origem          = 1 /* padrão: 1 */
                tt-ped-venda.cd-origem       = 2 /* 2 = EDI */
                tt-ped-venda.tp-preco        = 3 /* 3 = dia do fat. */
                tt-ped-venda.nr-tab-finan    = 1 /* padrão: 0, mas é obrigatório */
                tt-ped-venda.nr-ind-finan    = 1 /* padrão: 0, mas é obrigatório */
                tt-ped-venda.tab-ind-fin     = 1 /* padrão: mesmo que nr-tab-finan */ 
                tt-ped-venda.mo-codigo       = 0 /* 0 = moeda corrente */
                tt-ped-venda.cod-sit-aval    = 1 /* padrão: 1 */
                tt-ped-venda.cod-sit-pre     = 1 /* padrão: 1 */
                tt-ped-venda.cod-sit-ped     = 1 /* padrão: 1 */
                tt-ped-venda.cod-sit-com     = 1 /* padrão: 1 */
                tt-ped-venda.proc-edi        = 1 /* padrão: ? */
                /*tt-ped-venda.ind-aprov       = NO */
                tt-ped-venda.ind-aprov       = YES
                tt-ped-venda.tp-receita      = emitente.tp-rec-padrao
                tt-ped-venda.tp-pedido       = ""
                tt-ped-venda.nat-operacao    = c-nat-operacao
                tt-ped-venda.nr-pedido       = NEXT-VALUE(seq-nr-pedido)
                /*tt-ped-venda.cod-canal-venda = 1*/
                tt-ped-venda.cod-canal-venda = 0
                tt-ped-venda.cod-cond-pag    = emitente.cod-cond-pag
                tt-ped-venda.cod-portador    = 341
                tt-ped-venda.modalidade      = 6
                tt-ped-venda.cod-entrega     = "Padrão"
                tt-ped-venda.local-entreg    = emitente.endereco
                tt-ped-venda.bairro          = emitente.bairro
                tt-ped-venda.cidade          = emitente.cidade
                tt-ped-venda.estado          = emitente.estado
                tt-ped-venda.cep             = emitente.cep
                tt-ped-venda.caixa-postal    = ""
                tt-ped-venda.pais            = emitente.pais
                tt-ped-venda.cgc             = emitente.cgc
                tt-ped-venda.nome-transp     = "" /* (não obrigatório) */
                tt-ped-venda.dt-emissao      = TODAY
                tt-ped-venda.dt-implant      = TODAY
                /*tt-ped-venda.dt-entrega      = ?
                tt-ped-venda.dt-minfat       = ?
                tt-ped-venda.dt-lim-fat      = ?
                tt-ped-venda.dt-entorig      = ?*/
                tt-ped-venda.observacoes     = ""
                .
        IF c-tabpre <> "" THEN
            ASSIGN tt-ped-venda.nr-tabpre = c-tabpre.

        /* transportador da atualização clientes do estabelecimento - PD0507 */
        FIND FIRST estab-cli NO-LOCK 
            WHERE estab-cli.cod-estabel = cod-estabel
              AND estab-cli.nome-abrev = c-nome-abrev NO-ERROR.
        IF AVAIL estab-cli THEN
            ASSIGN tt-ped-venda.nome-transp = estab-cli.nome-transp.

        RUN setRecord IN hbodi159 (INPUT TABLE tt-ped-venda).
        RUN createRecord IN hbodi159.
        
        RUN getRowErrors IN hbodi159 (OUTPUT TABLE RowErros).
        IF CAN-FIND(FIRST RowErros) THEN DO:
            OUTPUT STREAM st-erro TO c:\temp\erros_bodi159.txt APPEND.
            FOR EACH RowErros:
                EXPORT STREAM st-erro DELIMITER ";" RowErros .
            END.
            OUTPUT STREAM st-erro CLOSE.
            UNDO , LEAVE.
        END.
    
        RUN getRecord IN hbodi159 (OUTPUT TABLE tt-ped-venda).
    
        DEF VAR i-nr-pedido AS INTEGER NO-UNDO.
        FIND FIRST tt-ped-venda NO-LOCK NO-ERROR.
        IF AVAIL tt-ped-venda THEN
            ASSIGN i-nr-pedido = tt-ped-venda.nr-pedido.
    
        DEF VAR rw-pedido AS ROWID NO-UNDO.
        RUN getRowid IN hbodi159 (OUTPUT rw-pedido).
    
        FIND FIRST repres NO-LOCK WHERE repres.nome-abrev = c-representante NO-ERROR.
        IF AVAIL repres THEN DO:
            CREATE ped-repre.
            ASSIGN ped-repre.nr-pedido   = i-nr-pedido
                   ped-repre.nome-ab-rep = repres.nome-abrev
                   ped-repre.ind-repbase = yes
                   ped-repre.perc-comis  = 0
                   ped-repre.comis-emis  = 0
                   .
        END.
    END.
    RUN destroyAll.
END PROCEDURE.

PROCEDURE adicionaPedidoItem:
    DEF INPUT PARAM cod-estabel  AS CHAR NO-UNDO INIT "100".
    DEF INPUT PARAM c-nome-abrev AS CHAR NO-UNDO.
    DEF INPUT PARAM c-ped        AS CHAR NO-UNDO.
    DEF INPUT PARAM c-it         AS CHAR NO-UNDO.
    DEF INPUT PARAM dt-vencto    AS DATE NO-UNDO.
    DEF INPUT PARAM d-qtd     AS DECIMAL NO-UNDO.
    DEF INPUT PARAM d-preco   AS DECIMAL NO-UNDO.
    DEF INPUT PARAM c-obs        AS CHAR NO-UNDO.
    DEF OUTPUT PARAM TABLE FOR RowErros .

    DEF VAR c-nat-operacao AS CHAR NO-UNDO INIT "5101".
    
    DEF VAR i-seq AS INTEGER NO-UNDO.

    FIND FIRST ITEM NO-LOCK WHERE ITEM.it-codigo = c-it NO-ERROR.
    IF NOT AVAIL ITEM THEN DO:
        MESSAGE "Erro Inesperado ! Item inválido !!!":U
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN "NOK".
    END.

    /* Crio Instancia da DBO */
    IF NOT VALID-HANDLE(hbodi154) THEN
        RUN dibo/bodi154.p    PERSISTENT SET hbodi154.

    IF NOT VALID-HANDLE(hbodi154sdf) THEN
        RUN dibo/bodi154sdf.p PERSISTENT SET hbodi154sdf.

    IF NOT VALID-HANDLE(hbodi154cal) THEN
        RUN dibo/bodi154cal.p PERSISTENT SET hbodi154cal.

    IF VALID-HANDLE(hbodi154) THEN
       RUN openQueryStatic IN hbodi154 (INPUT "DEFAULT":U).
    ELSE DO:
        MESSAGE "Erro Inesperado !!! Não Carregou BO !!!"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RUN destroyAll.
        RETURN "NOK".
    END.

    FIND FIRST emitente WHERE emitente.nome-abrev = c-nome-abrev NO-LOCK NO-ERROR.

    ASSIGN i-seq = 0.
    FOR EACH ped-item NO-LOCK WHERE ped-item.nr-pedcli = c-ped :
        IF i-seq < ped-item.nr-sequencia THEN
            ASSIGN i-seq = ped-item.nr-sequencia.
    END.
    ASSIGN i-seq = i-seq + 10.
    
    DO TRANSACTION:
           
        EMPTY TEMP-TABLE tt-ped-item.

        RUN newRecord IN hbodi154.
        RUN getRecord IN hbodi154 (OUTPUT TABLE tt-ped-item).
    
        FIND FIRST tt-ped-item NO-ERROR.
        ASSIGN tt-ped-item.nome-abrev   = emitente.nome-abrev
               tt-ped-item.nr-pedcli    = c-ped
               tt-ped-item.it-codigo    = c-it
               tt-ped-item.nr-sequencia = i-seq.
    
        RUN INPUTTable IN hbodi154sdf (INPUT TABLE tt-ped-item).
        RUN setDefaultItem             IN hbodi154sdf.
        RUN setDefaultPriceTable       IN hbodi154sdf.
        RUN setDefaultTransactionType  IN hbodi154sdf.
        RUN OUTPUTTable IN hbodi154sdf(OUTPUT TABLE tt-ped-item).

        FIND FIRST tt-ped-item NO-ERROR.
        
        /* retorna erro se cabeçalho do pedido (ped-venda) não existe. */
        EMPTY TEMP-TABLE tt-ped-venda.
        FIND ped-venda OF tt-ped-item NO-ERROR.
        IF NOT AVAIL ped-venda THEN DO:
            RUN destroyAll.    
            RETURN "NOK".
        END.
        BUFFER-COPY ped-venda TO tt-ped-venda.
        ASSIGN tt-ped-venda.r-rowid = ROWID(tt-ped-venda).

        
                /*
        == alterado para corrigir problema da CAT e natureza de operação ==
        == 2012.05.31 == rev 2012.06.12 (não usa mas deixa pronto se um dia voltar a usar).
        IF emitente.nat-operacao <> "" THEN
            ASSIGN c-nat-operacao = emitente.nat-operacao.*/
        RUN pega-natureza-operacao( INPUT cod-estabel,
                                    INPUT c-ped, 
                                    INPUT-OUTPUT c-nat-operacao ).

        ASSIGN /*tt-ped-item.nat-operacao   = c-nat-operacao*/
               tt-ped-item.nr-ordem       = 0
               tt-ped-item.dt-entorig     = dt-vencto
               tt-ped-item.dt-entrega     = dt-vencto
               tt-ped-item.qt-pedida      = d-qtd
               tt-ped-item.qt-un-fat      = d-qtd
               tt-ped-item.qt-atendida    = 0
               tt-ped-item.vl-pretab      = d-preco
               tt-ped-item.vl-preori      = d-preco
               tt-ped-item.vl-preuni      = d-preco
               tt-ped-item.vl-liq-it      = d-preco
               tt-ped-item.vl-liq-abe     = d-preco
               /*tt-ped-item.vl-tot-it      = d-preco*/
               tt-ped-item.vl-desconto    = 0
               tt-ped-item.val-desconto-inform = 0
               tt-ped-item.esp-ped        = tt-ped-venda.esp-ped
               tt-ped-item.cd-origem      = 2 /* 2 = EDI */ 
               tt-ped-item.tp-adm-lote    = 1 /* padrão: 1 */
               tt-ped-item.tp-aloc-lote   = 1 /* padrão: 1 */
               tt-ped-item.tipo-atend     = 2 /* 2 = parcial */
               tt-ped-item.cod-sit-item   = 1 /* padrão: 1 */
               tt-ped-item.cod-sit-pre    = tt-ped-venda.cod-sit-pre
               tt-ped-item.cod-sit-com    = tt-ped-venda.cod-sit-com
               tt-ped-item.cod-entrega    = tt-ped-venda.cod-entrega
               tt-ped-item.observacao     = c-obs
               .
        FOR FIRST preco-item FIELDS (nr-tabpre) NO-LOCK 
                WHERE preco-item.it-codigo = c-it
                  AND preco-item.situacao = 1,
            FIRST tb-preco FIELDS (nr-tabpre) OF preco-item NO-LOCK 
                WHERE tb-preco.situacao = 1:
            ASSIGN tt-ped-item.nr-tabpre = tb-preco.nr-tabpre.
        END.

        RUN SetRecord IN hbodi154 (INPUT TABLE tt-ped-item).
        RUN CreateRecord IN hbodi154.
        RUN GetRowErrors IN hbodi154 (OUTPUT TABLE RowErros).
        IF CAN-FIND(FIRST RowErros) THEN DO:

            OUTPUT STREAM st-erro TO c:\temp\erros_bodi154.txt APPEND.
            FOR EACH RowErros:
                EXPORT STREAM st-erro DELIMITER ";" RowErros .
            END.
            OUTPUT STREAM st-erro CLOSE.

            RUN destroyAll.
            UNDO , LEAVE.
        END.
    END.
    RUN destroyAll.
END PROCEDURE. /* adicionaPedidoItem */

PROCEDURE alteraPedidoItem:
    DEF INPUT  PARAM c-nome-abrev AS CHAR NO-UNDO.
    DEF INPUT  PARAM c-ped        AS CHAR NO-UNDO.
    DEF INPUT  PARAM i-sequencia  AS  INT NO-UNDO.
    DEF INPUT  PARAM c-it         AS CHAR NO-UNDO.
    DEF INPUT  PARAM dt-vencto    AS DATE NO-UNDO.
    DEF INPUT  PARAM d-qtd     AS DECIMAL NO-UNDO.
    DEF OUTPUT PARAM TABLE FOR RowErros.

    DEF VAR c-hora AS CHAR NO-UNDO.

    FIND FIRST ITEM NO-LOCK WHERE ITEM.it-codigo = c-it NO-ERROR.
    IF NOT AVAIL ITEM THEN DO:
        MESSAGE "Erro Inesperado ! Item inválido !!!"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN "NOK".
    END.

    /* Crio Instancia da DBO */
    IF NOT VALID-HANDLE(hbodi154) THEN
        RUN dibo/bodi154.p    PERSISTENT SET hbodi154.

    IF VALID-HANDLE(hbodi154) THEN
       RUN openQueryStatic IN hbodi154 (INPUT "DEFAULT":U).
    ELSE DO:
        MESSAGE "Erro Inesperado !!! Não Carregou bo !!!"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RUN destroyAll.
        RETURN "NOK".
    END.

        /* Crio Instancia da DBO */
    IF NOT VALID-HANDLE(hbodi159) THEN
        RUN dibo/bodi159.p    PERSISTENT SET hbodi159.
    
    IF VALID-HANDLE(hbodi159) THEN
        RUN openQueryStatic IN hbodi159 (INPUT "ChPedido":U).
    ELSE DO:
        MESSAGE "Erro Inesperado !!! Não carregou bo !!! " VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RUN destroyAll.
        RETURN "NOK".
    END.

    FIND FIRST emitente NO-LOCK 
        WHERE emitente.nome-abrev = c-nome-abrev NO-ERROR.

    DO TRANSACTION:

        EMPTY TEMP-TABLE tt-ped-item.

        RUN goToKey   IN hbodi159 (INPUT c-nome-abrev, 
                                   INPUT c-ped).
        RUN goToKey   IN hbodi154 (INPUT c-nome-abrev, 
                                   INPUT c-ped, 
                                   INPUT i-sequencia, 
                                   INPUT c-it, 
                                   INPUT "").
        
        RUN getRecord IN hbodi154 (OUTPUT TABLE tt-ped-item).
        
        FIND FIRST tt-ped-item NO-ERROR.
        ASSIGN tt-ped-item.qt-pedida    = d-qtd
               tt-ped-item.qt-un-fat    = d-qtd               
               .
        IF tt-ped-item.dt-entrega <> dt-vencto THEN DO:
            ASSIGN tt-ped-item.dt-entrega = dt-vencto.
        END.

        RUN SetRecord IN hbodi154 (INPUT TABLE tt-ped-item).
        RUN updateRecord IN hbodi154.
        RUN GetRowErrors IN hbodi154 (OUTPUT TABLE RowErros).
        IF CAN-FIND(FIRST RowErros) THEN DO:

            OUTPUT STREAM st-erro TO c:\temp\erros_bodi154.txt APPEND.
            FOR EACH RowErros:
                EXPORT STREAM st-erro DELIMITER ";" RowErros .
            END.
            OUTPUT STREAM st-erro CLOSE.

            RUN destroyAll.
            UNDO , LEAVE.
        END.
    END.
    RUN destroyAll.
END PROCEDURE. /* alteraPedidoItem */

PROCEDURE adicionaPedidoItemEntrega:
    DEF INPUT  PARAM c-nome-abrev AS CHAR NO-UNDO.
    DEF INPUT  PARAM c-ped        AS CHAR NO-UNDO.
    DEF INPUT  PARAM i-seq        AS  INT NO-UNDO.
    DEF INPUT  PARAM c-it         AS CHAR NO-UNDO.
    DEF INPUT  PARAM dt-vencto    AS DATE NO-UNDO.
    DEF INPUT  PARAM d-qtd     AS DECIMAL NO-UNDO.
    DEF INPUT  PARAM d-preco   AS DECIMAL NO-UNDO.
    DEF INPUT  PARAM c-obs        AS CHAR NO-UNDO.
    DEF OUTPUT PARAM TABLE FOR RowErros.

    DEF VAR i-seqe AS INTEGER NO-UNDO.

    DEF VAR c-hora AS CHAR INIT "000000".

    FIND FIRST ITEM NO-LOCK WHERE ITEM.it-codigo = c-it NO-ERROR.
    IF NOT AVAIL ITEM THEN DO:
        MESSAGE "Erro Inesperado ! Item inválido !!!"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN "NOK".
    END.

    ASSIGN i-seqe = 0.
    FOR EACH ped-ent NO-LOCK WHERE ped-ent.nr-pedcli    = c-ped 
                               AND ped-ent.nr-sequencia = i-seq :
        IF i-seqe < ped-ent.nr-entrega THEN
            ASSIGN i-seqe = ped-ent.nr-entrega.
    END.
    ASSIGN i-seqe = i-seqe + 10.

    /* Crio Instancia da DBO */
    IF NOT VALID-HANDLE(hbodi149) THEN
        RUN dibo/bodi149.p    PERSISTENT SET hbodi149.

    IF VALID-HANDLE(hbodi149) THEN
        RUN openQueryStatic IN hbodi149 (INPUT "DEFAULT":U).
    ELSE DO:
        MESSAGE "Erro Inesperado !!! Não carregou bo !!!" 
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RUN destroyAll.
        RETURN "NOK".
    END.

    FIND FIRST emitente WHERE emitente.nome-abrev = c-nome-abrev NO-LOCK NO-ERROR.

    DO TRANSACTION:
        
        EMPTY TEMP-TABLE tt-ped-ent.

        RUN newRecord IN hbodi149.
        RUN getRecord IN hbodi149 (OUTPUT TABLE tt-ped-ent).

        /*ASSIGN c-hora = "000000".
        FOR EACH ped-ent WHERE ped-ent.nr-pedcli  = c-ped 
                           AND ped-ent.it-codigo  = c-it
                           AND ped-ent.dt-entrega = dt-vencto NO-LOCK: 
            IF c-hora < ped-ent.hr-entrega THEN
                ASSIGN c-hora = ped-ent.hr-entrega.
        END.
        ASSIGN c-hora = STRING(  INT(c-hora) + 10, "999999" ).*/
        ASSIGN c-hora = get-hr-entrega(c-ped, c-it, dt-vencto, "000000").

        FIND FIRST tt-ped-ent NO-ERROR.
        ASSIGN tt-ped-ent.nr-pedcli    = c-ped
               tt-ped-ent.nr-sequencia = i-seq
               tt-ped-ent.nr-entrega   = i-seqe
               tt-ped-ent.nr-ent-prog  = i-seqe
               tt-ped-ent.it-codigo    = c-it
               tt-ped-ent.nome-abrev   = emitente.nome-abrev
               tt-ped-ent.qt-pedida    = d-qtd
               tt-ped-ent.qt-un-fat    = d-qtd
               tt-ped-ent.tipo-atend   = 2 /* 1 = total, 2 = parcial */
               tt-ped-ent.cd-origem    = 2 /* 2 - EDI */
               tt-ped-ent.hr-entrega   = c-hora
               tt-ped-ent.tp-entrega   = 1
               tt-ped-ent.dt-entrega   = dt-vencto
               tt-ped-ent.observacao   = c-obs
               .
        FIND FIRST ped-item WHERE ped-item.nr-pedcli = c-ped 
                              AND ped-item.nr-sequencia = i-seq NO-ERROR.
        ASSIGN tt-ped-ent.r-rowid = ROWID(ped-item).
        
        RUN SetRecord IN hbodi149 (INPUT TABLE tt-ped-ent).
        RUN CreateRecord IN hbodi149.
        RUN GetRowErrors IN hbodi149 (OUTPUT TABLE RowErros).
        IF CAN-FIND(FIRST RowErros) THEN DO:

            OUTPUT STREAM st-erro TO c:\temp\erros_bodi149.txt APPEND.
            FOR EACH RowErros:
                EXPORT STREAM st-erro DELIMITER ";" RowErros .
            END.
            OUTPUT STREAM st-erro CLOSE.

            RUN destroyAll.
            UNDO , LEAVE.
        END.
    END.
    RUN destroyAll.
END PROCEDURE. /* adicionaPedidoItemEntrega */

PROCEDURE alteraPedidoItemEntrega:
    DEF INPUT  PARAM c-nome-abrev AS CHAR NO-UNDO.
    DEF INPUT  PARAM c-ped        AS CHAR NO-UNDO.
    DEF INPUT  PARAM i-sequencia  AS  INT NO-UNDO.
    DEF INPUT  PARAM i-entrega    AS  INT NO-UNDO.
    DEF INPUT  PARAM c-it         AS CHAR NO-UNDO.
    DEF INPUT  PARAM dt-vencto    AS DATE NO-UNDO.
    DEF INPUT  PARAM d-qtd     AS DECIMAL NO-UNDO.
    DEF OUTPUT PARAM TABLE FOR RowErros.

    DEF VAR c-hora AS CHAR NO-UNDO.

    FIND FIRST ITEM NO-LOCK WHERE ITEM.it-codigo = c-it NO-ERROR.
    IF NOT AVAIL ITEM THEN DO:
        MESSAGE "Erro Inesperado ! Item inválido !!!"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN "NOK".
    END.

    /* Crio Instancia da DBO */
    IF NOT VALID-HANDLE(hbodi149) THEN
        RUN dibo/bodi149.p    PERSISTENT SET hbodi149.

    IF VALID-HANDLE(hbodi149) THEN
        RUN openQueryStatic IN hbodi149 (INPUT "DEFAULT":U).
    ELSE DO:
        MESSAGE "Erro Inesperado !!! Não carregou bo !!!" VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RUN destroyAll.
        RETURN "NOK".
    END.

    /* Crio Instancia da DBO */
    IF NOT VALID-HANDLE(hbodi154) THEN
        RUN dibo/bodi154.p    PERSISTENT SET hbodi154.

    IF VALID-HANDLE(hbodi154) THEN
       RUN openQueryStatic IN hbodi154 (INPUT "DEFAULT":U).
    ELSE DO:
        MESSAGE "Erro Inesperado !!! Não Carregou bo !!!"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RUN destroyAll.
        RETURN "NOK".
    END.

    /* Crio Instancia da DBO */
    IF NOT VALID-HANDLE(hbodi159) THEN
        RUN dibo/bodi159.p    PERSISTENT SET hbodi159.
    
    IF VALID-HANDLE(hbodi159) THEN
        RUN openQueryStatic IN hbodi159 (INPUT "ChPedido":U).
    ELSE DO:
        MESSAGE "Erro Inesperado !!! Não carregou bo !!! " VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RUN destroyAll.
        RETURN "NOK".
    END.

    FIND FIRST emitente WHERE emitente.nome-abrev = c-nome-abrev NO-LOCK NO-ERROR.

    DO TRANSACTION:

        EMPTY TEMP-TABLE tt-ped-ent.

        RUN goToKey   IN hbodi159 (INPUT c-nome-abrev, 
                                   INPUT c-ped).
        RUN goToKey   IN hbodi154 (INPUT c-nome-abrev, 
                                   INPUT c-ped, 
                                   INPUT i-sequencia, 
                                   INPUT c-it, 
                                   INPUT "").
        RUN goToKey   IN hbodi149 (INPUT c-nome-abrev, 
                                   INPUT c-ped, 
                                   INPUT i-sequencia, 
                                   INPUT c-it, 
                                   INPUT "", 
                                   INPUT i-entrega).
        RUN getRecord IN hbodi149 (OUTPUT TABLE tt-ped-ent).
        
        FIND FIRST tt-ped-ent NO-ERROR.
        ASSIGN tt-ped-ent.qt-pedida    = d-qtd
               tt-ped-ent.qt-un-fat    = d-qtd               
               .
        IF tt-ped-ent.dt-entrega <> dt-vencto THEN DO:
            ASSIGN c-hora = get-hr-entrega(c-ped, c-it, dt-vencto, tt-ped-ent.hr-entrega).
            ASSIGN tt-ped-ent.dt-entrega = dt-vencto
                   tt-ped-ent.hr-entrega = c-hora.
        END.

        RUN SetRecord IN hbodi149 (INPUT TABLE tt-ped-ent).
        RUN updateRecord IN hbodi149.
        RUN GetRowErrors IN hbodi149 (OUTPUT TABLE RowErros).
        IF CAN-FIND(FIRST RowErros) THEN DO:

            OUTPUT STREAM st-erro TO c:\temp\erros_bodi149.txt APPEND.
            PUT UNFORMATTED 'Alterando ...' SKIP.
            FOR EACH RowErros:
                EXPORT STREAM st-erro DELIMITER ";" RowErros .
            END.
            OUTPUT STREAM st-erro CLOSE.

            RUN destroyAll.
            UNDO , LEAVE.
        END.
    END.
    RUN destroyAll.
END PROCEDURE. /* alteraPedidoItemEntrega */

PROCEDURE cancelaPedidoItemEntrega:
    DEF INPUT  PARAM c-nome-abrev AS CHAR NO-UNDO.
    DEF INPUT  PARAM c-ped        AS CHAR NO-UNDO.
    DEF INPUT  PARAM i-sequencia  AS  INT NO-UNDO.
    DEF INPUT  PARAM i-entrega    AS  INT NO-UNDO.
    DEF INPUT  PARAM c-it         AS CHAR NO-UNDO.
    DEF INPUT  PARAM dt-vencto    AS DATE NO-UNDO.
    DEF INPUT  PARAM dt-cancela   AS DATE NO-UNDO.
    DEF OUTPUT PARAM TABLE FOR RowErros.

    DEF VAR i-motivo AS INTEGER NO-UNDO INIT 1. /* 1 = cancelado, 2 = suspende, 3 = reativa*/

    FIND FIRST ITEM NO-LOCK WHERE ITEM.it-codigo = c-it NO-ERROR.
    IF NOT AVAIL ITEM THEN DO:
        MESSAGE "Erro Inesperado ! Item inválido !!!"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN "NOK".
    END.

    IF NOT VALID-HANDLE(hbodi149can) THEN
        RUN dibo/bodi149can.p PERSISTENT SET hbodi149can.

    FIND FIRST emitente WHERE emitente.nome-abrev = c-nome-abrev NO-LOCK NO-ERROR.

    DO TRANSACTION:
        
        FOR FIRST ped-ent WHERE ped-ent.nr-pedcli   = c-ped 
                           AND ped-ent.it-codigo    = c-it 
                           AND ped-ent.nr-sequencia = i-sequencia
                           AND ped-ent.nr-entrega   = i-entrega: END.

        FIND FIRST ped-item OF ped-ent NO-ERROR.
        FIND FIRST ped-venda OF ped-item NO-ERROR.

        IF AVAIL ped-ent THEN
            RUN updateCancelation   IN hbodi149can( INPUT ROWID(ped-ent), 
                                                    INPUT "Cancelamento via EDI", 
                                                    INPUT dt-cancela, 
                                                    INPUT i-motivo ).
        ELSE DO:
            MESSAGE "Erro Inesperado ! Pedido não encontrado !!!"
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
            RUN destroyAll.
            RETURN "NOK".
        END.

        RUN GetRowErrors IN hbodi149can (OUTPUT TABLE RowErros).
        IF CAN-FIND(FIRST RowErros) THEN DO:

            OUTPUT STREAM st-erro TO c:\temp\erros_bodi149.txt APPEND.
            PUT UNFORMATTED 'Cancelando ...' SKIP.
            FOR EACH RowErros:
                EXPORT STREAM st-erro DELIMITER ";" RowErros .
            END.
            OUTPUT STREAM st-erro CLOSE.

            RUN destroyAll.
            UNDO , LEAVE.
        END.
    END.
    RUN destroyAll.
END PROCEDURE. /* cancelaPedidoItemEntrega */
/*****************************************************************/
/* Destroi Instancias  */
PROCEDURE destroyAll:
    IF VALID-HANDLE(hbodi149) THEN DO:
        RUN destroyBo IN hbodi149 NO-ERROR.
        IF VALID-HANDLE(hbodi149) THEN DO:
            DELETE PROCEDURE hbodi149.
            hbodi149 = ?.
        END.
    END.
    IF VALID-HANDLE(hbodi149can) THEN DO:
        RUN destroyBo IN hbodi149can NO-ERROR.
        IF VALID-HANDLE(hbodi149can) THEN DO:
            DELETE PROCEDURE hbodi149can.
            hbodi149can = ?.
        END.
    END.
    IF VALID-HANDLE(hbodi159) THEN DO:
        RUN destroyBo IN hbodi159.
        IF VALID-HANDLE(hbodi159) THEN DO:
            DELETE PROCEDURE hbodi159.
            hbodi159 = ?.
        END.
    END.
    IF VALID-HANDLE(hbodi154) THEN DO:
        RUN destroyBo IN hbodi154.
        IF VALID-HANDLE(hbodi154) THEN DO:
            DELETE PROCEDURE hbodi154.
            hbodi154 = ?.
        END.
    END.
    IF VALID-HANDLE(hbodi154sdf) THEN DO:
        DELETE PROCEDURE hbodi154sdf.
        hbodi154sdf = ?.
    END.
    IF VALID-HANDLE(hbodi154cal) THEN DO:
        DELETE PROCEDURE hbodi154cal.
        hbodi154cal = ?.
    END.
END PROCEDURE.
/**
 *  Para a ..... é definido uma natureza de operação diferente
 *  para cada tipo de pedido. 
 *  O tipo de pedido são os 4 primeiros caracteres "numero" do pedido.
 ************************************************************************/
PROCEDURE pega-natureza-operacao :
    DEF INPUT        PARAM cod-estabel AS CHAR NO-UNDO.
    DEF INPUT        PARAM nr-pedcli AS CHAR NO-UNDO.
    DEF INPUT-OUTPUT PARAM nat-oper  AS CHAR NO-UNDO.

    ASSIGN nr-pedcli = REPLACE(nr-pedcli, "#", "").

    IF emitente.nat-oper <> "" THEN /* para os outros clientes usa o padrão */
        ASSIGN nat-oper = emitente.nat-oper.

    IF emitente.cod-emitente = 10000124 THEN DO:
        IF nr-pedcli BEGINS "QAPP" OR nr-pedcli BEGINS "QAPC" THEN
            ASSIGN nat-oper  = "5101CA".
        IF nr-pedcli BEGINS "QHPP" OR nr-pedcli BEGINS "QHPC" THEN
            ASSIGN nat-oper  = "5124QH".
        IF nr-pedcli BEGINS "QRPP" THEN
            ASSIGN nat-oper  = "5124".
        IF nr-pedcli BEGINS "QYPP" OR nr-pedcli BEGINS "QYPD" THEN
            ASSIGN nat-oper  = "5102".
    END. ELSE IF emitente.cod-emitente = 10005825 THEN DO:
        IF nr-pedcli = "QEST" THEN
            ASSIGN nat-oper  = "5101CB".
    END. ELSE IF emitente.cod-emitente = 10006602 THEN DO:
        IF nr-pedcli BEGINS "QAPD" THEN
            ASSIGN nat-oper  = IF cod-estabel = "100" THEN "6101CA"
                               ELSE "5101PA".
        IF nr-pedcli BEGINS "QHPD" THEN
           ASSIGN nat-oper  = IF cod-estabel = "100" THEN "6124QH"
                              ELSE "5124PA".
    END.

    IF emitente.cod-emitente = 10007155 THEN
        ASSIGN nat-oper = "6101".
END PROCEDURE.
/**
 *  Retorna a proxima hora de entrega (mais 10 segundos da anterior)
 ************************************************************************/
FUNCTION get-hr-entrega RETURNS CHAR (nr-pedcli AS CHAR, it-codigo AS CHAR, dt-entrega AS DATE, c-hora AS CHAR):
    IF c-hora = "" THEN 
        ASSIGN c-hora = "000000".
    FOR EACH ped-ent WHERE ped-ent.nr-pedcli  = nr-pedcli
                       AND ped-ent.it-codigo  = it-codigo
                       AND ped-ent.dt-entrega = dt-entrega NO-LOCK: 
        IF c-hora < ped-ent.hr-entrega THEN
            ASSIGN c-hora = ped-ent.hr-entrega.
    END.
    RETURN STRING( INT(c-hora) + 10, "999999" ).
END FUNCTION.

/*FIND FIRST emitente NO-LOCK WHERE emitente.cod-emitente = 10000326 NO-ERROR.*/


