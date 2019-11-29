DEF {1} TEMP-TABLE tt-ITP /* cabe‡alho */
    FIELD id-rnd         AS INTEGER
    FIELD versao-rnd     AS INTEGER
    FIELD num-controle   AS INTEGER
    FIELD id-movimento   AS CHAR FORMAT "x(12)" /* AAMMDD HHMMSS */
    FIELD id-transmis    AS CHAR FORMAT "x(14)" /* CGC */
    FIELD id-receptor    AS CHAR FORMAT "x(14)" /* CGC */
    FIELD cod-transmis   AS CHAR FORMAT "x(8)"
    FIELD cod-receptor   AS CHAR FORMAT "x(8)"
    FIELD nome-transmis  AS CHAR FORMAT "x(25)"
    FIELD nome-receptor  AS CHAR FORMAT "x(25)"
    .

DEF {1} TEMP-TABLE tt-PD1 /* Prog. de Peca/Material Diaria  */
    FIELD codigo-refer       AS CHARACTER FORMAT "x(30)"
    FIELD resp-embalagem     AS CHARACTER FORMAT "x(1)"
    FIELD it-codigo          AS CHARACTER FORMAT "x(30)"
    FIELD cod-local-destino  AS CHARACTER FORMAT "x(5)"
    FIELD num-ult-nf		 AS CHARACTER FORMAT "x(6)" LABEL "Ult NF"
    FIELD serie-ult-nf	     AS CHARACTER FORMAT "x(4)" LABEL "Serie Ult NF"
    FIELD dt-ult-nf		     AS DATE FORMAT "99/99/9999" LABEL "Dt Ult NF"
    FIELD qt-entrega-acum    AS DECIMAL FORMAT ">>>>>>>>>>9.999" LABEL "Qtd Entr. Acum."
    FIELD dt-entrega-acum    AS DATE FORMAT "99/99/9999"
    FIELD qt-casas-decimais  AS INTEGER FORMAT "9"
    FIELD outras-neces       AS INTEGER FORMAT ">>>>>>>>9"
    FIELD id-tipo-programa   AS INTEGER FORMAT "9" /* 1 - P.Entrega, 2 - P.Embarque */
    FIELD proposito-trans    AS CHARACTER FORMAT "x(2)" /* 00:Primeira Transmissao, 04:Atualizacao, 05:Substituicao */
    FIELD unid-medida        AS CHARACTER FORMAT "x(2)"
    FIELD cod-tipo-forn      AS CHARACTER FORMAT "x(1)" /* P=Producao R=Reposicao T=Triangulacao E=Exportacao X=Outros A=Amostra F=Ferramentas e Solucoes */
    FIELD altera-tecnica	 AS CHARACTER FORMAT "x(4)" LABEL "Alt. Tecnica"
    .
DEF {1} TEMP-TABLE tt-PD2 /* Cronograma de Entrega/Embarque */ 
    FIELD ind               AS INTEGER
    FIELD dt-entrega        AS DATE FORMAT "99/99/9999"   LABEL "Data Entrega" EXTENT 6
    FIELD hr-entrega        AS INTEGER FORMAT "9999"      LABEL "Hora Entrega" EXTENT 6
    FIELD qt-entrega        AS INTEGER FORMAT ">>>>>>>>9" LABEL "Quantidade"   EXTENT 6
    FIELD r-ROWID           AS ROWID
    .
DEF {1} TEMP-TABLE tt-EP1 /* ESPECIFICACAO DA PROGRAMACAO   */ 
    FIELD cod-fab-destino   AS CHARACTER FORMAT "x(3)"
    FIELD id-programa-atu   AS CHARACTER FORMAT "x(9)"
    FIELD dt-programa-atu   AS DATE      FORMAT "99/99/9999"
    FIELD id-programa-ant   AS CHARACTER FORMAT "x(9)"
    FIELD dt-programa-ant   AS DATE      FORMAT "99/99/9999"
    FIELD qt-nesses-acum    AS DECIMAL   FORMAT ">>,>>>,>>>,>>9.999"
    FIELD cod-freq-forn     AS CHARACTER FORMAT "x(3)"
    FIELD nr-ped-compra     AS CHARACTER FORMAT "x(12)"
    FIELD cod-loc-material  AS CHARACTER FORMAT "x(10)"
    FIELD r-ROWID           AS ROWID
    .
DEF {1} TEMP-TABLE tt-PD3 /* ESPECIFICACAO P/ ENTREGA PECA */ 
    FIELD chassi            AS CHARACTER FORMAT "x(21)"
    FIELD pedido-cliente    AS CHARACTER FORMAT "x(12)"
    FIELD pedido-fornecedor AS CHARACTER FORMAT "x(15)"
    FIELD descricao-item    AS CHARACTER FORMAT "x(25)"
    FIELD local-descarga    AS CHARACTER FORMAT "x(7)"
    FIELD num-seq-montagem  AS CHARACTER FORMAT "x(10)"
    FIELD esp-tecnica-mod   AS CHARACTER FORMAT "x(25)"
    FIELD r-ROWID           AS ROWID
    .
/* TODO: 
DEF {1} TEMP-TABLE tt-PD4 /* COMPOSICAO DA PECA/MATERIAL */ 
    FIELD r-ROWID           AS ROWID
    .
DEF {1} TEMP-TABLE tt-PD5 /* DADOS PARA CHAMADA DO KANBAN */ 
    FIELD r-ROWID           AS ROWID
    .
*/
DEF {1} TEMP-TABLE tt-TE1 /* texto livre */
    FIELD txt-informa       AS CHAR FORMAT "x(120)" LABEL "Texto Livre"
    FIELD r-ROWID           AS ROWID
    INDEX id r-ROWID
    .
