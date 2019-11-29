DEF {1} TEMP-TABLE tt-ITP /* cabe‡alho */
    FIELD id-rnd         AS INTEGER
    FIELD versao-rnd     AS INTEGER
    FIELD num-controle   AS INTEGER
    FIELD id-movimento   AS CHAR FORMAT "x(12)" /* AAMMDD HHMMSS */
    FIELD id-transmis    AS CHAR FORMAT "x(14)" /* CGC */
    FIELD ident-receptor AS CHAR FORMAT "x(14)" /* CGC */
    FIELD cod-transmis   AS CHAR FORMAT "x(8)"
    FIELD cod-receptor   AS CHAR FORMAT "x(8)"
    FIELD nome-transmis  AS CHAR FORMAT "x(25)"
    FIELD nome-receptor  AS CHAR FORMAT "x(25)"
    .


DEF {1} TEMP-TABLE tt-PP1 /* identificacao geral do pedido */
    FIELD qtd-itens          AS INTEGER   FORMAT ">>>9"
    FIELD qtd-decimais       AS INTEGER   FORMAT "9"
    FIELD numero-pedido      AS CHARACTER FORMAT "x(12)"
    FIELD data-emissao       AS DATE      FORMAT "99/99/9999"
    FIELD id-versao-pedido   AS CHARACTER FORMAT "x(12)"
    FIELD dt-inicio-pedido   AS DATE      FORMAT "99/99/9999"
    FIELD id-contato         AS CHARACTER FORMAT "x(11)"
    FIELD via-transporte     AS CHARACTER FORMAT "x(3)"
    FIELD condicao-frete     AS CHARACTER FORMAT "x(3)"
    FIELD cod-unidade-moeda  AS CHARACTER FORMAT "x(3)"
    FIELD cod-cond-pagto     AS CHARACTER FORMAT "x(4)"
    FIELD percent-desconto   AS DECIMAL   FORMAT "99.99"
    .
DEF {1} TEMP-TABLE tt-PP2 /* identificacao do item */ 
    FIELD codigo-refer      AS CHARACTER FORMAT "x(30)"
    FIELD qtd-entrega       AS INTEGER   FORMAT ">>>>>>>>9"
    FIELD descricao         AS CHARACTER FORMAT "x(25)"
    FIELD valor-unitario    AS DECIMAL   FORMAT ">>>>>>9.99999"
    FIELD cod-lista-preco   AS CHARACTER FORMAT "x(6)"
    FIELD numero-item       AS INTEGER   FORMAT "9999"
    FIELD data-entrega      AS DATE      FORMAT "99/99/9999"
    FIELD cod-class-fiscal  AS INTEGER   FORMAT ">>>>>>>>>9"
    FIELD aliquota-ipi      AS DECIMAL   FORMAT "99.99"
    FIELD percent-desconto  AS DECIMAL   FORMAT "99.99"
    FIELD cod-cond-pagto    AS CHARACTER FORMAT "x(4)"
    FIELD r-ROWID           AS ROWID.
DEF {1} TEMP-TABLE tt-AE3 /* dados adicionais de comercializa‡Æo */ 
    FIELD local-faturamento AS INTEGER   FORMAT ">>>>>>>>>>>>>9"
    FIELD local-cobranca    AS INTEGER   FORMAT ">>>>>>>>>>>>>9"
    FIELD local-entrega     AS INTEGER   FORMAT ">>>>>>>>>>>>>9"
    FIELD tipo-transporte   AS CHARACTER FORMAT "x(4)"
    FIELD local-vai-vem     AS CHARACTER FORMAT "x(14)"
    FIELD fluxo-carro       AS CHARACTER FORMAT "x(1)"
    FIELD r-ROWID           AS ROWID.
DEF {1} TEMP-TABLE tt-PE4 /* dados da embalagem */ 
    FIELD embalagem2-cli    AS CHARACTER FORMAT "x(30)"
    FIELD embalagem2-for    AS CHARACTER FORMAT "x(30)"
    FIELD capac-embalagem2  AS DECIMAL   FORMAT ">>>>>>>>9.999"
    FIELD embalagem1-for    AS CHARACTER FORMAT "x(30)"
    FIELD capac-embalagem1  AS DECIMAL   FORMAT ">>>>>>>>9.999"
    FIELD respons-embalagem AS CHARACTER FORMAT "x(1)"
    FIELD r-ROWID           AS ROWID.
/*DEF {1} TEMP-TABLE tt-PE1 /*dados do item*/
    FIELD cod-fabrica-dest	 AS CHAR FORMAT "x(3)" LABEL "Cod Fabrica"
    FIELD id-programa  		 AS CHAR FORMAT "x(9)" LABEL "ID Programa"
    FIELD dt-prg-atu		 AS DATE FORMAT "99/99/9999" LABEL "Dt Programa"
    FIELD id-programa-ant    AS CHAR FORMAT "x(9)" LABEL "ID Programa Ant."
    FIELD dt-prg-ant		 AS DATE FORMAT "99/99/9999" LABEL "Dt Programa Ant."
    FIELD cod-it-cliente	 AS CHAR FORMAT "x(30)" LABEL "Codigo Item"
    FIELD cod-it-fornecedor	 AS CHAR FORMAT "x(30)" LABEL "Codigo Item CCS"
    FIELD num-ped-compra	 AS CHAR FORMAT "x(12)" LABEL "Pedido de compra"
    FIELD cod-local-destino	 AS CHAR FORMAT "x(5)"  LABEL "Local destino"
    FIELD ident-contato		 AS CHAR FORMAT "x(11)" LABEL "Contato"
    FIELD cod-unidade-medida AS CHAR FORMAT "x(2)"  LABEL "Un. Medida"
    FIELD qt-casas-decimais  AS INT  FORMAT "9"     LABEL "Qtd Casas Dec."
    FIELD cod-tipo-fornec	 AS CHAR FORMAT "x(1)"  LABEL "Tipo Fornec."
    .
DEF {1} TEMP-TABLE tt-PE2 /* inform. de entregas/embarque */
    FIELD dt-ult-entrega	AS DATE FORMAT "99/99/9999" LABEL "Dt Ult Entrega"
    FIELD num-ult-nf		AS CHAR FORMAT "x(6)"       LABEL "Ult NF"
    FIELD serie-ult-nf	    AS CHAR FORMAT "x(4)"       LABEL "Serie Ult NF"
    FIELD dt-ult-nf		    AS DATE FORMAT "99/99/9999" LABEL "Dt Ult NF"
    FIELD qt-ult-entrega	AS DECIMAL FORMAT ">>>>>>>>9.999"   LABEL "Qtd Ult Entrega"
    FIELD qt-entrega-acu    AS DECIMAL FORMAT ">>>>>>>>>>9.999" LABEL "Qtd Entr. Acum."
    FIELD qt-necessa-acu    AS DECIMAL FORMAT ">>>>>>>>>>9.999" LABEL "Qtd Necessidade"
    FIELD qt-lote-minimo	AS DECIMAL FORMAT ">>>>>>>>9.999"   LABEL "Qtd Lote Min."
    FIELD cod-freq-fornec   AS CHAR FORMAT "x(3)"       LABEL "Cod Frequencia"
    FIELD dt-liberacao	    AS DATE FORMAT "99/99/9999" LABEL "Dt Libera‡Æo"
    FIELD dt-libera-mp	    AS DATE FORMAT "99/99/9999" LABEL "Dt Libera‡Æo MP"
    FIELD cod-local-desc	AS CHAR FORMAT "x(7)"       LABEL "Cod Local Destino"
    FIELD periodo-embarq	AS CHAR FORMAT "x(4)"       LABEL "Per¡odo Embarque"
    FIELD cod-sit-item	    AS CHAR FORMAT "x(2)"       LABEL "Cod Situa‡Æo Item"
    FIELD id-tipo-prg		AS CHAR FORMAT "x(1)"       LABEL "ID Tipo Programa"
    FIELD pedido-reven	    AS CHAR FORMAT "x(13)"      LABEL "Pedido Revenda"
    FIELD qualific-prg	    AS CHAR FORMAT "x(1)"       LABEL "Qualific. Progr."
    FIELD tipo-ped-reven	AS CHAR FORMAT "x(2)"       LABEL "Tipo Pedido Reven."
    FIELD cod-via-transp    AS CHAR FORMAT "x(3)"       LABEL "Cod. Via Transp."
    FIELD r-ROWID           AS ROWID
    INDEX id r-ROWID
    .
DEF {1} TEMP-TABLE tt-PE3 /*cronograma de entrega/embarque*/
    FIELD ind               AS INTEGER
    FIELD dt-entrega        AS DATE FORMAT "99/99/9999"   LABEL "Data Entrega" EXTENT 7
    FIELD hr-entrega        AS INTEGER FORMAT "99"        LABEL "Hora Entrega" EXTENT 7
    FIELD qt-entrega        AS INTEGER FORMAT ">>>>>>>>9" LABEL "Quantidade"   EXTENT 7
    FIELD r-ROWID           AS ROWID
    INDEX id r-ROWID ind
    .
DEF {1} TEMP-TABLE tt-PE5 /*comple cronog entrega/embarque*/
    FIELD ind               AS INTEGER
    FIELD dt-inic-entrega	AS DATE FORMAT "99/99/9999" LABEL "Data Janela"     EXTENT 7
    FIELD id-programacao	AS CHAR FORMAT "x(1)"       LABEL "Status Programa" EXTENT 7
    FIELD id-prg-atual	    AS CHAR FORMAT "x(9)"       LABEL "ID Programa"     EXTENT 7
    FIELD r-ROWID           AS ROWID
    INDEX id r-ROWID ind
    .
DEF {1} TEMP-TABLE tt-PE6 /*dados complementares do item*/
    FIELD fat-conversao	    AS DECIMAL FORMAT ">>>>9.99999"   LABEL "Fator conversÆo"
    FIELD altera-tecnica	AS CHAR    FORMAT "x(4)"          LABEL "Alt. Tecnica"
    FIELD cod-material	    AS CHAR    FORMAT "x(10)"         LABEL "Codigo Mat."
    FIELD peso-item		    AS DECIMAL FORMAT ">>>>>>>>9.999" LABEL "Peso"
    FIELD un-medida-peso    AS CHAR    FORMAT "x(2)"          LABEL "Un. Med. Peso"
    FIELD r-ROWID           AS ROWID
    INDEX id r-ROWID
    .*/

DEF {1} TEMP-TABLE tt-TE1 /* texto livre */
    FIELD txt-informa       AS CHAR FORMAT "x(120)" LABEL "Texto Livre"
    FIELD r-ROWID           AS ROWID
    INDEX id r-ROWID
    .
