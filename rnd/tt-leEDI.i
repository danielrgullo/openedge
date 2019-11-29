DEF TEMP-TABLE tt-edi NO-UNDO
    FIELD id           AS CHAR
    FIELD cod-emitente LIKE emitente.cod-emitente
    FIELD dt-leitura   AS DATE
    .
DEF TEMP-TABLE tt-pedido NO-UNDO
    FIELD nr-pedcli   LIKE ped-venda.nr-pedcli
    FIELD codigo-refer   AS CHAR
    FIELD c-alt-tec   AS CHAR
    FIELD item-ccs    AS CHAR
    FIELD rev         AS CHAR
    FIELD c-estado    AS CHAR FORMAT 'x(40)' COLUMN-LABEL 'Status'
    FIELD c-tipoforn  AS CHAR
    FIELD cod-situa   AS CHAR
    FIELD ult-nf      AS CHAR FORMAT 'x(6)'
    FIELD dt-ult-nf   AS DATE
    FIELD dt-ult-ent  AS DATE
    FIELD qtd-ult-ent AS DECIMAL
    FIELD qtd-acum    AS DECIMAL
    FIELD cod-emitente AS INTEGER
    INDEX ipedcli nr-pedcli
    .
DEF TEMP-TABLE tt-ped-item NO-UNDO
    FIELD codigo-refer  AS CHAR    COLUMN-LABEL 'Item'    
    FIELD c-alt-tec  AS CHAR
    FIELD item-ccs   AS CHAR
    FIELD i-linha    AS INTEGER
    FIELD i-indice   AS INTEGER
    FIELD i-estado   AS INT     COLUMN-LABEL 'Status'
    FIELD nr-pedcli  AS CHAR    COLUMN-LABEL 'Pedido'
    FIELD dt-janela  AS DATE    COLUMN-LABEL 'Data!Janela'
    FIELD hr-entrega AS CHAR    COLUMN-LABEL 'Hora!Entrega'
    FIELD dt-entrega AS DATE    COLUMN-LABEL 'Data!Entrega'
    FIELD qt-pedida  AS INTEGER COLUMN-LABEL 'Qtd'
    FIELD l-alterado AS LOGICAL INIT FALSE
    FIELD c-obs      AS CHAR 
    FIELD c-obs-entr AS CHAR
    INDEX ipedclii nr-pedcli i-indice
    .
DEF TEMP-TABLE tt-ped-obs NO-UNDO
    FIELD nr-pedcli    AS CHAR
    FIELD narrativa    AS CHAR FORMAT 'x(70)'
    INDEX ipedcli nr-pedcli
    .
DEF TEMP-TABLE tt-aux-pe2 NO-UNDO
    FIELD nr-pedcli   AS CHAR
    FIELD codigo-refer   AS CHAR 
    FIELD c-estado    AS CHAR FORMAT 'x(40)' COLUMN-LABEL 'Status'
    FIELD cod-situa   AS CHAR FORMAT 'x(2)'
    FIELD ult-nf      AS CHAR FORMAT 'x(6)'
    FIELD dt-ult-nf   AS DATE
    FIELD dt-ult-ent  AS DATE
    FIELD qtd-ult-ent AS DECIMAL
    FIELD qtd-acum    AS DECIMAL
    INDEX id codigo-refer
    .
DEFINE TEMP-TABLE RowErros NO-UNDO 
   FIELD errorSequence    AS INTEGER
   FIELD errorNumber      AS INTEGER
   FIELD errorDescription AS CHARACTER FORMAT 'x(60)'
   FIELD errorParameters  AS CHARACTER FORMAT 'x(60)'
   FIELD errorType        AS CHARACTER FORMAT 'x(60)'
   FIELD errorHelp        AS CHARACTER FORMAT 'x(60)'
   FIELD errorsubtype     AS CHARACTER FORMAT 'x(60)'
    .
