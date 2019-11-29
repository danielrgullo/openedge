{cdp/cdcfgman.i}
{include/i_dbvers.i}

DEF TEMP-TABLE tt-reservas NO-UNDO
    FIELD proporcao AS DECIMAL 
    FIELD log-sem-saldo AS LOGICAL 
    FIELD nr-ord-produ LIKE ord-prod.nr-ord-produ
    FIELD cod-refer LIKE ITEM.cod-refer
    FIELD it-codigo LIKE ITEM.it-codigo
    FIELD quant-orig AS DECIMAL 
    FIELD quant-aloc AS DECIMAL 
    FIELD quant-atend AS DECIMAL 
    FIELD quant-calc AS DECIMAL 
    FIELD quant-requis AS DECIMAL 
    FIELD cod-depos LIKE deposito.cod-depos 
    FIELD cod-localiz LIKE mgcad.localiza.cod-localiz
    FIELD lote-serie LIKE saldo-estoq.lote 
    FIELD dt-vali-lote AS DATE 
    FIELD un LIKE ITEM.un
    FIELD estado AS integer
    FIELD tipo-sobra AS INTEGER 
    FIELD item-pai AS CHARACTER 
    FIELD op-codigo LIKE oper-ord.op-codigo
    FIELD cod-roteiro LIKE ord-prod.cod-roteiro
    FIELD veiculo AS LOGICAL 
    FIELD per-ppm AS DECIMAL 
    FIELD per-ppm-lote AS DECIMAL 
    FIELD tipo-formula AS INTEGER 
    FIELD qt-atend-lote AS DECIMAL 
    FIELD qt-requis-lote AS DECIMAL 
    FIELD qt-aloc-lote AS DECIMAL 
    FIELD alternativo-de AS CHARACTER 
    FIELD cod-refer-it-original AS CHARACTER 
    FIELD qt-atend-res AS DECIMAL 
    FIELD qt-atend-lote-res AS DECIMAL 
    FIELD quant-calc-orig AS DECIMAL 
    FIELD processada AS LOGICAL 
    FIELD rw-reserva AS ROWID 
    FIELD rw-saldo-estoq AS ROWID 
    FIELD tipo-ordem AS INTEGER 
    FIELD tempo AS INTEGER 
    FIELD tentativas AS INTEGER 
    FIELD sequencia AS INTEGER 
    INDEX seq nr-ord-produ sequencia.

DEF TEMP-TABLE tt-ref NO-UNDO
    FIELD line AS INTEGER 
    FIELD rw-split-operac AS ROWID
    FIELD nr-ord-prod AS INTEGER
    FIELD cod-motiv-refugo AS CHARACTER
    FIELD des-motiv-refugo AS CHARACTER
    FIELD codigo-rejei AS INTEGER
    FIELD qtd-operac-refgda AS DECIMAL
    FIELD qtd-operac-retrab AS DECIMAL.

DEF TEMP-TABLE tt-param-reporte NO-UNDO 
    FIELD cod-param AS CHARACTER 
    FIELD des-result-param AS CHARACTER 
    FIELD log-result-param AS LOGICAL
    FIELD dat-result-param AS DATE
    FIELD dec-result-param AS DECIMAL
    FIELD rw-result-param AS ROWID 
    FIELD handle-result-param AS HANDLE
    INDEX id cod-param.

DEF TEMP-TABLE tt-import-rep-oper NO-UNDO 
    FIELD tipo-rep AS INTEGER INIT 1
    FIELD nr-ord-produ AS INTEGER 
    FIELD num-operac-sfc AS INTEGER 
    FIELD num-split-operac AS INTEGER 
    FIELD cod-roteiro AS CHARACTER 
    FIELD op-codigo AS INTEGER 
    FIELD cod-ctrab AS CHARACTER 
    FIELD it-codigo AS CHARACTER 
    FIELD cod-refer AS CHARACTER 
    FIELD lote-serie AS CHARACTER 
    FIELD dat-valid-lote AS DATE 
    FIELD per-ppm AS DECIMAL 
    FIELD qtd-produzida AS DECIMAL 
    FIELD cod-motiv-refugo AS CHARACTER 
    FIELD qtd-refugada AS DECIMAL 
    FIELD qtd-refugada-un-ref AS DECIMAL 
    FIELD qtd-retrabalho AS DECIMAL 
    /*&IF "{&emsfnd_version}" < "1.0" &THEN
        FIELD conta-refugo AS CHARACTER 
    &ELSE*/
        FIELD ct-refugo AS CHARACTER
        FIELD sc-refugo AS CHARACTER
    /*&ENDIF*/
    FIELD dat-inic-rep AS DATE 
    FIELD hr-inic-rep AS DECIMAL
    FIELD dat-fim-rep AS DATE 
    FIELD hr-fim-rep AS DECIMAL 
    FIELD dat-inic-setup AS DATE 
    FIELD hr-inic-setup AS DECIMAL 
    FIELD dat-fim-setup AS DATE 
    FIELD hr-fim-setup AS DECIMAL 
    FIELD cod-equipe AS CHARACTER 
    FIELD cod-operador AS CHARACTER 
    FIELD cod-ferramenta AS CHARACTER 
    FIELD num-contador-ini AS INTEGER 
    FIELD num-contador-fim AS INTEGER 
    FIELD finaliza AS LOGICAL 
    FIELD linha AS INTEGER 
    FIELD erro AS LOGICAL 
    FIELD item-pai AS CHARACTER
    INDEX split tipo-rep it-codigo.

DEF TEMP-TABLE tt-reservas-final LIKE tt-reservas.


DEF TEMP-TABLE tt-apont-mob NO-UNDO
    FIELD nr-ord-prod AS INTEGER
    FIELD tipo-movto AS INTEGER /* 1 MOB ; 2 GGF */
    FIELD op-codigo AS INTEGER
    FIELD cod-roteiro AS CHAR
    FIELD it-codigo AS CHAR
    FIELD cb-mov-dir AS CHAR
    FIELD gm-codigo AS CHAR
    FIELD tipo-relogio AS INTEGER
    FIELD hora-ini AS INTEGER
    FIELD min-ini AS INTEGER
    FIELD hora-fim AS INTEGER
    FIELD min-fim AS INTEGER
    FIELD centesimal-ini AS DECIMAL
    FIELD centesimal-fim AS DECIMAL
    FIELD tempo AS DECIMAL
    FIELD minutos-report AS INTEGER
    FIELD referencia AS CHAR
    FIELD matr-func AS INTEGER
    FIELD nro-ord-seq AS INTEGER
    INDEX ordem nr-ord-prod tipo-movto.

DEF BUFFER b-saldo-estoq FOR saldo-estoq.
DEF BUFFER b-reservas FOR reservas.

DEF TEMP-TABLE RowErrors NO-UNDO 
    FIELD ErrorSequence AS INTEGER
    FIELD ErrorNumber   AS INTEGER 
    FIELD ErrorDescription AS CHAR FORMAT 'x(50)' /* <== */
    FIELD ErrorParameters AS CHAR  FORMAT 'x(50)'
    FIELD ErrorType AS CHAR  FORMAT 'x(50)'
    FIELD ErrorHelp AS CHAR FORMAT 'x(100)'  /* <== */
    FIELD ErrorSubType AS CHAR FORMAT 'x(50)'.

