{cdp/cdcfgman.i}
{include/i_dbvers.i}
{cpp/cpapi001.i}
{cdp/cd0666.i}


DEF TEMP-TABLE tt-reservas NO-UNDO
    FIELD proporcao             AS DECIMAL 
    FIELD log-sem-saldo         AS LOGICAL 
    FIELD nr-ord-produ          LIKE ord-prod.nr-ord-produ
    FIELD cod-refer             LIKE ITEM.cod-refer
    FIELD it-codigo             LIKE ITEM.it-codigo
    FIELD quant-orig            AS DECIMAL 
    FIELD quant-aloc            AS DECIMAL 
    FIELD quant-atend           AS DECIMAL 
    FIELD quant-calc            AS DECIMAL 
    FIELD quant-requis          AS DECIMAL 
    FIELD cod-depos             LIKE deposito.cod-depos 
    FIELD cod-localiz           LIKE mgcad.localiza.cod-localiz
    FIELD lote-serie            LIKE saldo-estoq.lote 
    FIELD dt-vali-lote          AS DATE FORMAT "99/99/9999"
    FIELD un                    LIKE ITEM.un
    FIELD estado                AS INTEGER
    FIELD tipo-sobra            AS INTEGER 
    FIELD item-pai              AS CHARACTER FORMAT "x(16)"
    FIELD op-codigo             LIKE oper-ord.op-codigo
    FIELD cod-roteiro           LIKE ord-prod.cod-roteiro
    FIELD veiculo               AS LOGICAL 
    FIELD per-ppm               AS DECIMAL 
    FIELD per-ppm-lote          AS DECIMAL 
    FIELD tipo-formula          AS INTEGER 
    FIELD qt-atend-lote         AS DECIMAL 
    FIELD qt-requis-lote        AS DECIMAL 
    FIELD qt-aloc-lote          AS DECIMAL 
    FIELD alternativo-de        AS CHARACTER 
    FIELD cod-refer-it-original AS CHARACTER 
    FIELD qt-atend-res          AS DECIMAL 
    FIELD qt-atend-lote-res     AS DECIMAL 
    FIELD quant-calc-orig       AS DECIMAL 
    FIELD processada            AS LOGICAL 
    FIELD rw-reserva            AS ROWID 
    FIELD rw-saldo-estoq        AS ROWID 
    FIELD tipo-ordem            AS INTEGER 
    FIELD tempo                 AS INTEGER 
    FIELD tentativas            AS INTEGER 
    FIELD sequencia             AS INTEGER 
    INDEX seq nr-ord-produ sequencia.
/*
&IF "{&emsfnd_version}" >= "1.0"  &THEN   
   
   DEF TEMP-TABLE tt-rep-prod 
       FIELD tipo AS INTEGER INIT 1
       FIELD nr-reporte AS INTEGER
       FIELD nr-ord-produ AS INTEGER
       FIELD data AS DATE
       FIELD qt-reporte AS DECIMAL
       FIELD qt-refugo AS DECIMAL
       FIELD qt-apr-cond AS DECIMAL
       FIELD it-codigo AS CHARACTER
       FIELD un AS CHARACTER
       FIELD nro-docto AS CHARACTER
       FIELD serie-docto AS CHARACTER
       FIELD cod-depos AS CHARACTER
       FIELD cod-localiz AS CHARACTER
       FIELD dep-refugo AS CHARACTER
       FIELD loc-refugo AS CHARACTER
       FIELD per-ppm AS DECIMAL
       FIELD lote-serie AS CHARACTER
       FIELD cod-refer AS CHARACTER
       FIELD dt-vali-lote AS DATE
       FIELD ct-codigo AS CHARACTER
       FIELD sc-codigo AS CHARACTER
       FIELD cod-cta-unif AS CHARACTER
       FIELD cod-ccusto-unif AS CHARACTER
       FIELD ct-refugo AS CHARACTER
       FIELD sc-refugo AS CHARACTER
       FIELD cod-depos-sai AS CHARACTER
       FIELD cod-local-sai AS CHARACTER
       FIELD op-codigo AS INTEGER
       FIELD cod-roteiro AS CHARACTER
       FIELD it-oper AS CHARACTER
       FIELD pto-controle AS INTEGER
       FIELD sequencia AS INTEGER
       FIELD baixa-reservas AS INTEGER INIT 1
       FIELD time-out AS INTEGER INIT 30
       FIELD tentativas AS INTEGER INIT 10
       FIELD procura-saldos AS LOGICAL INIT YES
       FIELD carrega-reservas AS LOGICAL INIT YES
       FIELD requis-automatica AS LOGICAL
       FIELD prog-seg AS CHARACTER
       FIELD finaliza-ordem AS LOGICAL
       FIELD finaliza-oper AS LOGICAL
       FIELD reserva AS LOGICAL INIT YES
       FIELD nro-ord-seq AS INTEGER
       FIELD linha AS INTEGER
       FIELD cod-versao-integracao AS INTEGER
       FIELD cod-emitente AS INTEGER
       FIELD nat-operacao AS CHARACTER
       INDEX codigo IS PRIMARY UNIQUE nr-ord-produ nro-ord-seq
       INDEX reserva reserva.
&ELSE

   DEF TEMP-TABLE tt-rep-prod 
       FIELD tipo              AS INTEGER INIT 1
       FIELD nr-reporte        LIKE rep-prod.nr-reporte
       FIELD nr-ord-produ      LIKE ord-prod.nr-ord-produ
       FIELD data              LIKE rep-prod.data
       FIELD qt-reporte        LIKE rep-prod.qt-reporte
       FIELD qt-refugo         LIKE rep-prod.qt-refugo
       FIELD qt-apr-cond       LIKE rep-prod.qt-apr-cond    
       FIELD it-codigo         LIKE rep-prod.it-codigo
       FIELD un                LIKE ord-prod.un    
       FIELD nro-docto         LIKE rep-prod.nro-docto
       FIELD serie-docto       LIKE rep-prod.serie-docto
       FIELD cod-depos         LIKE rep-prod.cod-depos
       FIELD cod-localiz       LIKE rep-prod.cod-localiz
       FIELD dep-refugo        LIKE rep-prod.cod-depos
       FIELD loc-refugo        LIKE rep-prod.cod-localiz
       FIELD per-ppm           LIKE item.per-ppm
       FIELD lote-serie        LIKE rep-prod.lote-serie
       FIELD cod-refer         LIKE rep-prod.cod-refer
       FIELD dt-vali-lote      LIKE rep-prod.dt-vali-lote
       FIELD conta-contabil    LIKE ord-prod.conta-ordem
       FIELD conta-refugo      LIKE rep-prod.conta-refugo
       FIELD conta-aplicacao   LIKE rep-prod.conta-aplicacao
       FIELD cod-depos-sai     LIKE rep-prod.cod-depos-sai
       FIELD cod-local-sai     LIKE rep-prod.cod-local-sai
       FIELD op-codigo         LIKE oper-ord.op-codigo
       FIELD cod-roteiro       LIKE oper-ord.cod-roteiro
       FIELD it-oper           LIKE oper-ord.it-codigo
       FIELD pto-controle      LIKE oper-ord.pto-controle
       FIELD sequencia         LIKE oper-ord.sequencia  
       FIELD baixa-reservas    AS INTEGER INIT 1
       FIELD time-out          AS INTEGER INIT 30
       FIELD tentativas        AS INTEGER INIT 10
       FIELD procura-saldos    AS LOGICAL INIT YES 
       FIELD carrega-reservas  AS LOGICAL INIT YES 
       FIELD requis-automatica AS LOGICAL
       FIELD prog-seg          AS CHAR
       FIELD finaliza-ordem    AS LOGICAL
       FIELD finaliza-oper     AS LOGICAL
       FIELD reserva           AS LOGICAL INIT YES 
       FIELD nro-ord-seq       AS INTEGER
       FIELD linha             AS INTEGER
       FIELD cod-versao-integracao AS INTEGER FORMAT "999"
       INDEX codigo IS PRIMARY UNIQUE nr-ord-produ nro-ord-seq
       INDEX reserva reserva.
&ENDIF       
    
DEF TEMP-TABLE tt-refugo
    FIELD nr-ord-produ LIKE ord-prod.nr-ord-produ
    FIELD codigo-rejei LIKE cod-rejeicao.codigo-rejei
    FIELD qt-refugo    LIKE ord-prod.qt-refugada
    FIELD observacao   LIKE ref-ordem.observacao
    FIELD nro-ord-seq  AS INTEGER INIT 0
    INDEX ordem-rejei  nr-ord-produ codigo-rejei.

DEF TEMP-TABLE tt-res-neg    
    FIELD nr-ord-produ LIKE ord-prod.nr-ord-produ
    FIELD it-codigo    LIKE reservas.it-codigo
    FIELD quant-requis LIKE reservas.quant-orig
    FIELD cod-depos    LIKE reservas.cod-depos
    FIELD cod-localiz  LIKE item.cod-localiz
    FIELD lote-serie   LIKE ord-prod.lote-serie
    FIELD cod-refer    LIKE ord-prod.cod-refer
    FIELD dt-vali-lote LIKE saldo-estoq.dt-vali-lote
    FIELD positivo     AS LOGICAL FORMAT "Sim/Nao"
    FIELD nro-ord-seq  AS INTEGER INIT 0    
    INDEX codigo IS PRIMARY nr-ord-produ it-codigo cod-depos
    INDEX indicador positivo.

DEF TEMP-TABLE tt-apont-mob
    FIELD nr-ord-prod    LIKE ord-prod.nr-ord-prod
    FIELD tipo-movto     AS INT FORMAT "99"
    FIELD op-codigo      LIKE movto-ggf.op-codigo
    FIELD cod-roteiro    LIKE movto-ggf.cod-roteiro
    FIELD it-codigo      LIKE operacao.it-codigo
    FIELD cd-mob-dir     LIKE operacao.cd-mob-dir
    FIELD gm-codigo      LIKE operacao.gm-codigo
    FIELD tipo-relogio   AS INT 
    FIELD hora-ini       AS INT FORMAT "9999"
    FIELD min-ini        AS INT FORMAT "99"
    FIELD hora-fim       AS INT FORMAT "9999"
    FIELD min-fim        AS INT FORMAT "99"
    FIELD centesimal-ini AS DEC FORMAT ">>>>>>>9,9999"
    FIELD centesimal-fim AS DEC FORMAT ">>>>>>>9,9999"   
    FIELD tempo          LIKE movto-ggf.horas-report 
    FIELD minutos-report AS INT FORMAT "99"
    FIELD referencia     LIKE movto-ggf.referencia
    FIELD matr-func      LIKE movto-ggf.matr-func
    FIELD nro-ord-seq    AS INTEGER 
    INDEX relogio tipo-relogio
    INDEX ordem IS PRIMARY nr-ord-prod tipo-movto.   
*/
DEF TEMP-TABLE tt-reservas-final LIKE tt-reservas .

DEF BUFFER b-split-operac FOR split-operac.
DEF BUFFER b-ord-prod FOR ord-prod.
DEF BUFFER b-oper-ord FOR oper-ord.
DEF BUFFER b-item FOR ITEM.
DEF BUFFER b-saldo-estoq FOR saldo-estoq.
DEF BUFFER b-reservas FOR reservas.

DEF VAR h-cpapi001   AS HANDLE NO-UNDO.
DEF VAR cod-depos    AS CHAR NO-UNDO.
DEF VAR quant-requis AS DECIMAL NO-UNDO.
DEF VAR qt-req-aux   AS DECIMAL NO-UNDO.
DEF VAR seq          AS INTEGER NO-UNDO INIT 1.
DEF VAR apto-parcial AS LOGICAL NO-UNDO INIT NO.

DEF INPUT PARAM tipo-report  LIKE ord-prod.rep-prod    NO-UNDO.
DEF INPUT PARAM nr-ord-prod  LIKE ord-prod.nr-ord-prod NO-UNDO.
DEF INPUT PARAM op-codigo    LIKE oper-ord.op-codigo   NO-UNDO.
DEF INPUT PARAM qt-reporte   AS DECIMAL                NO-UNDO.
DEF INPUT PARAM requisita    AS LOGICAL                NO-UNDO INIT NO.
DEF INPUT PARAM deposito-pad LIKE deposito.cod-depos   NO-UNDO.
DEF OUTPUT PARAM TABLE FOR tt-erro.


FIND FIRST b-ord-prod NO-LOCK 
    WHERE b-ord-prod.nr-ord-prod = nr-ord-prod NO-ERROR.
IF NOT AVAIL b-ord-prod THEN
    RETURN "NOK".

FIND LAST b-split-operac NO-LOCK 
    WHERE b-split-operac.nr-ord-prod = nr-ord-prod
      AND b-split-operac.op-codigo   = op-codigo  NO-ERROR.
IF NOT AVAIL b-split-operac THEN DO:
    RETURN "NOK".
END.

ASSIGN apto-parcial = b-ord-prod.qt-ordem > (b-ord-prod.qt-produ + b-ord-prod.qt-refug + qt-reporte).

FIND FIRST b-oper-ord OF b-split-operac NO-LOCK NO-ERROR.
FIND FIRST b-item OF b-split-operac NO-LOCK NO-ERROR.

IF deposito-pad <> "" THEN
    ASSIGN cod-depos = deposito-pad.
ELSE
    ASSIGN cod-depos = b-item.deposito-pad.

CREATE tt-rep-prod.
ASSIGN tt-rep-prod.nr-ord-prod = b-split-operac.nr-ord-prod
       tt-rep-prod.qt-reporte = qt-reporte
       tt-rep-prod.nro-docto = STRING(b-split-operac.nr-ord-prod)
       tt-rep-prod.tipo = tipo-report
       tt-rep-prod.data = TODAY
       tt-rep-prod.it-codigo = b-split-operac.it-codigo
       tt-rep-prod.un = b-item.un
       tt-rep-prod.cod-depos = cod-depos
       tt-rep-prod.cod-depos-sai = cod-depos
       tt-rep-prod.cod-local-sai = ?
       tt-rep-prod.linha = 1
       tt-rep-prod.nro-ord-seq = 1
       tt-rep-prod.cod-versao-integracao = 001
       tt-rep-prod.prog-seg = 'ccs_esp'       
       tt-rep-prod.requis-automatica = YES.
       
       &IF "{&emsfnd_version}" < "1.0"  &THEN
            ASSIGN tt-rep-prod.conta-aplic = b-item.conta-aplicacao.
       &ENDIF
            

RUN cpp/cpapi001.p PERSISTENT SET h-cpapi001 
    (INPUT-OUTPUT TABLE tt-rep-prod,
     INPUT        TABLE tt-refugo,
     INPUT        TABLE tt-res-neg,
     INPUT        TABLE tt-apont-mob,
     INPUT-OUTPUT TABLE tt-erro,
     INPUT YES).

FIND FIRST tt-erro NO-LOCK NO-ERROR.
IF AVAIL tt-erro THEN DO:
    DELETE PROCEDURE h-cpapi001.
    ASSIGN h-cpapi001 = ?.
    RETURN "NOK".
END.

RUN pi-carrega-tt-reservas IN h-cpapi001 (YES,
                                          tipo-report, /* reporte por operação */
                                          ROWID(b-oper-ord)).
RUN pi-atualiza-reservas IN h-cpapi001.
RUN pi-verifica-saldo IN h-cpapi001 (b-ord-prod.cod-estabel).
RUN pi-retorna-tt-reservas IN h-cpapi001 (OUTPUT TABLE tt-reservas).

FOR EACH tt-reservas NO-LOCK 
        WHERE NOT tt-reservas.log-sem-saldo:
    CREATE tt-reservas-final.
    RAW-TRANSFER tt-reservas TO tt-reservas-final.
    DELETE tt-reservas.
END.


FOR EACH tt-reservas,
    FIRST b-reservas OF tt-reservas NO-LOCK :

    ASSIGN quant-requis = tt-reservas.quant-requis.

    verifica_reservas:
    FOR EACH b-saldo-estoq NO-LOCK 
            WHERE b-saldo-estoq.cod-estab = b-ord-prod.cod-estabel
              AND b-saldo-estoq.it-codigo = tt-reservas.it-codigo
              AND b-saldo-estoq.cod-depos = cod-depos
              AND b-saldo-estoq.qtidade-atu > 0 :

        IF quant-requis <= 0 THEN DO:
            LEAVE verifica_reservas.
        END.

        IF quant-requis >= b-saldo-estoq.qtidade-atu THEN
            ASSIGN qt-req-aux = b-saldo-estoq.qtidade-atu.
        ELSE
            ASSIGN qt-req-aux = quant-requis.
        ASSIGN quant-requis = quant-requis - qt-req-aux.
        
        CREATE tt-reservas-final.
        ASSIGN tt-reservas-final.proporcao = tt-reservas.proporcao
               tt-reservas-final.log-sem-saldo  = NO
               tt-reservas-final.nr-ord-produ = tt-reservas.nr-ord-produ
               tt-reservas-final.cod-refer = tt-reservas.cod-refer
               tt-reservas-final.it-codigo = tt-reservas.it-codigo
               tt-reservas-final.quant-orig  = b-reservas.quant-orig
               tt-reservas-final.quant-aloc = b-reservas.quant-aloc
               tt-reservas-final.quant-atend = tt-reservas.quant-atend
               tt-reservas-final.quant-calc = tt-reservas.quant-calc
               tt-reservas-final.quant-requis = qt-req-aux
        
               tt-reservas-final.cod-depos = b-saldo-estoq.cod-depos 
               tt-reservas-final.cod-localiz =  b-saldo-estoq.cod-localiz
               tt-reservas-final.lote-serie = b-saldo-estoq.lote 
               tt-reservas-final.dt-vali-lote =  b-saldo-estoq.dt-vali-lote
               tt-reservas-final.un = tt-reservas.un
               tt-reservas-final.estado = tt-reservas.estado
               tt-reservas-final.tipo-sobra  = tt-reservas.tipo-sobra
               tt-reservas-final.item-pai  = tt-reservas.item-pai
               tt-reservas-final.op-codigo = tt-reservas.op-codigo
               tt-reservas-final.cod-roteiro = tt-reservas.cod-roteiro
               tt-reservas-final.veiculo  = tt-reservas.veiculo
               tt-reservas-final.per-ppm  = tt-reservas.per-ppm
               tt-reservas-final.per-ppm-lote = tt-reservas.per-ppm-lote
               tt-reservas-final.tipo-formula  = tt-reservas.tipo-formula
               tt-reservas-final.qt-atend-lote = tt-reservas.qt-atend-lote
               tt-reservas-final.qt-requis-lote = tt-reservas.qt-requis-lote
               tt-reservas-final.qt-aloc-lote = tt-reservas.qt-aloc-lote
              
               tt-reservas-final.alternativo-de  = tt-reservas.alternativo-de
               tt-reservas-final.cod-refer-it-original = tt-reservas.cod-refer-it-original
              
               tt-reservas-final.qt-atend-res = tt-reservas.qt-atend-res
               tt-reservas-final.qt-atend-lote-res = tt-reservas.qt-atend-lote-res
               tt-reservas-final.quant-calc-orig = tt-reservas.quant-calc-orig
                
               tt-reservas-final.processada  = tt-reservas.processada
               tt-reservas-final.rw-reserva  = tt-reservas.rw-reserva
               tt-reservas-final.rw-saldo-estoq  = ROWID(b-saldo-estoq)
               tt-reservas-final.tipo-ordem  = tt-reservas.tipo-ordem
               tt-reservas-final.tempo  = tt-reservas.tempo
               tt-reservas-final.tentativas  = tt-reservas.tentativas
               tt-reservas-final.sequencia  = seq
               .
        ASSIGN seq = seq + 1.
    END.
    IF quant-requis <= 0 THEN DO:
        DELETE tt-reservas.
    END.
END.

DEF VAR quant-var-maior AS DECIMAL NO-UNDO.
DEF VAR quant-var-menor AS DECIMAL NO-UNDO.
DEF VAR retorno AS CHAR NO-UNDO.

FIND FIRST b-oper-ord NO-LOCK WHERE b-oper-ord.nr-ord-prod = nr-ord-prod
                                AND b-oper-ord.op-codigo > op-codigo NO-ERROR.
IF NOT AVAIL b-oper-ord AND NOT apto-parcial THEN DO:

    FOR EACH b-reservas NO-LOCK WHERE b-reservas.nr-ord-prod = nr-ord-prod
                                  AND b-reservas.estado = 1, /* ativo */
        FIRST b-item OF b-reservas NO-LOCK WHERE b-item.tipo-requis = 1: /* tp.requisição = normal */

        ASSIGN quant-requis = b-reservas.quant-requis.
        IF (quant-requis = 0 OR quant-requis < b-reservas.quant-atend) AND b-reservas.quant-atend <> 0 THEN
            ASSIGN quant-requis = b-reservas.quant-atend.
        
        ASSIGN quant-var-maior = 0  /* verifica variação permitida do item: */
               quant-var-menor = 0.
        FOR FIRST item-manuf-estab WHERE item-manuf-estab.cod-estabel = b-ord-prod.cod-estabel
                                     AND item-manuf-estab.it-codigo   = b-reservas.it-codigo NO-LOCK :
            ASSIGN quant-var-maior = b-reservas.quant-orig * (item-manuf-estab.var-apto-maior / 100) * -1
                   quant-var-menor = b-reservas.quant-orig * (item-manuf-estab.var-apto-menor / 100).
        END.
        IF b-reservas.quant-orig - quant-requis <= quant-var-menor AND        /* se está dentro da variação ignora */
           b-reservas.quant-orig - quant-requis >= quant-var-maior THEN NEXT.

        /*IF b-reservas.quant-orig - b-reservas.quant-requis <= 0 THEN NEXT.
        IF b-reservas.quant-orig - b-reservas.quant-atend  <= 0 THEN NEXT.*/

        CREATE tt-erro.
        ASSIGN tt-erro.mensagem = "Falta requisitar item: " + b-reservas.it-codigo +
                                  "~nQuantidade: " + STRING(b-reservas.quant-orig - b-reservas.quant-requis) +
                                  "~nContatar o Recebimento de Chapas/Almoxarifado".
    
        ASSIGN retorno = "NOK".
    END.
END.

FIND FIRST tt-reservas NO-LOCK NO-ERROR.
IF AVAIL tt-reservas THEN DO:
    FOR EACH tt-reservas NO-LOCK:
        CREATE tt-erro.
        ASSIGN tt-erro.mensagem = "Falta saldo item: " + tt-reservas.it-codigo + 
                                  "~nQuantidade: " + STRING(tt-reservas.quant-calc) + " " + tt-reservas.un + "(s)" + 
                                  "~nNo depósito : " + tt-reservas.cod-depos +
                                  "~nContatar o PCP".
    END.
    ASSIGN retorno = "NOK".
END. ELSE IF requisita = YES AND retorno = "" THEN DO:
    /* não existe registros na tt-reservas; então achou saldo para todos os itens ! */
    RUN pi-recebe-tt-reservas IN h-cpapi001 (INPUT TABLE tt-reservas-final).
    RUN pi-processa-reservas-positivas IN h-cpapi001.
    ASSIGN retorno = RETURN-VALUE.
END.

DELETE PROCEDURE h-cpapi001.
ASSIGN h-cpapi001 = ?.

RETURN retorno.

