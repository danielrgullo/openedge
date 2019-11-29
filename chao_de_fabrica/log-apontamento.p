{utp/ut-glob.i}
{esp/fnetwork.i}
{c:/work/desenv/ccs/cf/funcoes-split.i}

FUNCTION saldo-aberto-operacao RETURNS DECIMAL (nr-ord-prod AS INTEGER, op-codigo AS INTEGER):
   RETURN saldo-operacao(nr-ord-prod, op-codigo) - saldo-operacao-anterior(nr-ord-prod, op-codigo).
END FUNCTION.

/* 
 * definidos na funcoes-split.i :
{cdp/cd0666.i "NEW SHARED"}
DEF BUFFER b-split-operac FOR split-operac.
*/

/* tabelas em CCS ***************************/
DEF TEMP-TABLE log-apontamento NO-UNDO
    FIELD data-hora AS DATETIME
    FIELD hostname AS CHAR
    FIELD username AS CHAR
    FIELD username-totvs AS CHAR
    FIELD nr-ord-prod  LIKE ord-prod.nr-ord-prod
    FIELD op-codigo    LIKE oper-ord.op-codigo
    FIELD num-split-operac LIKE split-operac.num-split-operac
    FIELD saldo-anterior AS DECIMAL COLUMN-LABEL 'Saldo Anterior'
    FIELD saldo-operacao AS DECIMAL COLUMN-LABEL 'Saldo Atual'
    FIELD cod-operador AS CHAR
    FIELD cod-ctrab    AS CHAR
    FIELD qt-aprovada  AS DECIMAL
    FIELD qt-refugada  AS DECIMAL
    FIELD cod-motiv-refugo AS CHAR
    INDEX id  data-hora 
    INDEX ord nr-ord-prod 
              op-codigo 
              num-split-operac
    .
DEF TEMP-TABLE log-apontamento-erros NO-UNDO
    FIELD rowid-log-apontamento AS ROWID
    FIELD cd-erro  AS INTEGER
    FIELD mensagem AS CHARACTER 
    INDEX id rowid-log-apontamento
    .
/*************************************/

/*
 * PARAM
 ****************************************************************/
DEF INPUT PARAM cod-operador LIKE operador.cod-operador NO-UNDO.
DEF INPUT PARAM cod-ctrab    LIKE ctrab.cod-ctrab NO-UNDO.
DEF INPUT PARAM split-operac-rowid AS ROWID NO-UNDO.
DEF INPUT PARAM qt-aprovada AS DECIMAL NO-UNDO.
DEF INPUT PARAM qt-refugada AS DECIMAL NO-UNDO.
DEF INPUT PARAM cod-motiv-refugo LIKE motiv-refugo.cod-motiv-refugo NO-UNDO.
DEF INPUT PARAM TABLE FOR tt-erro.

/*
 * MAIN
 ****************************************************************/
DEF VAR c-hostname AS CHAR NO-UNDO.
DEF VAR c-username AS CHAR NO-UNDO.
DEF VAR de-saldo-anterior AS DECIMAL NO-UNDO.
DEF VAR de-saldo-operacao AS DECIMAL NO-UNDO.

DEF VAR ult-op-codigo AS CHAR NO-UNDO.

FIND FIRST b-split-operac NO-LOCK
    WHERE ROWID(b-split-operac) = split-operac-rowid NO-ERROR.
IF NOT AVAIL b-split-operac THEN
    LEAVE. /* ? */

DO 
    ON ERROR UNDO, THROW
    ON STOP  UNDO, RETRY:

    ASSIGN c-hostname = get-hostname()
           c-username = OS-GETENV('USERNAME').
    ASSIGN de-saldo-anterior = saldo-operacao-anterior(b-split-operac.nr-ord-prod, b-split-operac.op-codigo)
           de-saldo-operacao = saldo-aberto-operacao(b-split-operac.nr-ord-prod, b-split-operac.op-codigo).
    
    CREATE log-apontamento.
    ASSIGN log-apontamento.data-hora = NOW
           log-apontamento.hostname = c-hostname
           log-apontamento.username = c-username
           log-apontamento.username-totvs = c-seg-usuario
           /*  dados ordem */ 
           log-apontamento.nr-ord-prod = b-split-operac.nr-ord-prod
           log-apontamento.op-codigo = b-split-operac.op-codigo
           log-apontamento.num-split-operac = b-split-operac.num-split-operac
           /* saldos  */
           log-apontamento.saldo-anterior = de-saldo-anterior
           log-apontamento.saldo-operacao = de-saldo-operacao
           /* parametros  */
           log-apontamento.cod-operador = cod-operador
           log-apontamento.cod-ctrab = cod-ctrab
           log-apontamento.qt-aprovada = qt-aprovada
           log-apontamento.qt-refugada = qt-refugada
           log-apontamento.cod-motiv-refugo = cod-motiv-refugo
           .
    /* erros retornados (em caso de tentativa de apontamento) */
    FOR EACH tt-erro NO-LOCK:
        IF TRIM(tt-erro.mensagem) = '' THEN
            NEXT.
        CREATE log-apontamento-erros.
        ASSIGN log-apontamento-erros.rowid-log-apontamento = ROWID(log-apontamento)
               log-apontamento-erros.cd-erro = tt-erro.cd-erro
               log-apontamento-erros.mensagem = tt-erro.mensagem.
    END .

END.
