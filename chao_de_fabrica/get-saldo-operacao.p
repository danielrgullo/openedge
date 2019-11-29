{c:/work/desenv/ccs/cf/funcoes-split.i}

DEF INPUT  PARAM i-nr-ord-prod LIKE ord-prod.nr-ord-prod NO-UNDO.
DEF INPUT  PARAM i-op-codigo LIKE oper-ord.op-codigo NO-UNDO.
DEF OUTPUT PARAM d-saldo AS DECIMAL NO-UNDO.

DEF VAR qtd-previsto AS DECIMAL NO-UNDO.
DEF VAR qtd-aprovado AS DECIMAL NO-UNDO.
DEF VAR qtd-refugado AS DECIMAL NO-UNDO.
DEF VAR saldo AS DECIMAL NO-UNDO.

/* talvez calcular isso como persistent em algum outro lugar */
ASSIGN qtd-aprovado = total-aprovado-operacao(i-nr-ord-prod, i-op-codigo)
       qtd-refugado = total-refugado-operacao(i-nr-ord-prod, i-op-codigo)
       qtd-previsto = aprovado-operacao-anterior(i-nr-ord-prod, i-op-codigo)
       d-saldo = (qtd-previsto - qtd-aprovado) - qtd-refugado.

IF d-saldo < 0 THEN
    ASSIGN d-saldo = 0.
