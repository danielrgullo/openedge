{c:/work/desenv/ccs/cf/funcoes-split.i}


DEF INPUT PARAM c-cod-ctrab    LIKE ctrab.cod-ctrab NO-UNDO.    
DEF INPUT PARAM c-cod-operador LIKE operador.cod-operador NO-UNDO.
DEF INPUT PARAM i-nr-ord-prod  LIKE ord-prod.nr-ord-prod NO-UNDO.
DEF INPUT PARAM i-op-codigo    LIKE oper-ord.op-codigo NO-UNDO.
DEF INPUT PARAM qt-apontado    AS DECIMAL NO-UNDO.

DEF NEW GLOBAL SHARED VAR c-seg-usuario AS CHAR FORMAT "x(12)" NO-UNDO.

DEF VAR i-op-codigo-orig LIKE oper-ord.op-codigo NO-UNDO.
DEF VAR op-saldo AS DECIMAL NO-UNDO.

ASSIGN i-op-codigo-orig = i-op-codigo.

looop_oper:
REPEAT:

    /* caminha pela rede pert da ordem */
    FIND FIRST pert-ordem NO-LOCK
        WHERE pert-ordem.nr-ord-prod = i-nr-ord-prod
          AND pert-ordem.op-codigo = i-op-codigo NO-ERROR.
    IF NOT AVAIL pert-ordem THEN DO:
        /* fim .. nÆo tinha saldo em nada. log? */
        LEAVE looop_oper.
    END.
    
    ASSIGN /* seta opera‡Æo como a predecessora */
           i-op-codigo = pert-ordem.op-prede 
           /* calcula saldo desta opera‡Æo */
           op-saldo = aprovado-operacao-anterior(i-nr-ord-prod,
                                                 i-op-codigo)
           op-saldo = op-saldo - total-aprovado-operacao(i-nr-ord-prod,
                                                         i-op-codigo).
    
    IF op-saldo > 0 THEN DO:
        /* existe algum saldo. log! */
        RUN pi-salva-log (c-cod-ctrab,
                          c-cod-operador,
                          i-nr-ord-prod,
                          i-op-codigo-orig,
                          qt-apontado,
                          i-op-codigo,
                          op-saldo).
        LEAVE looop_oper.
    END.
END.

PROCEDURE pi-salva-log:
    DEF INPUT PARAM cod-ctrab    LIKE ctrab.cod-ctrab NO-UNDO.
    DEF INPUT PARAM cod-operador LIKE operador.cod-operador NO-UNDO.
    DEF INPUT PARAM nr-ord-prod  LIKE ord-prod.nr-ord-prod NO-UNDO.
    DEF INPUT PARAM op-codigo    LIKE oper-ord.op-codigo NO-UNDO.
    DEF INPUT PARAM qt-apontado  AS DECIMAL NO-UNDO.
    DEF INPUT PARAM op-com-saldo LIKE oper-ord.op-codigo NO-UNDO.
    DEF INPUT PARAM saldo-op     AS DECIMAL NO-UNDO.

    DO 
        ON ERROR UNDO, RETRY
        ON STOP  UNDO, LEAVE:


        CREATE rep-oper-audit.
        ASSIGN rep-oper-audit.dt-apont = TODAY
               rep-oper-audit.hr-apont = TIME
               rep-oper-audit.cod-mensagem = 17006
               rep-oper-audit.des-mensagem = 'Opera‡Æo sem saldo'
               rep-oper-audit.nr-ord-prod = nr-ord-prod
               rep-oper-audit.op-codigo = op-codigo
               rep-oper-audit.qt-apontado = qt-apontado
               rep-oper-audit.op-codigo-anterior = op-com-saldo
               rep-oper-audit.qt-saldo-op = saldo-op               
               rep-oper-audit.cod-operador = cod-operador
               rep-oper-audit.cod-ctrab = cod-ctrab
               rep-oper-audit.usu-apont = c-seg-usuario
               .
    END.

END PROCEDURE.
