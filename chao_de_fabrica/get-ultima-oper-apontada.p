DEF TEMP-TABLE tt-splits NO-UNDO
    FIELD cod-estab   LIKE ord-prod.cod-estabel
    FIELD nr-ord-prod LIKE ord-prod.nr-ord-prod
    FIELD it-codigo   LIKE ITEM.it-codigo
    FIELD op-codigo   LIKE split-operac.op-codigo
    FIELD gm-codigo   LIKE split-operac.gm-codigo
    FIELD qt-split    AS DECIMAL
    INDEX idx1 nr-ord-prod
               op-codigo DESC.

/*
 *************************************************************/
DEF INPUT  PARAM nr-ord-prod LIKE ord-prod.nr-ord-prod NO-UNDO.
DEF OUTPUT PARAM i-op-codigo LIKE oper-ord.op-codigo   NO-UNDO INIT 0.

RUN pi-saldo-split(nr-ord-prod).

FOR EACH tt-splits NO-LOCK:
    FIND FIRST pert-ordem NO-LOCK 
        WHERE pert-ordem.nr-ord-prod = tt-splits.nr-ord-prod
          AND pert-ordem.op-codigo = tt-splits.op-codigo NO-ERROR.
    IF AVAIL pert-ordem THEN 
        ASSIGN i-op-codigo = pert-ordem.op-predec.
    LEAVE.
END.

/*
 *************************************************************/
PROCEDURE pi-saldo-split:
    DEF INPUT PARAM i-nr-ord-prod LIKE ord-prod.nr-ord-prod NO-UNDO.
    
    DEF VAR qt-saldo    AS DECIMAL NO-UNDO.
    DEF VAR qt-refug AS DECIMAL NO-UNDO.
    DEF VAR saldo-split AS DECIMAL NO-UNDO.

    EMPTY TEMP-TABLE tt-splits.
    
    FOR EACH ord-prod NO-LOCK
            WHERE ord-prod.nr-ord-prod = i-nr-ord-prod:
    
        ASSIGN qt-saldo = ord-prod.qt-ordem
               qt-refug = 0.
    
        ver_split:
        FOR EACH oper-ord OF ord-prod NO-LOCK:
            
            ASSIGN saldo-split = qt-saldo - oper-ord.qt-produzida.
    
            IF oper-ord.qt-refugada > 0 THEN
                ASSIGN qt-refug = qt-refug + oper-ord.qt-refugada.
            
            /* na opera‡Æo que foi refugada a quantidade produzida inclui o refugo! 
             * entÆo tira para saber o saldo correto. **/
            IF oper-ord.qt-refugada = 0 AND qt-refug > 0 THEN 
                ASSIGN saldo-split = saldo-split - qt-refug.
    
            IF oper-ord.estado = 3 AND saldo-split > 0 THEN
                ASSIGN saldo-split = 0.
    
            IF saldo-split > 0 THEN
                RUN pi-cria-tt-split (saldo-split).
    
            /* mesmo caso ali de cima ... mas faz o inverso:
             * coloca na produzida o refugo nas opera‡äes subsequentes **/
            IF oper-ord.qt-refugada > 0 THEN
                ASSIGN qt-saldo = oper-ord.qt-produzida.
            ELSE
                ASSIGN qt-saldo = oper-ord.qt-produzida + qt-refug.
        END.
    END.
END PROCEDURE.

PROCEDURE pi-cria-tt-split:
    DEF INPUT PARAM saldo-split AS DECIMAL NO-UNDO.    

    CREATE tt-splits.
    ASSIGN tt-splits.cod-estab   = ord-prod.cod-estabel
           tt-splits.nr-ord-prod = ord-prod.nr-ord-prod
           tt-splits.it-codigo   = ord-prod.it-codigo
           tt-splits.op-codigo   = oper-ord.op-codigo
           tt-splits.gm-codigo   = oper-ord.gm-codigo
           tt-splits.qt-split    = saldo-split
           .
    IF tt-splits.gm-codigo = '' OR oper-ord.tipo-oper = 2 THEN /* vazio ou opera‡Æo externa */
        ASSIGN tt-splits.gm-codigo = SUBSTRING(oper-ord.descricao, 1, 12).

END PROCEDURE.

