DEF BUFFER b-split-operac FOR split-operac.
DEF TEMP-TABLE RowError 
    FIELD ErrorSequence    AS INTEGER
    FIELD ErrorNumber      AS INTEGER
    FIELD ErrorDescription AS CHARACTER FORMAT "x(70)"
    FIELD ErrorParameters  AS CHARACTER FORMAT "x(70)"
    FIELD ErrorType        AS CHARACTER FORMAT "x(70)"
    FIELD ErrorHelp        AS CHARACTER FORMAT "x(70)"
    FIELD ErrorSubType     AS CHARACTER FORMAT "x(70)".

{ccs/cf/funcoes-operacoes.i}

FUNCTION cod-ctrab-default RETURNS CHAR (nr-ord-prod AS INTEGER,
                                         op-codigo   AS INTEGER):    
    FIND FIRST b-split-operac NO-LOCK 
        WHERE b-split-operac.nr-ord-prod = nr-ord-prod
          AND b-split-operac.op-codigo   = op-codigo NO-ERROR.
    IF AVAIL b-split-operac AND conta-ctrab-gm(b-split-operac.cod-ctrab) = 1 THEN
        RETURN b-split-operac.cod-ctrab.
    RETURN "".
END FUNCTION.

PROCEDURE divide-split:
    DEF INPUT PARAM r-split     AS ROWID NO-UNDO.
    DEF INPUT PARAM qt-apontado AS DECIMAL NO-UNDO.

    DEF VAR h-bo AS HANDLE NO-UNDO.
    DEF VAR retorno AS CHAR NO-UNDO.
    
    FIND FIRST split-operac NO-LOCK 
        WHERE ROWID(split-operac) = r-split NO-ERROR.
    IF NOT AVAIL split-operac THEN DO:
        RELEASE split-operac.
        RETURN "NOK".
    END.

    FIND FIRST ITEM OF split-operac NO-LOCK NO-ERROR.
    IF NOT AVAIL ITEM THEN
        RETURN "NOK".

    IF ITEM.fraciona = FALSE THEN   /* n∆o sei se precisa foráar isso ... */
        ASSIGN qt-apontado = INT(qt-apontado).

    RUN ccs/cf/bo-split-join-oper.p PERSISTENT SET h-bo.
    RUN divide-operacao IN h-bo (r-split, qt-apontado).
    ASSIGN retorno = STRING(RETURN-VALUE).
    RUN destroy-bo IN h-bo.
    ASSIGN h-bo = ?.
    RELEASE split-operac.
    RETURN retorno.
END PROCEDURE.

PROCEDURE unir-split:
    DEF INPUT PARAM nr-ord-prod LIKE ord-prod.nr-ord-prod NO-UNDO.
    DEF INPUT PARAM op-codigo   LIKE oper-ord.op-codigo NO-UNDO.

    DEF VAR h-bo AS HANDLE NO-UNDO.
    DEF VAR c AS INTEGER NO-UNDO.
    DEF VAR retorno AS CHAR NO-UNDO.
    
    DEF VAR r-split1 AS ROWID NO-UNDO.
    DEF VAR r-split2 AS ROWID NO-UNDO.

    DEF VAR cod-ctrab AS CHAR NO-UNDO.
    ASSIGN cod-ctrab = cod-ctrab-default(nr-ord-prod,
                                         op-codigo).
    
    ASSIGN c = 0.
    FOR EACH split-operac NO-LOCK 
            WHERE split-operac.nr-ord-prod = nr-ord-prod
              AND split-operac.op-codigo = op-codigo
              AND split-operac.cod-ctrab = cod-ctrab
              AND split-operac.ind-estado-split < 5:
        ASSIGN c = c + 1.
        IF r-split1 = ? THEN
            ASSIGN r-split1 = ROWID(split-operac).
        ELSE
            ASSIGN r-split2 = ROWID(split-operac).
    END.

    IF c < 2 THEN 
        RETURN "OK".

    RUN ccs/cf/bo-split-join-oper.p PERSISTENT SET h-bo.
    RUN unir-operacoes IN h-bo (r-split1, r-split2).
    ASSIGN retorno = STRING(RETURN-VALUE).
    RUN destroy-bo IN h-bo.
    ASSIGN h-bo = ?.
    RETURN retorno.  /*RETURN unir-split(nr-ord-prod, op-codigo).*/
END PROCEDURE.

FUNCTION tem-split-aberto RETURN LOGICAL (nr-ord-prod AS INTEGER,
                                          op-codigo   AS INTEGER,
                                          OUTPUT r-split AS ROWID,
                                          OUTPUT qt-saldo AS DECIMAL):
    FIND FIRST b-split-operac NO-LOCK 
        WHERE b-split-operac.nr-ord-prod = nr-ord-prod
          AND b-split-operac.op-codigo = op-codigo
          AND b-split-operac.ind-estado-split < 5 
        NO-ERROR.
    IF AVAIL b-split-operac THEN DO:
        ASSIGN qt-saldo = b-split-operac.qtd-previs-operac - (b-split-operac.qtd-operac-aprov + b-split-operac.qtd-operac-refgda)
               r-split = ROWID(b-split-operac).
        RETURN YES.
    END.
    RETURN NO.
END FUNCTION.

FUNCTION tem-split-aberto-ctrab RETURNS LOGICAL (nr-ord-prod AS INTEGER,
                                                 op-codigo   AS INTEGER,
                                                 cod-ctrab   AS CHARACTER,
                                                 OUTPUT r-split AS ROWID,
                                                 OUTPUT qt-saldo AS DECIMAL):
    FIND FIRST b-split-operac NO-LOCK 
        WHERE b-split-operac.nr-ord-prod = nr-ord-prod
          AND b-split-operac.op-codigo = op-codigo          
          AND b-split-operac.ind-estado-split < 5 
          AND b-split-operac.cod-ctrab = cod-ctrab NO-ERROR.
    IF AVAIL b-split-operac THEN DO:
        ASSIGN qt-saldo = b-split-operac.qtd-previs-operac - (b-split-operac.qtd-operac-aprov + b-split-operac.qtd-operac-refgda)
               r-split = ROWID(b-split-operac).
        IF qt-saldo <= 0 THEN DO:
            ASSIGN qt-saldo = 0
                   r-split = ?.
            RETURN NO.
        END.
        RETURN YES.
    END.
    RETURN NO.
END FUNCTION.

FUNCTION tem-split-vazio RETURN LOGICAL (INPUT nr-ord-prod AS INTEGER,
                                         INPUT op-codigo   AS INTEGER,
                                         OUTPUT r-split AS ROWID,
                                         OUTPUT qt-saldo AS DECIMAL):
    DEF VAR ret AS LOGICAL NO-UNDO.

    ASSIGN ret = tem-split-aberto-ctrab(nr-ord-prod,
                                        op-codigo,
                                        cod-ctrab-default(nr-ord-prod, /* quando o gm s¢ tem um ctrab, n∆o Ç vazio! */
                                                          op-codigo),  
                                        r-split,
                                        qt-saldo).
    IF NOT ret THEN
        ASSIGN ret = tem-split-aberto-ctrab(nr-ord-prod,
                                        op-codigo,
                                        "",
                                        r-split,
                                        qt-saldo).
    RETURN ret.
END FUNCTION.

FUNCTION total-aprovado-operacao RETURNS DECIMAL (nr-ord-prod AS INTEGER,
                                                  op-codigo   AS INTEGER):
    DEF VAR qt-aprovado AS DECIMAL NO-UNDO.
    FOR EACH b-split-operac NO-LOCK
        WHERE b-split-operac.nr-ord-prod = nr-ord-prod
          AND b-split-operac.op-codigo = op-codigo
          AND b-split-operac.qtd-operac-aprov > 0 :
        ASSIGN qt-aprovado  = qt-aprovado + b-split-operac.qtd-operac-aprov.
    END.
    RETURN qt-aprovado.
END FUNCTION.

FUNCTION total-refugado-operacao RETURNS DECIMAL (nr-ord-prod AS INTEGER,
                                                  op-codigo   AS INTEGER):
    DEF VAR qt-refugado AS DECIMAL NO-UNDO.
    FOR EACH b-split-operac NO-LOCK
        WHERE b-split-operac.nr-ord-prod = nr-ord-prod
          AND b-split-operac.op-codigo = op-codigo:
        ASSIGN qt-refugado  = qt-refugado + b-split-operac.qtd-operac-refgda.
    END.
    RETURN qt-refugado.
END FUNCTION.

FUNCTION saldo-operacao RETURNS DECIMAL (nr-ord-prod AS INTEGER,
                                         op-codigo   AS INTEGER):
    DEF VAR qt-saldo AS DECIMAL NO-UNDO.
    FOR EACH b-split-operac NO-LOCK
        WHERE b-split-operac.nr-ord-prod = nr-ord-prod
          AND b-split-operac.op-codigo = op-codigo
          AND b-split-operac.ind-estado-split < 5 :
        ASSIGN qt-saldo  = qt-saldo + (b-split-operac.qtd-previs-operac - 
                                       (b-split-operac.qtd-operac-aprov + b-split-operac.qtd-operac-refgda)).
    END.
    RETURN qt-saldo.
END FUNCTION.

FUNCTION saldo-operacao-anterior RETURNS DECIMAL (nr-ord-prod AS INTEGER,
                                                  op-codigo   AS INTEGER):
    DEF VAR op-predec LIKE oper-ord.op-codigo NO-UNDO.

    ASSIGN op-predec = operacao-anterior(nr-ord-prod, op-codigo).
    IF op-predec <= 0 THEN
        RETURN 0.0.

    DEF VAR qt-saldo AS DECIMAL NO-UNDO.
    FOR EACH b-split-operac NO-LOCK
        WHERE b-split-operac.nr-ord-prod = nr-ord-prod
          AND b-split-operac.op-codigo = op-predec
          AND b-split-operac.ind-estado-split < 5 :
        ASSIGN qt-saldo  = qt-saldo + (b-split-operac.qtd-previs-operac - 
                                       (b-split-operac.qtd-operac-aprov + b-split-operac.qtd-operac-refgda)).
    END.
    RETURN qt-saldo.
END FUNCTION.

FUNCTION aprovado-operacao-anterior RETURNS DECIMAL (nr-ord-prod AS INTEGER,
                                                     op-codigo   AS INTEGER):
    DEF VAR qt-saldo AS DECIMAL NO-UNDO.
    DEF VAR op-predec LIKE oper-ord.op-codigo NO-UNDO.

    ASSIGN op-predec = operacao-anterior(nr-ord-prod, op-codigo).
    IF op-predec > 0 THEN
        RETURN total-aprovado-operacao(nr-ord-prod, op-predec).
    
    FIND FIRST ord-prod NO-LOCK 
        WHERE ord-prod.nr-ord-prod = nr-ord-prod NO-ERROR.
    IF AVAIL ord-prod THEN
        RETURN ord-prod.qt-ordem.
    RETURN 0.0.
        /*RETURN total-aprovado-operacao(nr-ord-prod, op-codigo).*/
END FUNCTION.

FUNCTION tem-saldo-apontado RETURNS LOGICAL (nr-ord-prod AS INTEGER,
                                             op-codigo   AS INTEGER,
                                             qt-apontar  AS DECIMAL):
    DEF VAR qt-saldo AS DECIMAL NO-UNDO.
    ASSIGN qt-saldo = total-aprovado-operacao(nr-ord-prod, op-codigo).
    RETURN qt-apontar <= qt-saldo.
END FUNCTION.

FUNCTION tem-saldo-operacao  RETURNS LOGICAL (nr-ord-prod AS INTEGER,
                                              op-codigo   AS INTEGER,
                                              qt-apontar  AS DECIMAL):
    DEF VAR qt-saldo AS DECIMAL NO-UNDO.
    ASSIGN qt-saldo = saldo-operacao(nr-ord-prod, op-codigo).
    RETURN qt-apontar <= qt-saldo.
END FUNCTION.

FUNCTION saldo-suficiente-split RETURN LOGICAL (nr-ord-prod AS INTEGER,
                                                op-codigo   AS INTEGER,
                                                quantidade  AS DECIMAL):
    DEF VAR qt-saldo AS DECIMAL NO-UNDO.
    ASSIGN qt-saldo = saldo-operacao(nr-ord-prod, op-codigo).
    RETURN qt-saldo >= quantidade.
END FUNCTION.

FUNCTION eh-apontamento-parcial RETURN LOGICAL (nr-ord-prod AS INTEGER,
                                                op-codigo   AS INTEGER,
                                                quantidade  AS DECIMAL):
    DEF VAR qt-saldo AS DECIMAL NO-UNDO.
    ASSIGN qt-saldo = saldo-operacao(nr-ord-prod, op-codigo).
    RETURN qt-saldo <> quantidade.
END FUNCTION.

FUNCTION verifica-saldo-operacao RETURNS LOGICAL (nr-ord-prod AS INTEGER,  
                                                  op-codigo   AS INTEGER,
                                                  qt-apontado AS DECIMAL ):

    DEF VAR qt-saldo-ant AS DECIMAL NO-UNDO.
    DEF VAR qt-saldo     AS DECIMAL NO-UNDO.
    DEF VAR op-predec LIKE oper-ord.op-codigo NO-UNDO.

    ASSIGN op-predec = operacao-anterior(nr-ord-prod, op-codigo).
    IF op-predec <= 0 THEN
        RETURN TRUE.

    ASSIGN qt-saldo-ant = total-aprovado-operacao(nr-ord-prod, op-predec)
           qt-saldo     = total-aprovado-operacao(nr-ord-prod, op-codigo)
           qt-saldo = qt-saldo + qt-apontado.

    RETURN qt-saldo-ant >= qt-saldo.
    /*
    IF op-predec > 0 AND NOT tem-saldo-apontado(nr-ord-prod, op-predec, qt-apontado) THEN
            RETURN FALSE.
    
    RETURN tem-saldo-operacao(nr-ord-prod, op-codigo, qt-apontado).*/
END FUNCTION.

FUNCTION tem-split-com-quantidade RETURNS LOGICAL (nr-ord-prod AS INTEGER,
                                                   op-codigo   AS INTEGER,
                                                   qt-apontado AS DECIMAL ):
    FIND FIRST b-split-operac NO-LOCK 
        WHERE b-split-operac.nr-ord-prod = nr-ord-prod
          AND b-split-operac.op-codigo   = op-codigo          
          AND b-split-operac.ind-estado-split = 5 
          AND b-split-operac.qtd-operac-aprov = qt-apontado NO-ERROR.
    RETURN AVAIL (b-split-operac).
END FUNCTION.

FUNCTION conta-split-com-quantidade RETURNS INTEGER (nr-ord-prod AS INTEGER,
                                                     op-codigo   AS INTEGER,
                                                     qt-apontado AS DECIMAL ):
    DEF VAR conta AS INTEGER NO-UNDO.
    FOR EACH b-split-operac NO-LOCK 
        WHERE b-split-operac.nr-ord-prod = nr-ord-prod
          AND b-split-operac.op-codigo   = op-codigo          
          AND b-split-operac.ind-estado-split = 5 
          AND b-split-operac.qtd-operac-aprov = qt-apontado :
        ASSIGN conta = conta + 1.
    END.
    RETURN conta.
END FUNCTION.

FUNCTION apontamento-quebra-lote RETURNS LOGICAL (nr-ord-prod AS INTEGER,
                                                  op-codigo   AS INTEGER,
                                                  qt-apontado AS DECIMAL):

    DEF VAR atual AS INTEGER NO-UNDO.
    DEF VAR anterior AS INTEGER NO-UNDO.

    ASSIGN atual = conta-split-com-quantidade(nr-ord-prod,
                                              op-codigo,
                                              qt-apontado)
          anterior = conta-split-com-quantidade(nr-ord-prod,
                                                operacao-anterior(nr-ord-prod,
                                                                  op-codigo),
                                                qt-apontado).
    RETURN atual > anterior.
END FUNCTION.

FUNCTION pode-apontar-horario RETURNS LOGICAL (cod-ctrab AS CHAR,
                                               data AS DATE,
                                               hr-ini AS INTEGER,
                                               hr-fim AS INTEGER):

    DEF BUFFER b-grup-maquin FOR grup-maquin.
    DEF BUFFER b-area-produc-ctrab FOR area-produc-ctrab.
    

    IF hr-fim < hr-ini THEN
        RETURN NO.

    FIND FIRST b-area-produc-ctrab NO-LOCK 
        WHERE b-area-produc-ctrab.cod-ctrab = cod-ctrab NO-ERROR.
    IF NOT AVAIL b-area-produc-ctrab THEN
        RETURN NO.
    
    FIND FIRST b-grup-maquin OF b-area-produc-ctrab NO-LOCK NO-ERROR.
    IF NOT AVAIL b-grup-maquin THEN
        RETURN NO.
    
    IF b-grup-maquin.ind-tip-ctrab <> 1 THEN
        RETURN YES.

    FIND FIRST b-split-operac NO-LOCK
        WHERE b-split-operac.cod-ctrab = cod-ctrab
          AND b-split-operac.dat-fim-operac = data
          AND (b-split-operac.qtd-segs-inic-operac  <= hr-fim OR
               b-split-operac.qtd-segs-fim-operac   <= hr-fim)
          AND b-split-operac.qtd-segs-fim-operac >= hr-ini NO-ERROR.
    RETURN NOT AVAIL(b-split-operac).
END FUNCTION.

FUNCTION apontamento-gm-alternativo RETURNS LOGICAL (r-split-operac AS ROWID,
                                                     c-novo-gm      AS CHAR ):
    FIND FIRST b-split-operac NO-LOCK 
        WHERE ROWID(b-split-operac) = r-split-operac NO-ERROR.
    IF NOT AVAIL b-split-operac THEN 
        RETURN FALSE.

    RETURN verifica-grup-maquina-alternativo(b-split-operac.gm-codigo, c-novo-gm).
END FUNCTION.

PROCEDURE aloca-gm-codigo-split :
    DEF INPUT PARAM r-split-operac AS ROWID NO-UNDO.
    DEF INPUT PARAM c-novo-gm    LIKE grup-maquin.gm-codigo NO-UNDO.
    DEF INPUT PARAM c-novo-ctrab LIKE ctrab.cod-ctrab NO-UNDO.

    DEF VAR gm-codigo-ant LIKE split-operac.gm-codigo NO-UNDO.
    
    DEF VAR h-boin535 AS HANDLE NO-UNDO.
    
    FIND FIRST b-split-operac NO-LOCK 
        WHERE ROWID(b-split-operac) = r-split-operac NO-ERROR.
    IF NOT AVAIL b-split-operac THEN 
        RETURN "Split n∆o encontrado!".

    ASSIGN gm-codigo-ant = b-split-operac.gm-codigo.

    IF gm-codigo-ant = c-novo-gm AND b-split-operac.cod-ctrab = c-novo-ctrab THEN
        RETURN "OK".

    IF grup-maquina-do-ctrab(c-novo-ctrab) <> c-novo-gm AND
       NOT verifica-grup-maquina-alternativo(b-split-operac.gm-codigo, c-novo-gm) THEN
        RETURN "Este n∆o Ç um grupo de m†quina alternativo v†lido!".

    RUN inbo/boin535.r PERSISTENT SET h-boin535.

    RUN alocarCtrab IN h-boin535
         (INPUT r-split-operac,
          INPUT c-novo-gm,
          INPUT c-novo-ctrab,
          INPUT "", // nova-ferram 
          OUTPUT TABLE RowError).
    
    DELETE PROCEDURE h-boin535 NO-ERROR.
    ASSIGN h-boin535 = ?.

    FIND FIRST RowError NO-LOCK NO-ERROR.
    IF AVAIL RowError THEN
        RETURN RowError.ErrorDescription.

    RETURN gm-codigo-ant.
END PROCEDURE.
