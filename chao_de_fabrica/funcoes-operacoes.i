{cdp/cd0666.i "NEW SHARED"}

DEF BUFFER b-oper-ord   FOR oper-ord.
DEF BUFFER b-pert-ordem FOR pert-ordem.
DEF BUFFER b-ctrab      FOR ctrab.
/* *** *** *** *** *** *** *** *** *** *** *** *** *** *** *** *** *** *** *** *** *** *** */
FUNCTION primeira-operacao RETURNS LOGICAL (nr-ord-prod AS INTEGER,
                                            op-codigo   AS INTEGER):

    FIND FIRST b-oper-ord NO-LOCK 
        WHERE b-oper-ord.nr-ord-prod = nr-ord-prod
          AND b-oper-ord.op-codigo = op-codigo NO-ERROR.
    IF NOT AVAIL b-oper-ord THEN
        RETURN FALSE.

    FIND FIRST b-oper-ord NO-LOCK 
        WHERE b-oper-ord.nr-ord-prod = nr-ord-prod
          AND b-oper-ord.op-codigo < op-codigo NO-ERROR.

    RETURN NOT AVAIL(b-oper-ord).
END FUNCTION.

FUNCTION ultima-operacao RETURNS LOGICAL (nr-ord-prod AS INTEGER,
                                          op-codigo   AS INTEGER):

    FIND FIRST b-oper-ord NO-LOCK 
        WHERE b-oper-ord.nr-ord-prod = nr-ord-prod
          AND b-oper-ord.op-codigo = op-codigo NO-ERROR.
    IF NOT AVAIL b-oper-ord THEN
        RETURN FALSE.

    FIND LAST b-oper-ord NO-LOCK 
        WHERE b-oper-ord.nr-ord-prod = nr-ord-prod
          AND b-oper-ord.op-codigo > op-codigo NO-ERROR.

    RETURN NOT AVAIL(b-oper-ord).
END FUNCTION.

FUNCTION tem-pert-ordem RETURNS LOGICAL (nr-ord-prod AS INTEGER):
    FIND FIRST b-pert-ordem NO-LOCK 
        WHERE b-pert-ordem.nr-ord-prod = nr-ord-prod NO-ERROR.
    RETURN AVAIL(b-pert-ordem).
END FUNCTION.

FUNCTION operacao-anterior-calculada RETURN INTEGER (nr-ord-prod AS INTEGER,
                                                     op-codigo   AS INTEGER):
    FOR EACH b-oper-ord NO-LOCK 
            WHERE b-oper-ord.nr-ord-prod = nr-ord-prod
              AND b-oper-ord.op-codigo < op-codigo
        BY b-oper-ord.op-codigo DESC:
        
        IF b-oper-ord.tipo-oper = 2 /* externa */ THEN
            RETURN 0.

        RETURN b-oper-ord.op-codigo.
    END.
    RETURN -1.
END FUNCTION.

FUNCTION operacao-anterior RETURN INTEGER (nr-ord-prod AS INTEGER,
                                           op-codigo   AS INTEGER):

    IF NOT tem-pert-ordem(nr-ord-prod) THEN
        RETURN operacao-anterior-calculada(nr-ord-prod,
                                           op-codigo).

    FIND FIRST b-pert-ordem NO-LOCK 
        WHERE b-pert-ordem.nr-ord-prod = nr-ord-prod
          AND b-pert-ordem.op-codigo = op-codigo NO-ERROR.
    IF NOT AVAIL b-pert-ordem THEN
        RETURN -1.

    FIND FIRST b-oper-ord NO-LOCK 
        WHERE b-oper-ord.nr-ord-prod = nr-ord-prod
          AND b-oper-ord.op-codigo = b-pert-ordem.op-predec NO-ERROR.
    IF b-oper-ord.tipo-oper = 2 /* externa */ THEN
        RETURN 0.

    RETURN b-pert-ordem.op-predec.
END FUNCTION.

FUNCTION grup-maquina-do-ctrab RETURNS CHAR (cod-ctrab AS CHAR):
    FIND FIRST b-ctrab NO-LOCK 
        WHERE b-ctrab.cod-ctrab = cod-ctrab NO-ERROR.
    IF NOT AVAIL b-ctrab THEN
        RETURN "".
    RETURN b-ctrab.gm-codigo.
END FUNCTION.

FUNCTION verifica-grup-maquina-alternativo RETURNS LOGICAL (gm-codigo AS CHAR, 
                                                            gm-altern AS CHAR):
    IF gm-codigo = gm-altern THEN /* porque est† comparando? */
        RETURN TRUE.

    FIND FIRST grup-maq-altern NO-LOCK 
        WHERE grup-maq-altern.gm-codigo = gm-codigo
          AND grup-maq-altern.gm-altern = gm-altern NO-ERROR.
    RETURN AVAIL(grup-maq-altern).
END FUNCTION.

FUNCTION valida-operacao-ctrab RETURNS LOGICAL (nr-ord-prod AS INTEGER,
                                                op-codigo AS INTEGER,
                                                cod-ctrab AS CHAR):
    DEF VAR gm-codigo-ctrab AS CHAR NO-UNDO.

    FIND FIRST b-oper-ord NO-LOCK 
        WHERE b-oper-ord.nr-ord-prod = nr-ord-prod
          AND b-oper-ord.op-codigo   = op-codigo NO-ERROR.
    IF NOT AVAIL b-oper-ord THEN
        RETURN FALSE.

    ASSIGN gm-codigo-ctrab = grup-maquina-do-ctrab(cod-ctrab).

    IF gm-codigo-ctrab = b-oper-ord.gm-codigo THEN
        RETURN TRUE.
    ELSE
        RETURN verifica-grup-maquina-alternativo(b-oper-ord.gm-codigo,
                                                 gm-codigo-ctrab).

    /*FIND FIRST b-ctrab NO-LOCK WHERE b-ctrab.cod-ctrab = cod-ctrab
                                 AND b-ctrab.gm-codigo = b-oper-ord.gm-codigo NO-ERROR.
    RETURN AVAIL(b-ctrab).*/
END FUNCTION.

FUNCTION gm-codigo-do-ctrab RETURNS CHAR (cod-ctrab AS CHAR):
    FIND FIRST b-ctrab NO-LOCK 
        WHERE b-ctrab.cod-ctrab = cod-ctrab NO-ERROR.
    IF NOT AVAIL b-ctrab THEN
        RETURN "".
    RETURN b-ctrab.gm-codigo.
END FUNCTION.

FUNCTION conta-ctrab-gm RETURNS INTEGER (cod-ctrab AS CHAR):
    DEF VAR c AS INTEGER NO-UNDO INIT 0.
    DEF VAR gm-codigo AS CHAR NO-UNDO.
    ASSIGN gm-codigo = gm-codigo-do-ctrab(cod-ctrab).
    FOR EACH b-ctrab NO-LOCK 
            WHERE b-ctrab.gm-codigo = gm-codigo:
        ASSIGN c = c + 1.        
    END.
    RETURN c.
END FUNCTION.

FUNCTION get-tempo-operacao RETURNS DECIMAL (nr-ord-prod AS INTEGER,
                                             op-codigo   AS INTEGER,
                                             qtd AS DECIMAL):
    /* em minutos */
    DEF VAR tempo-setup   AS DECIMAL NO-UNDO.
    DEF VAR tempo-exec    AS DECIMAL NO-UNDO.
    DEF VAR tempo-total   AS DECIMAL NO-UNDO.

    FIND FIRST b-oper-ord NO-LOCK 
        WHERE b-oper-ord.nr-ord-prod = nr-ord-prod
          AND b-oper-ord.op-codigo = op-codigo NO-ERROR.
    IF NOT AVAIL b-oper-ord THEN
        RETURN 60.0.
    
    CASE b-oper-ord.un-med-tempo:
        WHEN 1 THEN /* HORA */
            ASSIGN tempo-exec  = b-oper-ord.tempo-maquin * 3600
                   tempo-setup = b-oper-ord.tempo-prepar * 3600.
        WHEN 2 THEN /* MINUTO */
            ASSIGN tempo-exec  = b-oper-ord.tempo-maquin * 60
                   tempo-setup = b-oper-ord.tempo-prepar * 60.
        WHEN 3 THEN /* SEGUNDO */
            ASSIGN tempo-exec  = b-oper-ord.tempo-maquin
                   tempo-setup = b-oper-ord.tempo-prepar.
        WHEN 4 THEN /* DIA */
            ASSIGN tempo-exec  = b-oper-ord.tempo-maquin * 86400
                   tempo-setup = b-oper-ord.tempo-prepar * 86400.
    END.

    ASSIGN tempo-exec = (tempo-exec / b-oper-ord.qtd-previs-operac) * qtd
           tempo-total = tempo-exec + tempo-setup.

    IF tempo-total < 60.0 THEN
        RETURN 60.0.
    RETURN tempo-total.
END FUNCTION.

FUNCTION deposito-ctrab RETURNS CHAR (cod-ctrab AS CHAR):

    FIND FIRST area-produc-ctrab NO-LOCK 
        WHERE area-produc-ctrab.cod-ctrab = cod-ctrab
          AND area-produc-ctrab.dat-fim-valid > TODAY NO-ERROR.
    IF NOT AVAIL area-produc-ctrab THEN
        RETURN "".
    
    
    FIND FIRST area-produc OF area-produc-ctrab NO-LOCK NO-ERROR.
    IF NOT AVAIL area-produc THEN
        RETURN "".

    FIND FIRST ccs-area-produc NO-LOCK 
        WHERE ccs-area-produc.cod-area-produc = area-produc.cod-area-produc NO-ERROR.
    IF AVAIL ccs-area-produc THEN DO:
        RETURN ccs-area-produc.cod-depos.
    END. ELSE DO:
        FOR EACH estrut-area-produc NO-LOCK 
                WHERE estrut-area-produc.cod-area-filho = area-produc-ctrab.cod-area-produc:
            FIND FIRST ccs-area-produc NO-LOCK 
                WHERE ccs-area-produc.cod-area-produc = estrut-area-produc.cod-area-produc NO-ERROR.
            IF AVAIL ccs-area-produc THEN
                RETURN ccs-area-produc.cod-depos.
        END.
    END.
    RETURN "".
END FUNCTION.

FUNCTION deposito-gm-codigo RETURNS CHAR (gm-codigo AS CHAR):

    FIND FIRST grup-maquin NO-LOCK 
        WHERE grup-maquin.gm-codigo = gm-codigo NO-ERROR.
    IF NOT AVAIL grup-maquin THEN
        RETURN "".

    FIND FIRST area-produc OF grup-maquin NO-LOCK NO-ERROR.
    IF NOT AVAIL area-produc THEN
        RETURN "".
    
    FIND FIRST ccs-area-produc NO-LOCK 
        WHERE ccs-area-produc.cod-area-produc = area-produc.cod-area-produc NO-ERROR.
    IF AVAIL ccs-area-produc THEN
        RETURN ccs-area-produc.cod-depos.
    ELSE DO:
        FOR EACH estrut-area-produc NO-LOCK 
                WHERE estrut-area-produc.cod-area-filho = grup-maquin.cod-area-produc:
            FIND FIRST ccs-area-produc NO-LOCK 
                WHERE ccs-area-produc.cod-area-produc = estrut-area-produc.cod-area-produc NO-ERROR.
            IF AVAIL ccs-area-produc THEN
                RETURN ccs-area-produc.cod-depos.
        END.
    END.
    RETURN "".
END FUNCTION.

FUNCTION get-grup-maquina-ctrab RETURNS CHAR ( cod-ctrab AS CHAR ):
    FIND FIRST b-ctrab NO-LOCK 
        WHERE b-ctrab.cod-ctrab = cod-ctrab NO-ERROR.
    IF NOT AVAIL b-ctrab THEN RETURN "".
    RETURN b-ctrab.gm-codigo.
END FUNCTION.

FUNCTION verifica-saldo-reservas RETURNS LOGICAL (nr-ord-prod AS INTEGER,
                                                  op-codigo AS INTEGER,
                                                  qt-reportada AS DECIMAL,
                                                  cod-ctrab AS CHAR):
    DEF VAR cod-depos AS CHAR NO-UNDO.
    
    FIND FIRST b-oper-ord NO-LOCK 
        WHERE b-oper-ord.nr-ord-prod = nr-ord-prod
          AND b-oper-ord.op-codigo = op-codigo NO-ERROR.
    IF NOT AVAIL b-oper-ord THEN DO:
        RUN cria-erro("Saldo insuficiente para algumas reservas desta ordem.~Erro inesperado!").
        RETURN FALSE. /* n∆o Ç para vir para c† */
    END.
    /* E se o split teve o grupo de m†quina alterado? veja IF mais abaixo */
    ASSIGN cod-depos = deposito-gm-codigo(b-oper-ord.gm-codigo).
    IF cod-depos = "" OR get-grup-maquina-ctrab(cod-ctrab) <> b-oper-ord.gm-codigo THEN DO:
        ASSIGN cod-depos = deposito-ctrab(cod-ctrab). /*pode isso ? - tem casos de um centro de trabalho para mais de uma area de produá∆o. */
    END.

    RUN ccs/op/saldo-reservas.p (INPUT 2, /* por operaá∆o */
                                 INPUT nr-ord-prod,
                                 INPUT op-codigo,
                                 INPUT qt-reportada,
                                 INPUT NO, /* n∆o faz as requisiá‰es, s¢ verifica */
                                 INPUT cod-depos,
                                 OUTPUT TABLE tt-erro).

    IF RETURN-VALUE = "NOK" THEN DO:
        FIND FIRST tt-erro NO-LOCK NO-ERROR.
        IF NOT AVAIL tt-erro THEN
            RUN cria-erro("Saldo insuficiente para algumas reservas desta ordem.~nContatar O PCP").
        RETURN FALSE.
    END.
    RETURN TRUE.
END FUNCTION.

PROCEDURE cria-erro:
    DEF INPUT PARAM c-erro AS CHAR NO-UNDO.

    DEF VAR i AS INTEGER NO-UNDO INIT 1.
    FIND LAST tt-erro NO-LOCK NO-ERROR.
    IF AVAIL tt-erro THEN
        ASSIGN i = tt-erro.i-sequen + 1.

    CREATE tt-erro.
    ASSIGN tt-erro.i-sequen = i
           tt-erro.cd-erro  = 17006
           tt-erro.mensagem = c-erro.
END PROCEDURE.

FUNCTION item-ordem-qtd-fracionada RETURNS LOGICAL (nr-ord-prod AS INTEGER):
    DEF BUFFER b-ord-prod FOR ord-prod.
    DEF BUFFER b-item FOR ITEM.

    FIND FIRST b-ord-prod NO-LOCK 
        WHERE b-ord-prod.nr-ord-prod = nr-ord-prod NO-ERROR.
    IF NOT AVAIL b-ord-prod THEN 
        RETURN FALSE.

    FIND FIRST b-item OF b-ord-prod NO-LOCK NO-ERROR.
    IF NOT AVAIL b-item THEN
        RETURN FALSE.

    RETURN b-item.fraciona.
END FUNCTION.
