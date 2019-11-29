DEF TEMP-TABLE tt-bo-erro 
	FIELD i-sequen AS INTEGER
	FIELD cd-erro AS INTEGER
	FIELD mensagem AS CHARACTER
	FIELD parametros AS CHARACTER
	FIELD errortype AS CHARACTER
	FIELD errorhelp AS CHARACTER
	FIELD errorsubtype AS CHARACTER
	.
DEF VAR h-boin535a AS HANDLE NO-UNDO.

PROCEDURE inicializa-bo:
    IF NOT VALID-HANDLE(h-boin535a) THEN
        RUN inbo/boin535a.p PERSISTENT SET h-boin535a.
END PROCEDURE.

PROCEDURE divide-operacao:
    DEF INPUT PARAM r-rowid AS ROWID NO-UNDO.
    DEF INPUT PARAM qt-apontado AS DECIMAL NO-UNDO.

    DEF VAR qt-saldo AS DECIMAL NO-UNDO.
    DEF VAR c-return-value AS CHAR NO-UNDO.

    RUN inicializa-bo.

    FIND FIRST split-operac 
        WHERE ROWID(split-operac) = r-rowid NO-ERROR. /* n∆o pode ter no-lock ! */
    IF NOT AVAIL split-operac THEN DO:
        RUN destroy-bo.
        RETURN "NOK".
    END.

    ASSIGN qt-saldo = split-operac.qtd-previs-operac - (split-operac.qtd-operac-aprov + 
                                                        split-operac.qtd-operac-refgda).

    IF qt-saldo < qt-apontado THEN DO:
        RUN destroy-bo.
        RETURN "NOK".
    END.
        
    IF qt-saldo - qt-apontado = 0 THEN DO:
        RUN destroy-bo.
        RETURN "NOK".
    END.
        

    RUN DividirSplitQtd IN h-boin535a (BUFFER split-operac,
    								   INPUT qt-apontado,
    								   INPUT qt-saldo - qt-apontado,
    								   OUTPUT TABLE tt-bo-erro).
    ASSIGN c-return-value = RETURN-VALUE.
    RUN destroy-bo.

    RETURN c-return-value.
END.

PROCEDURE get-tt-bo-erro:
    DEF OUTPUT PARAM TABLE FOR tt-bo-erro.
END PROCEDURE.

PROCEDURE unir-operacoes:
    DEF INPUT PARAM r-rowid1 AS ROWID NO-UNDO.
    DEF INPUT PARAM r-rowid2 AS ROWID NO-UNDO.

    RUN inicializa-bo.

    RUN UnirSplit IN h-boin535a(INPUT r-rowid2,
                                INPUT r-rowid1, /* essa que vai ficar */
                                OUTPUT TABLE tt-bo-erro).
    RETURN RETURN-VALUE.
END PROCEDURE.

PROCEDURE destroy-bo:
    RELEASE split-operac.
    IF VALID-HANDLE(h-boin535a) THEN
	    DELETE PROCEDURE h-boin535a NO-ERROR.
    ASSIGN h-boin535a = ?.
END PROCEDURE.

/*
FIND FIRST tt-bo-erro NO-ERROR.
IF AVAILABLE tt-bo-erro THEN DO:
    RUN utp/ut-msgs.p (INPUT "show",
					   INPUT tt-bo-erro.cd-erro,
					   INPUT RETURN-VALUE).
	DELETE tt-bo-erro.
END.*/
