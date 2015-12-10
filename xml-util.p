FUNCTION cria-noderef RETURNS HANDLE (documento AS HANDLE,
                                      descricao AS CHAR,
                                      tipo      AS CHAR):
    DEF VAR elemento AS HANDLE NO-UNDO.
    CREATE X-NODEREF elemento.
    documento:CREATE-NODE(elemento, descricao, tipo).
    RETURN elemento.
END FUNCTION.

FUNCTION novo-elemento RETURNS HANDLE (documento    AS HANDLE,
                                       elemento-pai AS HANDLE,
                                       descricao AS CHAR):
    DEF VAR elemento AS HANDLE NO-UNDO.
    ASSIGN elemento = cria-noderef(documento, descricao, "ELEMENT").
    elemento-pai:APPEND-CHILD(elemento).
    RETURN elemento.
END FUNCTION.

FUNCTION novo-elemento-texto RETURNS HANDLE (documento AS HANDLE,
                                             elemento-pai AS HANDLE,
                                             valor     AS CHAR ).
    DEF VAR elemento AS HANDLE NO-UNDO.
    ASSIGN elemento = cria-noderef(documento, "", "TEXT").
    elemento:NODE-VALUE = valor.
    elemento-pai:APPEND-CHILD(elemento).
    DELETE OBJECT elemento-pai NO-ERROR.
    RETURN elemento.
END FUNCTION.

FUNCTION cria-xml RETURNS HANDLE ():
    DEF VAR documento AS HANDLE NO-UNDO.
    CREATE X-DOCUMENT documento.
    ASSIGN documento:ENCODING = "ISO-8859-1".
    RETURN documento.
END FUNCTION.

PROCEDURE adiciona-valor-elemento:
    DEF INPUT PARAM documento    AS HANDLE NO-UNDO.
    DEF INPUT PARAM elemento-pai AS HANDLE NO-UNDO.
    DEF INPUT PARAM descricao    AS CHAR   NO-UNDO.
    DEF INPUT PARAM valor        AS CHAR   NO-UNDO.

    DELETE OBJECT novo-elemento-texto(documento,
                                      novo-elemento(documento, 
                                                    elemento-pai, 
                                                    descricao),
                                      valor).
END PROCEDURE.

FUNCTION dec2TXT RETURNS CHAR (valor AS DECIMAL):
    RETURN REPLACE(STRING(valor), ",", ".").
END FUNCTION.

FUNCTION dt2SQL RETURNS CHAR (dt AS DATE):
    RETURN STRING(YEAR(dt), "9999") + "-" +
           STRING(MONTH(dt),"99") + "-" +
           STRING(DAY(dt), "99").
END FUNCTION.


/* EXEMPLO :

DEF VAR doc-xml AS LONGCHAR NO-UNDO.

DEF VAR doc AS HANDLE NO-UNDO.
DEF VAR op  AS HANDLE NO-UNDO.

ASSIGN doc  = cria-xml()
       op = novo-elemento(doc, doc, "ordem").

RUN adiciona-valor-elemento(doc, op, 'versao', '1.0').
RUN adiciona-valor-elemento(doc, op, 'usuario', c-seg-usuario).

DELETE OBJECT op NO-ERROR.

doc:SAVE("file", "c:\temp\" + gera-nome-arquivo(nr-ord-prod)).
/*doc:SAVE("longchar", doc-xml).*/

DELETE OBJECT doc NO-ERROR.
*/

