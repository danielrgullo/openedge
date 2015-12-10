PROCEDURE ShellExecuteA EXTERNAL "shell32" :
    DEFINE INPUT PARAMETER HWND AS LONG.
    DEFINE INPUT PARAMETER lpOperation AS CHARACTER.
    DEFINE INPUT PARAMETER lpFile AS CHARACTER.
    DEFINE INPUT PARAMETER lpParameters AS CHARACTER.
    DEFINE INPUT PARAMETER lpDirectory AS CHARACTER.
    DEFINE INPUT PARAMETER nShowCmd AS LONG.
    DEFINE RETURN PARAMETER hInstance AS LONG.
END.

PROCEDURE FindExecutableA EXTERNAL "shell32.dll":
    define input parameter lpFile as char.
    define input parameter lpDirectory as char.
    define input-output parameter lpResult as char.
    define return parameter hInstance as long.
END PROCEDURE.

PROCEDURE GetShortPathNameA EXTERNAL "KERNEL32":
    DEF INPUT  PARAM lpszLongPath  AS CHAR NO-UNDO.
    DEF INPUT  PARAM lpszShortPath AS LONG NO-UNDO.
    DEF INPUT  PARAM cchBuffer     AS LONG NO-UNDO.
    DEF RETURN PARAM lenBuffer     AS LONG NO-UNDO.
END PROCEDURE.

FUNCTION getShortName RETURNS CHAR (c-arquivo AS CHAR):
    DEF VAR maxSize AS INT INIT 255 NO-UNDO.
    DEF VAR short-name AS MEMPTR.
    REPEAT:
        SET-SIZE(short-name) = maxSize.
        PUT-SHORT(short-name, 1) = 0.
        RUN GetShortPathNameA (c-arquivo,
                               GET-POINTER-VALUE(short-name),
                               GET-SIZE(short-name),
                               OUTPUT maxSize).
        IF GET-SIZE(short-name) >= maxSize THEN 
            LEAVE.
        SET-SIZE(short-name) = 0.
    END.
    RETURN GET-STRING(short-name,1).
END FUNCTION.

PROCEDURE winExecute:
    DEF INPUT PARAM c-exec AS CHAR NO-UNDO.
    DEF INPUT PARAM c-doc  AS CHAR NO-UNDO.

    DEF VAR h-Instance AS INTEGER NO-UNDO.

    RUN ShellExecuteA (0,
                      "open",
                      c-exec, c-doc, 
                      "", 1,
                      OUTPUT h-Instance).
END PROCEDURE.

PROCEDURE openDocument:
    DEF INPUT PARAM c-doc AS CHAR NO-UNDO.

    DEF VAR h-Instance AS INTEGER NO-UNDO.
    DEF VAR c-exec     AS CHAR NO-UNDO.

    ASSIGN c-exec = fill("x",255)
           c-doc  = getShortName(c-doc).
    
    RUN FindExecutableA (INPUT c-doc,
                         INPUT "",
                         INPUT-OUTPUT c-exec, 
                         OUTPUT h-Instance).
    
    IF c-exec = "" OR h-Instance = ? THEN DO:
        MESSAGE "Não foi possivel localizar o programa que abre este arquivo." SKIP
                "Ou o arquivo não existe."
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
    END. ELSE DO:
        RUN winExecute(c-exec, CHR(34) + c-doc + CHR(34)).
    END.
END PROCEDURE.

PROCEDURE openFolder:
    DEF INPUT PARAM folder AS CHAR NO-UNDO.
    RUN winExecute(folder, "").
END PROCEDURE.

PROCEDURE openURL:
    DEF INPUT PARAM c-url AS CHAR NO-UNDO.
    RUN winExecute(c-url, "").
END PROCEDURE.

