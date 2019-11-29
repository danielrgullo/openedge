 /* 
    RND 4 - Aviso de embarque
 */
FUNCTION fi-achaNF   RETURNS CHAR  ( obs AS CHAR ) FORWARD.
FUNCTION fi-achaData RETURNS DATE  ( cod-estabel AS CHAR, nf AS CHAR, serie AS CHAR ) FORWARD.
FUNCTION fi-achaDataT RETURNS CHAR ( obs AS CHAR ) FORWARD.
FUNCTION isNumeric RETURNS LOGICAL ( num AS CHAR ) FORWARD.
FUNCTION espaco RETURNS CHAR (n AS INTEGER) FORWARD.
 
{include/i-freeac.i}

DEFINE NEW SHARED STREAM arqcatnf.

DEF BUFFER notafiscal FOR nota-fiscal.

DEF TEMP-TABLE tt-it-nota-fisc NO-UNDO
    FIELD it-cliente  AS CHAR
    FIELD ponbr AS CHAR    
    FIELD rev AS CHAR
    FIELD qt-faturada LIKE it-nota-fisc.qt-faturada[1]
    FIELD un LIKE it-nota-fisc.un[1]
    FIELD itipi LIKE it-nota-fisc.vl-ipi-it
    FIELD aliquota-ipi LIKE it-nota-fisc.aliquota-ipi
    FIELD vl-preuni LIKE it-nota-fisc.vl-preuni
    FIELD tipo  AS CHAR FORMAT "x(1)"
    FIELD val-pct-desconto-total LIKE it-nota-fisc.val-pct-desconto-total
    FIELD vl-desconto LIKE it-nota-fisc.vl-desconto
    FIELD aliquota-icm LIKE it-nota-fisc.aliquota-icm
    FIELD vl-bicms-it LIKE it-nota-fisc.vl-bicms-it
    FIELD vl-icms-it LIKE it-nota-fisc.vl-icms-it
    FIELD vl-ipi-it LIKE it-nota-fisc.vl-ipi-it
    FIELD vl-tot-item LIKE it-nota-fisc.vl-tot-item
    FIELD class-fiscal LIKE it-nota-fisc.class-fiscal
    FIELD cod-cfop LIKE natur-oper.cod-cfop
    FIELD ind-tip-nota LIKE nota-fiscal.ind-tip-nota
    FIELD d-dtae8 LIKE doc-fiscal.dt-emis-doc
    FIELD c-nfae8 LIKE it-nota-fisc.nr-docum
    FIELD c-serae8 LIKE it-nota-fisc.serie-docum
    FIELD nat-docum LIKE it-nota-fisc.nat-docum
    .

PROCEDURE geraEDI22:
    DEF INPUT PARAM cod-estabel LIKE nota-fiscal.cod-estabel NO-UNDO.
    DEF INPUT PARAM nr-nota     LIKE nota-fiscal.nr-nota-fis NO-UNDO.
    DEF INPUT PARAM serie       LIKE nota-fiscal.serie       NO-UNDO.

    DEF VAR cli LIKE emitente.cod-emitente NO-UNDO.
    DEF VAR ccs AS CHAR NO-UNDO.

    /*DEF VAR c-te1 AS CHAR NO-UNDO.*/
    DEF VAR c-nfae8  AS CHAR NO-UNDO.
    DEF VAR c-serae8 AS CHAR NO-UNDO.
    DEF VAR d-dtae8  AS DATE NO-UNDO.

    DEF VAR totnf  AS DEC NO-UNDO.
    DEF VAR totipi AS DEC NO-UNDO.
    DEF VAR itipi  AS DEC NO-UNDO.

    DEF VAR cont    AS DECIMAL NO-UNDO.
    DEF VAR qty_it  AS DECIMAL NO-UNDO.
    DEF VAR dtvenc  AS CHAR FORMAT "x(6)" INITIAL "AAMMDD" NO-UNDO.
    DEF VAR dtduedt AS CHAR FORMAT "x(6)" INITIAL "AAMMDD" NO-UNDO.
    DEF VAR especie AS CHAR FORMAT "x(2)" INITIAL "02" NO-UNDO.
    DEF VAR fabrica AS CHAR FORMAT "x(3)" INITIAL "000" NO-UNDO.
    /*? P=Producao R=Reposicao T=Triangulacao E=Exportacao X=Outros A=Amostra F=Ferramentas e Solucoes */
    DEF VAR tipo    AS CHAR FORMAT "x(1)" INITIAL "P" NO-UNDO.
    /* */
    DEF VAR it-cliente   AS CHAR NO-UNDO.
    DEF VAR ponbr   AS CHAR NO-UNDO.
    DEF VAR nlinhas AS INTEGER NO-UNDO.
    DEF VAR ctrl    AS INTEGER NO-UNDO.
    DEF VAR arq     AS CHAR NO-UNDO.

    DEF VAR c-chave-acesso AS CHAR FORMAT "x(44)" NO-UNDO.

    DEF VAR rev   AS CHAR NO-UNDO.
    DEF VAR cgc   AS CHAR NO-UNDO.
    DEF VAR pasta AS CHAR NO-UNDO.

    DEF VAR nome-abrev AS CHAR NO-UNDO.

    DEF VAR tot_icm  AS DECIMAL NO-UNDO.
    DEF VAR tot_bicm AS DECIMAL NO-UNDO.

    DEF VAR c-serie AS CHAR NO-UNDO.

    ASSIGN c-serie = serie.
    IF c-serie = "" THEN c-serie = "0".
    
    FIND FIRST nota-fiscal NO-LOCK WHERE nota-fiscal.cod-estabel = cod-estabel 
                                     AND nota-fiscal.nr-nota-fis = nr-nota  
                                     AND nota-fiscal.serie       = serie NO-ERROR.
    IF NOT AVAIL nota-fiscal THEN
        LEAVE.
    ELSE
        cli = nota-fiscal.cod-emitente.

    IF cli = 10000158 THEN 
        cli = 10000159.

    FIND FIRST emitente NO-LOCK WHERE emitente.cod-emitente = cli NO-ERROR.
    IF NOT AVAIL emitente THEN LEAVE.

    ASSIGN cgc        = emitente.cgc
           nome-abrev = SUBSTR(emitente.nome-emit,1,25).

    ccs = "Q9994G0 ".
    IF cli = 10002155 THEN ccs = "00034857".
    IF cli = 10009063 THEN ccs = "00008499".
    IF cli = 10006602 THEN ccs = "Q9994G6 ".

    IF cli = 10000124 OR cli = 10005825 OR cli = 10006602 THEN
        pasta = "\\192.0.0.149\ediccs$\AV\".
    ELSE IF cli = 10002395 OR cli = 10005276 OR cli = 10005705 OR cli = 10005725 THEN
            pasta = "\\192.0.0.149\ediccs$\sintel\snd\".
    ELSE
        pasta = "\\192.0.0.149\ediccs$\EDI\".
    /*pasta = "C:\work\".*/    

    arq = pasta + cgc + "_RND00422_00964350000178_" +
          STRING(YEAR(TODAY)) + STRING(MONTH(TODAY),"99") + STRING(DAY(TODAY),"99") +
          REPLACE(STRING(TIME,"hh:mm:ss"),":","") + ".TXT".

    FILE-INFO:FILE-NAME = arq.

    DO WHILE FILE-INFO:PATHNAME <> ?:

        arq = pasta + cgc + "_RND00422_00964350000178_" +
              STRING(YEAR(TODAY)) + STRING(MONTH(TODAY),"99") + STRING(DAY(TODAY),"99") +
              STRING(INTEGER(REPLACE(STRING(TIME,"hh:mm:ss"),":","")) + 1, "999999")
              + ".TXT".
        
        FILE-INFO:FILE-NAME = arq.

    END.
    
    OUTPUT STREAM arqcatnf TO VALUE(arq).
    
    nlinhas = 0.

    FOR EACH nota-fiscal NO-LOCK WHERE nota-fiscal.cod-estabel = cod-estabel 
                                     AND nota-fiscal.nr-nota-fis = nr-nota  
                                     AND nota-fiscal.serie       = serie :
        
        /*IF zn_type = "D" THEN NEXT. 
        IF zn_cancel_date <> ? THEN NEXT.*/
    
        ctrl = 0.
        RUN contaEnvios(nota-fiscal.dt-emis-nota, OUTPUT ctrl).

        /*
           ITP
        */
        PUT STREAM arqcatnf UNFORMATTED
                "ITP" 
                "00422"
                STRING(ctrl, "99999")
                SUBSTRING(STRING(YEAR(TODAY)),3,2) STRING(MONTH(TODAY),"99") STRING(DAY(TODAY),"99") 
                REPLACE(STRING(TIME,"hh:mm:ss"),":","")
                "00964350000178" + cgc + ccs + "        CCS TENOLOGIA INDUSTRIAL " nome-abrev + FILL(" ", 25 - LENGTH(nome-abrev))
                FILL(" ",9)
                SKIP .
    
        nlinhas = nlinhas + 1.
    
        qty_it = 0.
        RUN contaItems(INPUT nota-fiscal.cod-estabel, 
                       INPUT nota-fiscal.nr-nota-fis, 
                       INPUT nota-fiscal.serie, 
                       OUTPUT qty_it,
                       OUTPUT tot_icm,
                       OUTPUT tot_bicm,
                       OUTPUT dtduedt
                      ).
    
        FIND FIRST fat-duplic WHERE fat-duplic.cod-estabel = nota-fiscal.cod-estabel 
                                AND fat-duplic.serie       = nota-fiscal.serie
                                AND fat-duplic.nr-fatura   = nota-fiscal.nr-nota-fis NO-LOCK NO-ERROR.
        IF NOT AVAIL fat-duplic THEN
            dtvenc = "000000".
        ELSE
            dtvenc = SUBSTRING(STRING(YEAR(fat-duplic.dt-venciment)),3,2) + STRING(MONTH(fat-duplic.dt-venciment),"99") + STRING(DAY(fat-duplic.dt-venciment),"99").

         /*IF dtduedt = "000000" THEN*/
            dtduedt = SUBSTRING(STRING(YEAR(nota-fiscal.dt-emis-nota + 1)),3,2) + STRING(MONTH(nota-fiscal.dt-emis-nota + 1), "99") + STRING(DAY(nota-fiscal.dt-emis-nota + 1), "99").
        
        IF SUBSTRING(nota-fiscal.nr-pedcli,4,1) = "P" THEN
            fabrica = "028".
        ELSE IF SUBSTRING(nota-fiscal.nr-pedcli,4,1) = "T" THEN
            fabrica = "081".
        ELSE IF SUBSTRING(nota-fiscal.nr-pedcli,4,1) = "C" THEN
            fabrica = "010".
        ELSE
            fabrica = "000".

        IF nota-fiscal.ind-tip-nota = 3 OR nota-fiscal.ind-tip-nota = 4 THEN 
            ASSIGN tipo = "X"
                   especie = "04".
        ELSE
            ASSIGN tipo = "P"
                   especie = "02".
    
        totipi = 0.
        FIND FIRST natur-oper OF nota-fiscal NO-LOCK NO-ERROR.
        IF AVAIL natur-oper AND natur-oper.cd-trib-ipi = 1 THEN
            totipi = nota-fiscal.vl-tot-ipi.

        /*
           AE1
        */
        PUT STREAM arqcatnf UNFORMATTED  
                  "AE1" 
                  STRING(INT(nota-fiscal.nr-nota-fis), "999999") c-serie FORMAT "x(4)"
                  SUBSTRING(STRING(YEAR(nota-fiscal.dt-emis-nota)),3,2) + STRING(MONTH(nota-fiscal.dt-emis-nota), "99") + STRING(DAY(nota-fiscal.dt-emis-nota), "99")
                  STRING(qty_it, "999")
                  REPLACE(STRING(nota-fiscal.vl-tot-nota, "999999999999999.99"), ",","")
                  "0"
                  STRING(INT(REPLACE(natur-oper.cod-cfop,",","")) , "99999")
                  REPLACE(STRING(tot_icm, "999999999999999.99"), ",","")
                  dtvenc
                  especie /* vede anexo ? */
                  REPLACE(STRING(totipi,  "999999999999999.99"), ",","")
                  fabrica
                  SUBSTRING(STRING(YEAR(nota-fiscal.dt-emis-nota + 1)),3,2) + STRING(MONTH(nota-fiscal.dt-emis-nota + 1), "99") + STRING(DAY(nota-fiscal.dt-emis-nota + 1), "99")
                  "0000"
                  FILL(" ",15)
                  SUBSTRING(STRING(YEAR(nota-fiscal.dt-emis-nota)),3,2) + STRING(MONTH(nota-fiscal.dt-emis-nota), "99") + STRING(DAY(nota-fiscal.dt-emis-nota), "99")  
                  "0000"
                  FILL(" ",3) /* filler */ 
                  SKIP.
    
        nlinhas = nlinhas + 1.

        /*
            NF6 - complemento : NFE
        */

        ASSIGN c-chave-acesso = SUBSTRING(nota-fiscal.char-2,3,44).

        PUT STREAM arqcatnf UNFORMATTED  
                  "NF6"
                  c-chave-acesso FILL(" ", 44 - LENGTH(c-chave-acesso))
                  FILL(" ",20) /* ato concessorio ? */ 
                  FILL(" ",61) /* filler */ 
                  SKIP.
    
        /*
           NF2
        */

        FIND FIRST natur-oper OF nota-fiscal NO-LOCK NO-ERROR.
        
        PUT STREAM arqcatnf UNFORMATTED  
                  "NF2" 
                  FILL("0", 48)  /* 4 * 12 */
                  REPLACE(STRING(tot_bicm, "9999999999.99"), ",","")
                  FILL("0", 24) /* 12 + 6 * 2 */
                  FILL(" ", 7)  /* 4 + 3 */
                  STRING(INT(REPLACE(natur-oper.cod-cfop,",","")), "99999")
                  FILL("0", 12)
                  FILL(" ",15) 
                  FILL(" ",2) 
                  SKIP.
    
        nlinhas = nlinhas + 1.
    
        IF cli <> 10002155 AND cli <> 10009063 THEN DO:
            /*
               NF3
            */                  
            PUT STREAM arqcatnf UNFORMATTED  
                     "NF3" 
                     dtvenc 
                     FILL("0", 54) 
                     FILL(" ", 65)
                     SKIP.
        
            nlinhas = nlinhas + 1.
        END.
    
        cont = 0.
    
        FIND FIRST edi-ccs NO-LOCK WHERE edi-ccs.nr-nota-fis  = nota-fiscal.nr-nota-fis
                                     AND edi-ccs.serie        = nota-fiscal.serie NO-ERROR.
        IF NOT AVAIL edi-ccs THEN DO:
            CREATE edi-ccs.
            ASSIGN edi-ccs.nr-nota-fis  = nota-fiscal.nr-nota-fis
                   edi-ccs.serie        = nota-fiscal.serie
                   edi-ccs.dt-emis-nota = nota-fiscal.dt-emis-nota
                   edi-ccs.hora         = STRING(TIME, "HH:MM").
        END.

        /*
         *************************** ITENS NA NOTA-FISCAL ****************************
        */
        EMPTY TEMP-TABLE tt-it-nota-fisc.
        FOR EACH it-nota-fisc NO-LOCK WHERE it-nota-fisc.cod-estabel = nota-fiscal.cod-estabel
                                        AND it-nota-fisc.nr-nota-fis = nota-fiscal.nr-nota-fis
                                        AND it-nota-fisc.serie       = nota-fiscal.serie :
        
            IF it-nota-fisc.it-codigo = "" THEN NEXT.
            FIND FIRST ITEM NO-LOCK WHERE ITEM.it-codigo = it-nota-fisc.it-codigo NO-ERROR.
            IF NOT AVAIL ITEM THEN NEXT.

            FIND FIRST natur-oper OF it-nota-fisc NO-LOCK NO-ERROR.
            IF NOT AVAIL natur-oper THEN NEXT.

            IF ITEM.codigo-refer = "" OR ITEM.it-codigo = "" THEN DO:
                ASSIGN it-cliente = "S/CODIGO"
                       rev        = "".
            END. ELSE DO:
                ASSIGN it-cliente = REPLACE(REPLACE( ENTRY(1, ITEM.codigo-refer, "#") ,"-",""), " ", "")
                       rev        = ENTRY(2, ITEM.codigo-refer, "#" ) NO-ERROR.
            END.

            IF it-nota-fisc.nr-pedcli = "" THEN
                ponbr = "".
            ELSE
                ponbr = SUBSTRING(it-nota-fisc.nr-pedcli,1,12).

            IF cli = 10002155 AND NOT ponbr BEGINS "#" THEN ponbr = "5500008945".
            ASSIGN ponbr = REPLACE(ponbr, "#","").

            FIND FIRST tt-it-nota-fisc NO-LOCK WHERE tt-it-nota-fisc.it-cliente = it-cliente NO-ERROR.
            IF NOT AVAIL tt-it-nota-fisc THEN DO:
                CREATE tt-it-nota-fisc.
                ASSIGN tt-it-nota-fisc.it-cliente   = it-cliente
                       tt-it-nota-fisc.rev          = rev
                       tt-it-nota-fisc.un           = SUBSTRING(it-nota-fisc.un[1], 1, 2)
                       tt-it-nota-fisc.ponbr        = ponbr                       
                       tt-it-nota-fisc.vl-preuni    = it-nota-fisc.vl-preuni
                       tt-it-nota-fisc.aliquota-ipi = it-nota-fisc.aliquota-ipi
                       tt-it-nota-fisc.aliquota-icm = it-nota-fisc.aliquota-icm
                       tt-it-nota-fisc.class-fiscal = it-nota-fisc.class-fiscal
                       tt-it-nota-fisc.cod-cfop     = natur-oper.cod-cfop
                       tt-it-nota-fisc.nat-docum    = it-nota-fisc.nat-docum
                       tt-it-nota-fisc.ind-tip-nota = nota-fiscal.ind-tip-nota
                       tt-it-nota-fisc.val-pct-desconto-total = it-nota-fisc.val-pct-desconto-total
                       .
                
                IF cli = 10000124 OR cli = 10005825 OR cli = 10006602 THEN 
                    ASSIGN tt-it-nota-fisc.un = "EA".
            END.            
            ASSIGN tt-it-nota-fisc.qt-faturada = tt-it-nota-fisc.qt-faturada + it-nota-fisc.qt-faturada[1]
                   tt-it-nota-fisc.vl-tot-item = tt-it-nota-fisc.vl-tot-item + it-nota-fisc.vl-tot-item
                   tt-it-nota-fisc.vl-desconto = tt-it-nota-fisc.vl-desconto + it-nota-fisc.vl-desconto
                   tt-it-nota-fisc.vl-bicms-it = tt-it-nota-fisc.vl-bicms-it + it-nota-fisc.vl-bicms-it
                   tt-it-nota-fisc.vl-icms-it  = tt-it-nota-fisc.vl-icms-it  + it-nota-fisc.vl-icms-it
                   tt-it-nota-fisc.vl-ipi-it   = tt-it-nota-fisc.vl-ipi-it   + it-nota-fisc.vl-ipi-it
                   .
        END.

        FOR EACH tt-it-nota-fisc NO-LOCK:
            cont = cont + 1.            

            /*
               AE2 
            */
            PUT STREAM arqcatnf UNFORMATTED  
                     "AE2" 
                     STRING(cont, "999")
                     UPPER(tt-it-nota-fisc.ponbr + FILL(" ", 12 - LENGTH(tt-it-nota-fisc.ponbr)))
                     UPPER(tt-it-nota-fisc.it-cliente + FILL(" ", 30 - LENGTH(tt-it-nota-fisc.it-cliente)))
                     STRING(tt-it-nota-fisc.qt-faturada, "999999999")
                     tt-it-nota-fisc.un FILL(" ", 2 - LENGTH(tt-it-nota-fisc.un))
                     STRING( INT(REPLACE(tt-it-nota-fisc.class-fiscal, ",","")), "9999999999" ).

            IF totipi = 0 THEN 
                 PUT STREAM arqcatnf UNFORMATTED "0000".
            ELSE
                 PUT STREAM arqcatnf UNFORMATTED REPLACE(STRING(tt-it-nota-fisc.aliquota-ipi, "99.99"), ",","").

            PUT STREAM arqcatnf UNFORMATTED  
                     REPLACE(STRING(ABS(tt-it-nota-fisc.vl-preuni), "9999999.99999"), ",","")
                     FILL("0", 9) + FILL(" ", 2)
                     FILL("0", 9) + tt-it-nota-fisc.un + FILL(" ", 2 - LENGTH(tt-it-nota-fisc.un))
                     tipo 
                     REPLACE(STRING(ABS(tt-it-nota-fisc.val-pct-desconto-total), "99.99"), ",","")
                     REPLACE(STRING(ABS(tt-it-nota-fisc.vl-desconto), "999999999.99"), ",","")
                     FILL(" ", 4 - LENGTH(SUBSTRING(tt-it-nota-fisc.rev ,1,4)))  SUBSTRING(tt-it-nota-fisc.rev , 1, 4) 
                     FILL(" ", 1)
                     SKIP .
            nlinhas = nlinhas + 1.
            IF totipi = 0 THEN
                itipi = 0.
            ELSE
                itipi = tt-it-nota-fisc.vl-ipi-it.
            /*
                AE4
            */
            PUT STREAM arqcatnf UNFORMATTED  
                     "AE4"
                     REPLACE(STRING(tt-it-nota-fisc.aliquota-icm, "99.99"), ",","")
                     REPLACE(STRING(tt-it-nota-fisc.vl-bicms-it, "999999999999999.99"), ",","")
                     REPLACE(STRING(tt-it-nota-fisc.vl-icms-it , "999999999999999.99"), ",","")
                     REPLACE(STRING(itipi  , "999999999999999.99"), ",","")
                     "00" /* codigo de situacao tributaria ? */
                     FILL(" ", 30) /* codigo-refer? */
                     "000000" 
                     FILL(" ", 13) 
                     FILL(" ", 5)
                     " "
                     REPLACE(STRING(tt-it-nota-fisc.vl-tot-item, "9999999999.99"), ",","")
                     " "
                     SKIP.
            nlinhas = nlinhas + 1.
            IF cli <> 10002155 AND cli <> 10009063 THEN DO:
                /*
                    AE6
                */
                PUT STREAM arqcatnf UNFORMATTED  
                         "AE6"
                         "BR"
                         FILL(" ", 123)
                         SKIP.
        
                nlinhas = nlinhas + 1.
            END.

            IF cli <> 10002155 AND cli <> 10009063 THEN DO:
                /*
                   AE7
                */
                PUT STREAM arqcatnf UNFORMATTED  
                          "AE7"
                          FILL("0", 12)
                          STRING(INT(REPLACE(tt-it-nota-fisc.cod-cfop,",","")), "99999")
                          FILL("0", 48)
                          FILL(" ", 5) 
                          FILL("0", 12)
                          FILL(" ", 41) 
                          FILL(" ", 2) /* filler */
                          SKIP.
                nlinhas = nlinhas + 1.
            END.
            /*IF cli <> 10002155 AND cli <> 10009063 THEN DO:
                IF tt-it-nota-fisc.ind-tip-nota = 3 OR tt-it-nota-fisc.ind-tip-nota = 4 THEN DO:
                       
                    FIND FIRST doc-fiscal NO-LOCK WHERE doc-fiscal.cod-estabel  = nota-fiscal.cod-estabel
                                                    AND doc-fiscal.serie        = tt-it-nota-fisc.c-serae8
                                                    AND doc-fiscal.nr-doc-fis   = tt-it-nota-fisc.c-nfae8
                                                    AND doc-fiscal.cod-emitente = nota-fiscal.cod-emitente
                                                    AND doc-fiscal.nat-oper     = tt-it-nota-fisc.nat-docum NO-ERROR.
                    IF AVAIL doc-fiscal THEN
                        ASSIGN d-dtae8 = doc-fiscal.dt-emis-doc.
    
                    IF TRIM(tt-it-nota-fisc.c-nfae8) <> "" THEN DO:
    
                        ASSIGN c-nfae8 = STRING(INT(c-nfae8),"999999").
                        /*
                           AE8 - NF Remessa
                        */
                        PUT STREAM arqcatnf UNFORMATTED  
                                  "AE8"
                                  c-nfae8
                                  tt-it-nota-fisc.c-serae8
                                  SUBSTRING(STRING(YEAR(d-dtae8)),3,2) + STRING(MONTH(d-dtae8), "99") + STRING(DAY(d-dtae8), "99")  
                                  "000"
                                  FILL(" ", 16)
                                  FILL(" ", 17)
                                  FILL(" ", 10)
                                  FILL(" ", 6)
                                  FILL(" ", 57) /* filler */
                                  SKIP.
                
                        nlinhas = nlinhas + 1.
                    END.
                END.
            END. temporariamente off */ /* cli <> 10002155 ... */
        END.
    END. /* nota-fiscal */        
    nlinhas = nlinhas + 1.
    /*
        FTP
    */
    PUT STREAM arqcatnf UNFORMATTED
            "FTP"
            STRING(ctrl, "99999")
            STRING(nlinhas, "999999999")
            FILL("0", 17)
            "D"
            FILL(" ", 93) /* filler */
            SKIP.
    OUTPUT STREAM arqcatnf CLOSE.
END PROCEDURE.

PROCEDURE geraEDI18:
    DEF INPUT PARAM cod-estabel LIKE nota-fiscal.cod-estabel NO-UNDO.
    DEF INPUT PARAM nr-nota     LIKE nota-fiscal.nr-nota-fis NO-UNDO.
    DEF INPUT PARAM serie       LIKE nota-fiscal.serie       NO-UNDO.

    DEF VAR cli LIKE emitente.cod-emitente NO-UNDO.
    DEF VAR ccs AS CHAR NO-UNDO.

    /*DEF VAR c-te1 AS CHAR NO-UNDO.*/
    DEF VAR c-nfae8  AS CHAR NO-UNDO.
    DEF VAR c-serae8 AS CHAR NO-UNDO.
    DEF VAR d-dtae8  AS DATE NO-UNDO.

    DEF VAR totnf  AS DEC NO-UNDO.
    DEF VAR totipi AS DEC NO-UNDO.
    DEF VAR itipi  AS DEC NO-UNDO.

    DEF VAR cont    AS DECIMAL NO-UNDO.
    DEF VAR qty_it  AS DECIMAL NO-UNDO.
    DEF VAR dtvenc  AS CHAR FORMAT "x(6)" INITIAL "AAMMDD" NO-UNDO.
    DEF VAR dtduedt AS CHAR FORMAT "x(6)" INITIAL "AAMMDD" NO-UNDO.
    DEF VAR especie AS CHAR FORMAT "x(2)" INITIAL "02" NO-UNDO.
    DEF VAR fabrica AS CHAR FORMAT "x(3)" INITIAL "000" NO-UNDO.
    /*? P=Producao R=Reposicao T=Triangulacao E=Exportacao X=Outros A=Amostra F=Ferramentas e Solucoes */
    DEF VAR tipo    AS CHAR FORMAT "x(1)" INITIAL "P" NO-UNDO.
    /* */
    DEF VAR it-cliente   AS CHAR NO-UNDO.
    DEF VAR ponbr   AS CHAR NO-UNDO.
    DEF VAR nlinhas AS INTEGER NO-UNDO.
    DEF VAR ctrl    AS INTEGER NO-UNDO.
    DEF VAR arq     AS CHAR NO-UNDO.

    DEF VAR rev   AS CHAR NO-UNDO.
    DEF VAR cgc   AS CHAR NO-UNDO.
    DEF VAR pasta AS CHAR NO-UNDO.

    DEF VAR nome-abrev AS CHAR NO-UNDO.

    DEF VAR tot_icm  AS DECIMAL NO-UNDO.
    DEF VAR tot_bicm AS DECIMAL NO-UNDO.

    DEF VAR c-serie AS CHAR NO-UNDO.
    DEF VAR un AS CHAR FORMAT "x(2)" NO-UNDO.

    ASSIGN c-serie = serie.
    IF c-serie = "" THEN c-serie = "0".

    FIND FIRST nota-fiscal NO-LOCK WHERE nota-fiscal.cod-estabel = cod-estabel 
                                     AND nota-fiscal.nr-nota-fis = nr-nota  
                                     AND nota-fiscal.serie       = serie NO-ERROR.
    IF NOT AVAIL nota-fiscal THEN
        LEAVE.
    ELSE
        cli = nota-fiscal.cod-emitente.

    IF cli = 10000158 THEN 
        cli = 10000159.

    FIND FIRST emitente NO-LOCK WHERE emitente.cod-emitente = cli NO-ERROR.
    IF NOT AVAIL emitente THEN LEAVE.

    ASSIGN cgc        = emitente.cgc
           nome-abrev = SUBSTR(emitente.nome-emit,1,25).

    ccs = "Q9994G0 ".
    IF cli = 10002155 THEN ccs = "00034857".
    IF cli = 10009063 THEN ccs = "00008499".
    IF cli = 10006602 THEN ccs = "Q9994G6 ".

    IF cli = 10000124 OR cli = 10005825 OR cli = 10006602 THEN
        pasta = "\\192.0.0.149\ediccs$\AV\".
    ELSE IF cli = 10002395 OR cli = 10005276 OR cli = 10005705 OR cli = 10005725 THEN
            pasta = "\\192.0.0.149\ediccs$\sintel\snd\".
    ELSE
        pasta = "\\192.0.0.149\ediccs$\EDI\".

    arq = pasta + cgc + "_RND00418_00964350000178_" +
          STRING(YEAR(TODAY)) + STRING(MONTH(TODAY),"99") + STRING(DAY(TODAY),"99") +
          REPLACE(STRING(TIME,"hh:mm:ss"),":","") + ".TXT".
    FILE-INFO:FILE-NAME = arq.
    DO WHILE FILE-INFO:PATHNAME <> ?:
        arq = pasta + cgc + "_RND00418_00964350000178_" +
              STRING(YEAR(TODAY)) + STRING(MONTH(TODAY),"99") + STRING(DAY(TODAY),"99") +
              STRING(INTEGER(REPLACE(STRING(TIME,"hh:mm:ss"),":","")) + 1, "999999")
              + ".TXT".
        FILE-INFO:FILE-NAME = arq.
    END.
    OUTPUT STREAM arqcatnf TO VALUE(arq).
    
    nlinhas = 0.
    FOR EACH nota-fiscal NO-LOCK WHERE nota-fiscal.cod-estabel = cod-estabel 
                                     AND nota-fiscal.nr-nota-fis = nr-nota  
                                     AND nota-fiscal.serie       = serie :
        ctrl = 0.
        RUN contaEnvios(nota-fiscal.dt-emis-nota, OUTPUT ctrl).
        /*
           ITP
        */
        PUT STREAM arqcatnf UNFORMATTED
                "ITP" 
                "00418"
                STRING(ctrl, "99999")
                SUBSTRING(STRING(YEAR(TODAY)),3,2) STRING(MONTH(TODAY),"99") STRING(DAY(TODAY),"99") 
                REPLACE(STRING(TIME,"hh:mm:ss"),":","")
                "00964350000178" + cgc + ccs + "        CCS TENOLOGIA INDUSTRIAL " nome-abrev + FILL(" ", 25 - LENGTH(nome-abrev))
                FILL(" ",9)
                SKIP .
    
        nlinhas = nlinhas + 1.
    
        qty_it = 0.
        RUN contaItems(INPUT nota-fiscal.cod-estabel, 
                       INPUT nota-fiscal.nr-nota-fis, 
                       INPUT nota-fiscal.serie, 
                       OUTPUT qty_it,
                       OUTPUT tot_icm,
                       OUTPUT tot_bicm,
                       OUTPUT dtduedt
                      ).
    
        FIND FIRST fat-duplic WHERE fat-duplic.cod-estabel = nota-fiscal.cod-estabel 
                                AND fat-duplic.serie       = nota-fiscal.serie
                                AND fat-duplic.nr-fatura   = nota-fiscal.nr-nota-fis NO-LOCK NO-ERROR.
        IF NOT AVAIL fat-duplic THEN
            dtvenc = "000000".
        ELSE
            dtvenc = STRING(DAY(fat-duplic.dt-venciment),"99") + STRING(MONTH(fat-duplic.dt-venciment),"99") + SUBSTRING(STRING(YEAR(fat-duplic.dt-venciment)),3,2).

         /*IF dtduedt = "000000" THEN*/
            dtduedt = SUBSTRING(STRING(YEAR(nota-fiscal.dt-emis-nota + 1)),3,2) + STRING(MONTH(nota-fiscal.dt-emis-nota + 1), "99") + STRING(DAY(nota-fiscal.dt-emis-nota + 1), "99").
        
        IF SUBSTRING(nota-fiscal.nr-pedcli,4,1) = "P" THEN
            fabrica = "028".
        ELSE IF SUBSTRING(nota-fiscal.nr-pedcli,4,1) = "T" THEN
            fabrica = "081".
        ELSE IF SUBSTRING(nota-fiscal.nr-pedcli,4,1) = "C" THEN
            fabrica = "010".
        ELSE
            fabrica = "000".

        IF nota-fiscal.ind-tip-nota = 3 OR nota-fiscal.ind-tip-nota = 4 THEN 
            ASSIGN tipo = "X"
                   especie = "04".
        ELSE
            ASSIGN tipo = "P"
                   especie = "02".
    
        totipi = 0.
        FIND FIRST natur-oper OF nota-fiscal NO-LOCK NO-ERROR.
        IF AVAIL natur-oper AND natur-oper.cd-trib-ipi = 1 THEN
            totipi = nota-fiscal.vl-tot-ipi.

        /*
           AE1
        */
        PUT STREAM arqcatnf UNFORMATTED  
                  "AE1" STRING(INT(nota-fiscal.nr-nota-fis), "999999") c-serie FORMAT "x(4)"
                  SUBSTRING(STRING(YEAR(nota-fiscal.dt-emis-nota)),3,2) + STRING(MONTH(nota-fiscal.dt-emis-nota), "99") + STRING(DAY(nota-fiscal.dt-emis-nota), "99")
                  STRING(qty_it, "999")
                  REPLACE(STRING(nota-fiscal.vl-tot-nota, "999999999999999.99"), ",","")
                  "0"
                  STRING(INT(REPLACE(natur-oper.cod-cfop,",","")) , "99999")
                  REPLACE(STRING(tot_icm, "999999999999999.99"), ",","")
                  dtvenc
                  especie /* vede anexo ? */
                  REPLACE(STRING(totipi,  "999999999999999.99"), ",","")
                  fabrica
                  SUBSTRING(STRING(YEAR(nota-fiscal.dt-emis-nota + 1)),3,2) + STRING(MONTH(nota-fiscal.dt-emis-nota + 1), "99") + STRING(DAY(nota-fiscal.dt-emis-nota + 1), "99")
                  "0000"
                  FILL(" ",15)
                  SUBSTRING(STRING(YEAR(nota-fiscal.dt-emis-nota)),3,2) + STRING(MONTH(nota-fiscal.dt-emis-nota), "99") + STRING(DAY(nota-fiscal.dt-emis-nota), "99")  
                  "0000"
                  FILL(" ",3) /* filler */ 
                  SKIP.
    
        nlinhas = nlinhas + 1.
    
        /*
           NF2
        */
        PUT STREAM arqcatnf UNFORMATTED  
                  "NF2" 
                  FILL("0", 48) 
                  REPLACE(STRING(tot_bicm, "9999999999.99"), ",","")
                  FILL(" ", 65) 
                  SKIP.
    
        nlinhas = nlinhas + 1.
    
        /*
           NF3
        */                  
        PUT STREAM arqcatnf UNFORMATTED  
                 "NF3" 
                 dtvenc 
                 FILL("0", 54) 
                 FILL(" ", 65)
                 SKIP.
    
        nlinhas = nlinhas + 1.
    
        cont = 0.
    
        FIND FIRST edi-ccs NO-LOCK WHERE edi-ccs.nr-nota-fis  = nota-fiscal.nr-nota-fis
                                     AND edi-ccs.serie        = nota-fiscal.serie NO-ERROR.
        IF NOT AVAIL edi-ccs THEN DO:
            CREATE edi-ccs.
            ASSIGN edi-ccs.nr-nota-fis  = nota-fiscal.nr-nota-fis
                   edi-ccs.serie        = nota-fiscal.serie
                   edi-ccs.dt-emis-nota = nota-fiscal.dt-emis-nota
                   edi-ccs.hora         = STRING(TIME, "HH:MM").
        END.

        /*
         *************************** ZND DETALHE ****************************
        */
        FOR EACH it-nota-fisc NO-LOCK WHERE it-nota-fisc.cod-estabel = nota-fiscal.cod-estabel
                                        AND it-nota-fisc.nr-nota-fis = nota-fiscal.nr-nota-fis
                                        AND it-nota-fisc.serie       = nota-fiscal.serie :
        
            /*IF it-nota-fisc.it-codigo = "" THEN NEXT.*/
            FIND FIRST ITEM NO-LOCK WHERE ITEM.it-codigo = it-nota-fisc.it-codigo NO-ERROR.
            IF NOT AVAIL ITEM THEN NEXT.

            cont = cont + 1.            

            IF it-nota-fisc.it-codigo = "" THEN DO:
                ASSIGN it-cliente = "S/CODIGO"
                       rev   = "".
            END. ELSE DO:
                IF ITEM.codigo-refer = "" THEN DO:
    
                    ASSIGN it-cliente = "S/CODIGO"
                           rev   = "".
    
                END. ELSE DO:
                    it-cliente = REPLACE(REPLACE( ENTRY(1, ITEM.codigo-refer, "#") ,"-",""), " ", "") NO-ERROR.
                    rev   = "".
                    rev   = ENTRY(2, ITEM.codigo-refer, "#" ) NO-ERROR.
                END.
            END.

            IF it-nota-fisc.nr-pedcli = "" THEN  
                ponbr = "".
            ELSE
                ponbr = SUBSTRING(it-nota-fisc.nr-pedcli,1,9).

            IF cli = 10002155 AND NOT ponbr BEGINS "#" THEN ponbr = "5500008945".
            ASSIGN ponbr = REPLACE(ponbr, "#","").
                    
            ASSIGN un = SUBSTRING(it-nota-fisc.un[1], 1, 2).
            IF cli = 10000124 OR cli = 10005825 OR cli = 10006602 THEN un = "EA".

            /*
               AE2 
            */
            PUT STREAM arqcatnf UNFORMATTED  
                     "AE2" 
                     STRING(cont, "999")
                     UPPER(ponbr + FILL(" ", 12 - LENGTH(ponbr)))
                     UPPER(it-cliente + FILL(" ", 30 - LENGTH(it-cliente)))
                     STRING(it-nota-fisc.qt-faturada[1], "999999999")
                     un FILL(" ", 2 - LENGTH(un))
                     STRING( INT(REPLACE(it-nota-fisc.class-fiscal, ",","")), "9999999999" ).

            IF totipi = 0 THEN 
                 PUT STREAM arqcatnf UNFORMATTED "0000".
            ELSE
                 PUT STREAM arqcatnf UNFORMATTED REPLACE(STRING(it-nota-fisc.aliquota-ipi, "99.99"), ",","").

            PUT STREAM arqcatnf UNFORMATTED  
                     REPLACE(STRING(ABS(it-nota-fisc.vl-preuni), "9999999.99999"), ",","")
                     FILL("0", 9) + FILL(" ", 2)
                     FILL("0", 9) + "EA"
                     tipo 
                     REPLACE(STRING(ABS(it-nota-fisc.val-pct-desconto-total), "99.99"), ",","")
                     REPLACE(STRING(ABS(it-nota-fisc.vl-desconto), "999999999.99"), ",","")
                     FILL(" ", 4 - LENGTH( SUBSTRING( rev ,1,4) ) )  SUBSTRING( rev , 1, 4) 
                     FILL(" ", 1)
                     SKIP .
    
            nlinhas = nlinhas + 1.

            IF totipi = 0 THEN
                itipi = 0.
            ELSE
                itipi = it-nota-fisc.vl-ipi-it.
        
            /*
                AE4
            */
            PUT STREAM arqcatnf UNFORMATTED  
                     "AE4"
                     REPLACE(STRING(it-nota-fisc.aliquota-icm, "99.99"), ",","")
                     REPLACE(STRING(it-nota-fisc.vl-bicms-it, "999999999999999.99"), ",","")
                     REPLACE(STRING(it-nota-fisc.vl-icms-it , "999999999999999.99"), ",","")
                     REPLACE(STRING(itipi  , "999999999999999.99"), ",","")
                     "00"
                     FILL(" ", 30) "000000" 
                     FILL(" ", 13) FILL(" ", 5)
                     " "
                     REPLACE(STRING(it-nota-fisc.vl-tot-item, "9999999999.99"), ",","")
                     " "
                     /*FILL(" ", 10)*/
                     SKIP.
    
            nlinhas = nlinhas + 1.
           
            /*
                AE6
            */
            PUT STREAM arqcatnf UNFORMATTED  
                     "AE6"
                     "BR"
                     FILL(" ", 123)
                     SKIP.
    
            nlinhas = nlinhas + 1.

            FIND FIRST natur-oper OF it-nota-fisc NO-LOCK NO-ERROR.

            /*
               AE7
            */
            PUT STREAM arqcatnf UNFORMATTED  
                      "AE7"
                      FILL("0", 12)
                      STRING(INT(REPLACE(natur-oper.cod-cfop,",","")), "99999") 
                      FILL("0", 48)
                      FILL(" ", 5) /* filler */
                      FILL("0", 12)
                      FILL(" ", 43) /* filler */
                      SKIP.
    
            nlinhas = nlinhas + 1.

            IF nota-fiscal.ind-tip-nota = 3 OR nota-fiscal.ind-tip-nota = 4 THEN DO:

                ASSIGN c-nfae8  = it-nota-fisc.nr-docum
                       c-serae8 = it-nota-fisc.serie-docum.

                FIND FIRST doc-fiscal NO-LOCK WHERE doc-fiscal.cod-estabel  = nota-fiscal.cod-estabel
                                                AND doc-fiscal.serie        = c-serae8
                                                AND doc-fiscal.nr-doc-fis   = c-nfae8
                                                AND doc-fiscal.cod-emitente = nota-fiscal.cod-emitente
                                                AND doc-fiscal.nat-oper     = it-nota-fisc.nat-docum NO-ERROR.
                IF AVAIL doc-fiscal THEN
                    ASSIGN d-dtae8 = doc-fiscal.dt-emis-doc.

                IF TRIM(c-nfae8) <> "" THEN DO:

                    ASSIGN c-nfae8 = STRING(INT(c-nfae8),"999999").

                    /*
                       AE8
                    */
                    PUT STREAM arqcatnf UNFORMATTED  
                              "AE8"
                              c-nfae8
                              c-serae8
                              SUBSTRING(STRING(YEAR(d-dtae8)),3,2) + STRING(MONTH(d-dtae8), "99") + STRING(DAY(d-dtae8), "99")  
                              "000"
                              FILL(" ", 16)
                              FILL(" ", 17)
                              FILL(" ", 10)
                              FILL(" ", 63) /* filler */
                              SKIP.
            
                    nlinhas = nlinhas + 1.
                END.
            END.
        END.

        /*IF nota-fiscal.ind-tip-nota = 3 OR nota-fiscal.ind-tip-nota = 4 THEN DO:

            c-te1 = fn-free-accent(natur-oper.denominacao) + " Referente Ös NFS: " + REPLACE(fi-achaNF(nota-fiscal.observ-nota), " ", ",").
            IF LENGTH(c-te1) > 120 THEN
                c-te1 = SUBSTR(c-te1, 1, 120).
            /*
               TE1
            */
            PUT STREAM arqcatnf UNFORMATTED  
                      "TE1"
                      c-te1
                      FILL(" ", 120 - LENGTH(c-te1))
                      FILL(" ", 5) /* filler */
                      SKIP.
    
            nlinhas = nlinhas + 1.

        END.*/


    END. /* nota-fiscal */        

    nlinhas = nlinhas + 1.

    /*
        FTP
    */
    PUT STREAM arqcatnf UNFORMATTED
            "FTP"
            STRING(ctrl, "99999")
            STRING(nlinhas, "999999999")
            FILL("0", 17)
            "D"
            FILL(" ", 93) /* filler */
            SKIP.

    OUTPUT STREAM arqcatnf CLOSE.

END PROCEDURE.


PROCEDURE geraEDI17:
    DEF INPUT PARAM cod-estabel LIKE nota-fiscal.cod-estabel NO-UNDO.
    DEF INPUT PARAM nr-nota     LIKE nota-fiscal.nr-nota-fis NO-UNDO.
    DEF INPUT PARAM serie       LIKE nota-fiscal.serie       NO-UNDO.

    DEF VAR cli LIKE emitente.cod-emitente NO-UNDO.
    DEF VAR ccs AS CHAR NO-UNDO.

    /*DEF VAR c-te1 AS CHAR NO-UNDO.*/
    DEF VAR c-nfae8  AS CHAR NO-UNDO.
    DEF VAR c-serae8 AS CHAR NO-UNDO.
    DEF VAR d-dtae8  AS DATE NO-UNDO.

    DEF VAR totnf  AS DEC NO-UNDO.
    DEF VAR totipi AS DEC NO-UNDO.
    DEF VAR itipi  AS DEC NO-UNDO.

    DEF VAR cont    AS DECIMAL NO-UNDO.
    DEF VAR qty_it  AS DECIMAL NO-UNDO.
    DEF VAR dtvenc  AS CHAR FORMAT "x(6)" INITIAL "AAMMDD" NO-UNDO.
    DEF VAR dtduedt AS CHAR FORMAT "x(6)" INITIAL "AAMMDD" NO-UNDO.
    DEF VAR especie AS CHAR FORMAT "x(2)" INITIAL "02" NO-UNDO.
    DEF VAR fabrica AS CHAR FORMAT "x(3)" INITIAL "000" NO-UNDO.
    /*? P=Producao R=Reposicao T=Triangulacao E=Exportacao X=Outros A=Amostra F=Ferramentas e Solucoes */
    DEF VAR tipo    AS CHAR FORMAT "x(1)" INITIAL "P" NO-UNDO.
    /* */
    DEF VAR it-cliente   AS CHAR NO-UNDO.
    DEF VAR ponbr   AS CHAR NO-UNDO.
    DEF VAR nlinhas AS INTEGER NO-UNDO.
    DEF VAR ctrl    AS INTEGER NO-UNDO.
    DEF VAR arq     AS CHAR NO-UNDO.

    DEF VAR rev   AS CHAR NO-UNDO.
    DEF VAR cgc   AS CHAR NO-UNDO.
    DEF VAR pasta AS CHAR NO-UNDO.

    DEF VAR nome-abrev AS CHAR NO-UNDO.

    DEF VAR tot_icm  AS DECIMAL NO-UNDO.
    DEF VAR tot_bicm AS DECIMAL NO-UNDO.

    DEF VAR c-serie AS CHAR NO-UNDO.
    DEF VAR un AS CHAR FORMAT "x(2)" NO-UNDO.

    ASSIGN c-serie = serie.
    IF c-serie = "" THEN c-serie = "0".

    FIND FIRST nota-fiscal NO-LOCK WHERE nota-fiscal.cod-estabel = cod-estabel 
                                     AND nota-fiscal.nr-nota-fis = nr-nota  
                                     AND nota-fiscal.serie       = serie NO-ERROR.
    IF NOT AVAIL nota-fiscal THEN
        LEAVE.
    ELSE
        cli = nota-fiscal.cod-emitente.

    IF cli = 10000158 THEN 
        cli = 10000159.

    FIND FIRST emitente NO-LOCK WHERE emitente.cod-emitente = cli NO-ERROR.
    IF NOT AVAIL emitente THEN LEAVE.

    ASSIGN cgc        = emitente.cgc
           nome-abrev = SUBSTR(emitente.nome-emit,1,25).

    ccs = "Q9994G0 ".
    IF cli = 10002155 THEN ccs = "00034857".
    IF cli = 10009063 THEN ccs = "00008499".
    IF cli = 10006602 THEN ccs = "Q9994G6 ".

    IF cli = 10000124 OR cli = 10005825 OR cli = 10006602 THEN
        pasta = "\\192.0.0.149\ediccs$\AV\".
    ELSE IF cli = 10002395 OR cli = 10005276 OR cli = 10005705 OR cli = 10005725 THEN
            pasta = "\\192.0.0.149\ediccs$\sintel\snd\".
    ELSE
        pasta = "\\192.0.0.149\ediccs$\EDI\".

    arq = pasta + cgc + "_RND00417_00964350000178_" +
          STRING(YEAR(TODAY)) + STRING(MONTH(TODAY),"99") + STRING(DAY(TODAY),"99") +
          REPLACE(STRING(TIME,"hh:mm:ss"),":","") + ".TXT".

    FILE-INFO:FILE-NAME = arq.

    DO WHILE FILE-INFO:PATHNAME <> ?:

        arq = pasta + cgc + "_RND00417_00964350000178_" +
              STRING(YEAR(TODAY)) + STRING(MONTH(TODAY),"99") + STRING(DAY(TODAY),"99") +
              STRING(INTEGER(REPLACE(STRING(TIME,"hh:mm:ss"),":","")) + 1, "999999")
              + ".TXT".
        
        FILE-INFO:FILE-NAME = arq.

    END.
    
    OUTPUT STREAM arqcatnf TO VALUE(arq).
    
    nlinhas = 0.

    FOR EACH nota-fiscal NO-LOCK WHERE nota-fiscal.cod-estabel = cod-estabel 
                                   AND nota-fiscal.nr-nota-fis = nr-nota  
                                   AND nota-fiscal.serie       = serie :
        
        /*IF zn_type = "D" THEN NEXT. 
        IF zn_cancel_date <> ? THEN NEXT.*/
    
        ctrl = 0.
        RUN contaEnvios(nota-fiscal.dt-emis-nota, OUTPUT ctrl).

        /*
           ITP
        */
        PUT STREAM arqcatnf UNFORMATTED
                "ITP" 
                "00417"
                STRING(ctrl, "99999")
                SUBSTRING(STRING(YEAR(TODAY)),3,2) STRING(MONTH(TODAY),"99") STRING(DAY(TODAY),"99") 
                REPLACE(STRING(TIME,"hh:mm:ss"),":","")
                "00964350000178" + cgc + ccs + "        CCS TENOLOGIA INDUSTRIAL " nome-abrev + FILL(" ", 25 - LENGTH(nome-abrev))
                FILL(" ",9)
                SKIP .
    
        nlinhas = nlinhas + 1.
    
        qty_it = 0.
        RUN contaItems(INPUT nota-fiscal.cod-estabel, 
                       INPUT nota-fiscal.nr-nota-fis, 
                       INPUT nota-fiscal.serie, 
                       OUTPUT qty_it,
                       OUTPUT tot_icm,
                       OUTPUT tot_bicm,
                       OUTPUT dtduedt
                      ).
    
        FIND FIRST fat-duplic WHERE fat-duplic.cod-estabel = nota-fiscal.cod-estabel 
                                AND fat-duplic.serie       = nota-fiscal.serie
                                AND fat-duplic.nr-fatura   = nota-fiscal.nr-nota-fis NO-LOCK NO-ERROR.
        IF NOT AVAIL fat-duplic THEN
            dtvenc = "000000".
        ELSE
            dtvenc = SUBSTRING(STRING(YEAR(fat-duplic.dt-venciment)),3,2) + STRING(MONTH(fat-duplic.dt-venciment),"99") + STRING(DAY(fat-duplic.dt-venciment),"99").

         /*IF dtduedt = "000000" THEN*/
            dtduedt = SUBSTRING(STRING(YEAR(nota-fiscal.dt-emis-nota + 1)),3,2) + STRING(MONTH(nota-fiscal.dt-emis-nota + 1), "99") + STRING(DAY(nota-fiscal.dt-emis-nota + 1), "99").
        
        IF SUBSTRING(nota-fiscal.nr-pedcli,4,1) = "P" THEN
            fabrica = "028".
        ELSE IF SUBSTRING(nota-fiscal.nr-pedcli,4,1) = "T" THEN
            fabrica = "081".
        ELSE IF SUBSTRING(nota-fiscal.nr-pedcli,4,1) = "C" THEN
            fabrica = "010".
        ELSE
            fabrica = "000".

        IF nota-fiscal.ind-tip-nota = 3 OR nota-fiscal.ind-tip-nota = 4 THEN 
            ASSIGN tipo = "X"
                   especie = "04".
        ELSE
            ASSIGN tipo = "P"
                   especie = "02".
    
        totipi = 0.
        FIND FIRST natur-oper OF nota-fiscal NO-LOCK NO-ERROR.
        IF AVAIL natur-oper AND natur-oper.cd-trib-ipi = 1 THEN
            totipi = nota-fiscal.vl-tot-ipi.

        /*
           AE1
        */
        PUT STREAM arqcatnf UNFORMATTED  
                  "AE1" STRING(INT(nota-fiscal.nr-nota-fis), "999999") c-serie FORMAT "x(4)"
                  SUBSTRING(STRING(YEAR(nota-fiscal.dt-emis-nota)),3,2) + STRING(MONTH(nota-fiscal.dt-emis-nota), "99") + STRING(DAY(nota-fiscal.dt-emis-nota), "99")
                  STRING(qty_it, "999")
                  REPLACE(STRING(nota-fiscal.vl-tot-nota, "999999999999999.99"), ",","")
                  "0"
                  STRING(INT(REPLACE(natur-oper.cod-cfop,",","")) , "99999")
                  REPLACE(STRING(tot_icm, "999999999999999.99"), ",","")
                  dtvenc
                  especie /* vede anexo ? */
                  REPLACE(STRING(totipi,  "999999999999999.99"), ",","")
                  fabrica
                  SUBSTRING(STRING(YEAR(nota-fiscal.dt-emis-nota + 1)),3,2) + STRING(MONTH(nota-fiscal.dt-emis-nota + 1), "99") + STRING(DAY(nota-fiscal.dt-emis-nota + 1), "99")
                  "0000"
                  FILL(" ",15)
                  SUBSTRING(STRING(YEAR(nota-fiscal.dt-emis-nota)),3,2) + STRING(MONTH(nota-fiscal.dt-emis-nota), "99") + STRING(DAY(nota-fiscal.dt-emis-nota), "99")  
                  "0000"
                  FILL(" ",3) /* filler */ 
                  SKIP.
    
        nlinhas = nlinhas + 1.
    
        /*
           NF2
        */
        PUT STREAM arqcatnf UNFORMATTED  
                  "NF2" 
                  FILL("0", 48) 
                  REPLACE(STRING(tot_bicm, "9999999999.99"), ",","")
                  FILL("0", 12)
                  FILL(" ", 53) 
                  SKIP.
    
        nlinhas = nlinhas + 1.
    
        /*
           NF3
        */                  
        PUT STREAM arqcatnf UNFORMATTED  
                 "NF3" 
                 dtvenc 
                 FILL("0", 54) 
                 FILL(" ", 65)
                 SKIP.
    
        nlinhas = nlinhas + 1.
    
        cont = 0.
    
        FIND FIRST edi-ccs NO-LOCK WHERE edi-ccs.nr-nota-fis  = nota-fiscal.nr-nota-fis
                                     AND edi-ccs.serie        = nota-fiscal.serie NO-ERROR.
        IF NOT AVAIL edi-ccs THEN DO:
            CREATE edi-ccs.
            ASSIGN edi-ccs.nr-nota-fis  = nota-fiscal.nr-nota-fis
                   edi-ccs.serie        = nota-fiscal.serie
                   edi-ccs.dt-emis-nota = nota-fiscal.dt-emis-nota
                   edi-ccs.hora         = STRING(TIME, "HH:MM").
        END.

        /*
         *************************** ZND DETALHE ****************************
        */
        FOR EACH it-nota-fisc NO-LOCK WHERE it-nota-fisc.cod-estabel = nota-fiscal.cod-estabel
                                        AND it-nota-fisc.nr-nota-fis = nota-fiscal.nr-nota-fis
                                        AND it-nota-fisc.serie       = nota-fiscal.serie :
        
            IF it-nota-fisc.it-codigo = "" THEN NEXT.
            FIND FIRST ITEM NO-LOCK WHERE ITEM.it-codigo = it-nota-fisc.it-codigo NO-ERROR.
            IF NOT AVAIL ITEM THEN NEXT.

            
            cont = cont + 1.            

            IF ITEM.codigo-refer = "" THEN DO:

                ASSIGN it-cliente = "S/CODIGO"
                       rev   = "".

            END. ELSE DO:
                it-cliente = REPLACE(REPLACE( ENTRY(1, ITEM.codigo-refer, "#") ,"-",""), " ", "") NO-ERROR.
                rev   = "".
                rev   = ENTRY(2, ITEM.codigo-refer, "#" ) NO-ERROR.
            END.

            IF it-nota-fisc.nr-pedcli = "" THEN  
                ponbr = "".
            ELSE
                ponbr = SUBSTRING(it-nota-fisc.nr-pedcli,1,9).

            IF cli = 10002155 AND NOT ponbr BEGINS "#" THEN ponbr = "5500008945".
            ASSIGN ponbr = REPLACE(ponbr, "#","")
                   un = SUBSTRING(it-nota-fisc.un[1], 1, 2).

            /*
               AE2 
            */
            PUT STREAM arqcatnf UNFORMATTED  
                     "AE2" 
                     STRING(cont, "999")
                     UPPER(ponbr + FILL(" ", 12 - LENGTH(ponbr)))
                     UPPER(it-cliente + FILL(" ", 30 - LENGTH(it-cliente)))
                     STRING(it-nota-fisc.qt-faturada[1], "999999999")
                     un FILL(" ", 2 - LENGTH(un))
                     STRING( INT(REPLACE(it-nota-fisc.class-fiscal, ",","")), "9999999999" ).

            IF totipi = 0 THEN 
                 PUT STREAM arqcatnf UNFORMATTED "0000".
            ELSE
                 PUT STREAM arqcatnf UNFORMATTED REPLACE(STRING(it-nota-fisc.aliquota-ipi, "99.99"), ",","").

            PUT STREAM arqcatnf UNFORMATTED  
                     /*REPLACE(STRING(ABS(it-nota-fisc.vl-preuni), "9999999.99999"), ",","")*/
                    REPLACE(STRING(ABS(it-nota-fisc.vl-preuni), "9999999.99"), ",","") + "000"
                     FILL("0", 9) + FILL(" ", 2)
                     FILL("0", 9) + "EA"
                     tipo 
                     REPLACE(STRING(ABS(it-nota-fisc.val-pct-desconto-total), "99.99"), ",","")
                     REPLACE(STRING(ABS(it-nota-fisc.vl-desconto), "999999999.99"), ",","")
                     FILL(" ", 4 - LENGTH( SUBSTRING( rev ,1,4) ) )  SUBSTRING( rev , 1, 4) 
                     FILL(" ", 1)
                     SKIP .
    
            nlinhas = nlinhas + 1.

            IF totipi = 0 THEN
                itipi = 0.
            ELSE
                itipi = it-nota-fisc.vl-ipi-it.
        
            /*
                AE4
            */
            PUT STREAM arqcatnf UNFORMATTED  
                     "AE4"
                     REPLACE(STRING(it-nota-fisc.aliquota-icm, "99.99"), ",","")
                     REPLACE(STRING(it-nota-fisc.vl-bicms-it, "999999999999999.99"), ",","")
                     REPLACE(STRING(it-nota-fisc.vl-icms-it , "999999999999999.99"), ",","")
                     REPLACE(STRING(itipi  , "999999999999999.99"), ",","")
                     "00"
                     FILL(" ", 30) "000000" 
                     FILL(" ", 13) FILL(" ", 5)
                     " "
                     REPLACE(STRING(it-nota-fisc.vl-tot-item, "9999999999.99"), ",","")
                     " "
                     /*FILL(" ", 10)*/
                     SKIP.
    
            nlinhas = nlinhas + 1.
           /*
            /*
                AE6
            */
            PUT STREAM arqcatnf UNFORMATTED  
                     "AE6"
                     "BR"
                     FILL(" ", 123)
                     SKIP.
    
            nlinhas = nlinhas + 1.*/

            FIND FIRST natur-oper OF it-nota-fisc NO-LOCK NO-ERROR.

            /*
               AE7
            */
            PUT STREAM arqcatnf UNFORMATTED  
                      "AE7"
                      FILL("0", 12)
                      STRING(INT(REPLACE(natur-oper.cod-cfop,",","")), "99999") 
                      FILL("0", 48)
                      FILL(" ", 5) /* filler */
                      FILL("0", 12)
                      FILL(" ", 43) /* filler */
                      SKIP.
    
            nlinhas = nlinhas + 1.

            IF nota-fiscal.ind-tip-nota = 3 OR nota-fiscal.ind-tip-nota = 4 THEN DO:

                ASSIGN c-nfae8  = it-nota-fisc.nr-docum
                       c-serae8 = it-nota-fisc.serie-docum.

                FIND FIRST doc-fiscal NO-LOCK WHERE doc-fiscal.cod-estabel  = nota-fiscal.cod-estabel
                                                AND doc-fiscal.serie        = c-serae8
                                                AND doc-fiscal.nr-doc-fis   = c-nfae8
                                                AND doc-fiscal.cod-emitente = nota-fiscal.cod-emitente
                                                AND doc-fiscal.nat-oper     = it-nota-fisc.nat-docum NO-ERROR.
                IF AVAIL doc-fiscal THEN
                    ASSIGN d-dtae8 = doc-fiscal.dt-emis-doc.

                IF TRIM(c-nfae8) <> "" THEN DO:

                    ASSIGN c-nfae8 = STRING(INT(c-nfae8),"999999").
                
                    /*
                       AE8
                    */
                    PUT STREAM arqcatnf UNFORMATTED  
                              "AE8"
                              c-nfae8
                              c-serae8
                              SUBSTRING(STRING(YEAR(d-dtae8)),3,2) + STRING(MONTH(d-dtae8), "99") + STRING(DAY(d-dtae8), "99")  
                              "000"
                              FILL(" ", 16)
                              FILL(" ", 17)
                              FILL(" ", 10)
                              FILL(" ", 63) /* filler */
                              SKIP.
            
                    nlinhas = nlinhas + 1.
                END.
            END.
        END.

        /*IF nota-fiscal.ind-tip-nota = 3 OR nota-fiscal.ind-tip-nota = 4 THEN DO:

            c-te1 = fn-free-accent(natur-oper.denominacao) + " Referente Ös NFS: " + REPLACE(fi-achaNF(nota-fiscal.observ-nota), " ", ",").
            IF LENGTH(c-te1) > 120 THEN
                c-te1 = SUBSTR(c-te1, 1, 120).
            /*
               TE1
            */
            PUT STREAM arqcatnf UNFORMATTED  
                      "TE1"
                      c-te1
                      FILL(" ", 120 - LENGTH(c-te1))
                      FILL(" ", 5) /* filler */
                      SKIP.
    
            nlinhas = nlinhas + 1.

        END.*/


    END. /* nota-fiscal */        

    nlinhas = nlinhas + 1.

    /*
        FTP
    */
    PUT STREAM arqcatnf UNFORMATTED
            "FTP"
            STRING(ctrl, "99999")
            STRING(nlinhas, "999999999")
            FILL("0", 17)
            "D"
            FILL(" ", 93) /* filler */
            SKIP.

    OUTPUT STREAM arqcatnf CLOSE.

END PROCEDURE.

PROCEDURE geraEDI15:
    DEF INPUT PARAM cod-estabel LIKE nota-fiscal.cod-estabel NO-UNDO.
    DEF INPUT PARAM nr-nota     LIKE nota-fiscal.nr-nota-fis NO-UNDO.
    DEF INPUT PARAM serie       LIKE nota-fiscal.serie NO-UNDO.

    DEF VAR cli LIKE emitente.cod-emitente NO-UNDO.

    DEF VAR ccs   AS CHAR NO-UNDO.
    /*DEF VAR c-te1 AS CHAR NO-UNDO.*/
    DEF VAR c-nfae8  AS CHAR NO-UNDO.
    DEF VAR c-serae8 AS CHAR NO-UNDO.
    DEF VAR d-dtae8  AS DATE NO-UNDO.

    DEF VAR totnf  AS DEC NO-UNDO.
    DEF VAR totipi AS DEC NO-UNDO.
    DEF VAR itipi  AS DEC NO-UNDO.

    DEF VAR cont    AS DECIMAL NO-UNDO.
    DEF VAR qty_it  AS DECIMAL NO-UNDO.
    DEF VAR dtvenc  AS CHAR FORMAT "x(6)" INITIAL "AAMMDD" NO-UNDO.
    DEF VAR especie AS CHAR FORMAT "x(2)" INITIAL "02" NO-UNDO.
    DEF VAR fabrica AS CHAR FORMAT "x(3)" INITIAL "000" NO-UNDO.
    /*? P=Producao R=Reposicao T=Triangulacao E=Exportacao X=Outros A=Amostra F=Ferramentas e Solucoes */
    DEF VAR tipo    AS CHAR FORMAT "x(1)" INITIAL "P" NO-UNDO.
    /* */
    DEF VAR it-cliente   AS CHAR NO-UNDO.
    DEF VAR ponbr   AS CHAR NO-UNDO.
    DEF VAR nlinhas AS INTEGER NO-UNDO.
    DEF VAR ctrl    AS INTEGER NO-UNDO.
    DEF VAR arq     AS CHAR NO-UNDO.

    DEF VAR rev   AS CHAR NO-UNDO.
    DEF VAR cgc   AS CHAR NO-UNDO.
    DEF VAR pasta AS CHAR NO-UNDO.

    DEF VAR un AS CHAR NO-UNDO FORMAT "x(2)".

    DEF VAR tot_icm  AS DECIMAL NO-UNDO.
    DEF VAR tot_bicm AS DECIMAL NO-UNDO.
    DEF VAR nome-abrev AS CHAR NO-UNDO.

    DEF VAR c-serie AS CHAR NO-UNDO.

    ASSIGN c-serie = serie.
    IF c-serie = "" THEN c-serie = "0".

    FIND FIRST nota-fiscal NO-LOCK WHERE nota-fiscal.cod-estabel = cod-estabel 
                                     AND nota-fiscal.nr-nota-fis = nr-nota  
                                     AND nota-fiscal.serie       = serie NO-ERROR.
    IF NOT AVAIL nota-fiscal THEN
        LEAVE.
    ELSE
        cli = nota-fiscal.cod-emitente.

    IF cli = 10000158 THEN 
        cli = 10000159.

    ccs = "Q9994G0 ".
    IF cli = 10002155 THEN ccs = "00034857".
    IF cli = 10009063 THEN ccs = "00008499".
    IF cli = 10006602 THEN ccs = "Q9994G6 ".
                                          

    FIND FIRST emitente NO-LOCK WHERE emitente.cod-emitente = cli NO-ERROR.
    IF NOT AVAIL emitente THEN LEAVE.

    ASSIGN cgc = emitente.cgc
           nome-abrev = SUBSTR(emitente.nome-emit,1,25).

    IF cli = 10000124 OR cli = 10005825 OR cli = 10006602 THEN
        pasta = "\\192.0.0.149\ediccs$\AV\".
    ELSE IF cli = 10002395 OR cli = 10005276 OR cli = 10005705 OR cli = 10005725 THEN
            pasta = "\\192.0.0.149\ediccs$\sintel\snd\".
    ELSE
        pasta = "\\192.0.0.149\ediccs$\EDI\".

    arq = pasta + cgc + "_RND00415_00964350000178_" +
          STRING(YEAR(TODAY)) + STRING(MONTH(TODAY),"99") + STRING(DAY(TODAY),"99") +
          REPLACE(STRING(TIME,"hh:mm:ss"),":","") + ".TXT".

    FILE-INFO:FILE-NAME = arq.

    DO WHILE FILE-INFO:PATHNAME <> ?:

        arq = pasta + cgc + "_RND00415_00964350000178_" +
              STRING(YEAR(TODAY)) + STRING(MONTH(TODAY),"99") + STRING(DAY(TODAY),"99") +
              STRING(INTEGER(REPLACE(STRING(TIME,"hh:mm:ss"),":","")) + 1, "999999")
              + ".TXT".
        
        FILE-INFO:FILE-NAME = arq.

    END.
    
    OUTPUT STREAM arqcatnf TO VALUE(arq).
    
    nlinhas = 0.

    FOR EACH nota-fiscal NO-LOCK WHERE nota-fiscal.cod-estabel = cod-estabel 
                                   AND nota-fiscal.nr-nota-fis = nr-nota  
                                   AND nota-fiscal.serie       = serie :
        
        /*IF zn_type = "D" THEN NEXT. 
        IF zn_cancel_date <> ? THEN NEXT.*/
    
        ctrl = 0.
        RUN contaEnvios(nota-fiscal.dt-emis-nota, OUTPUT ctrl).

        /*
           ITP
        */
        PUT STREAM arqcatnf UNFORMATTED
                "ITP" 
                "00415"
                STRING(ctrl, "99999")
                SUBSTRING(STRING(YEAR(TODAY)),3,2) STRING(MONTH(TODAY),"99") STRING(DAY(TODAY),"99") 
                REPLACE(STRING(TIME,"hh:mm:ss"),":","")
                "00964350000178" + cgc + ccs + "        CCS TENOLOGIA INDUSTRIAL " nome-abrev + FILL(" ", 25 - LENGTH(nome-abrev))                                          
                FILL(" ",9)
                SKIP /* SKIP*/ .
    
        nlinhas = nlinhas + 1.
    
    
        qty_it = 0.
        RUN contaItems(INPUT nota-fiscal.cod-estabel, 
                       INPUT nota-fiscal.nr-nota-fis, 
                       INPUT nota-fiscal.serie, 
                       OUTPUT qty_it,
                       OUTPUT tot_icm,
                       OUTPUT tot_bicm,
                       OUTPUT dtvenc
                      ).
                   
        FIND FIRST fat-duplic WHERE fat-duplic.cod-estabel = nota-fiscal.cod-estabel 
                                AND fat-duplic.serie       = nota-fiscal.serie  
                                AND fat-duplic.nr-fatura   = nota-fiscal.nr-nota-fis NO-LOCK NO-ERROR.
        IF NOT AVAIL fat-duplic THEN
            dtvenc = "000000".
        ELSE DO:
            IF nota-fiscal.cod-emitente = 10002155 OR nota-fiscal.cod-emitente = 10009063 THEN
                dtvenc = SUBSTRING(STRING(YEAR(fat-duplic.dt-venciment)),3,2) + STRING(MONTH(fat-duplic.dt-venciment),"99") +  STRING(DAY(fat-duplic.dt-venciment),"99").
            ELSE
                dtvenc = STRING(DAY(fat-duplic.dt-venciment),"99") + STRING(MONTH(fat-duplic.dt-venciment),"99") + SUBSTRING(STRING(YEAR(fat-duplic.dt-venciment)),3,2) .
        END.
    
        IF SUBSTRING(nota-fiscal.nr-pedcli,4,1) = "P" THEN
            fabrica = "028".
        ELSE IF SUBSTRING(nota-fiscal.nr-pedcli,4,1) = "T" THEN
            fabrica = "081".
        ELSE IF SUBSTRING(nota-fiscal.nr-pedcli,4,1) = "C" THEN
            fabrica = "010".
        ELSE
            fabrica = "000".
    
        IF nota-fiscal.ind-tip-nota = 3 OR nota-fiscal.ind-tip-nota = 4 THEN 
            ASSIGN tipo = "X"
                   especie = "04".
        ELSE
            ASSIGN tipo = "P"
                   especie = "02".

        totipi = 0.
        FIND FIRST natur-oper OF nota-fiscal NO-LOCK NO-ERROR.
        IF AVAIL natur-oper AND natur-oper.cd-trib-ipi = 1 THEN
            totipi = nota-fiscal.vl-tot-ipi.

        /*
           AE1
        */
        PUT STREAM arqcatnf UNFORMATTED  
                  "AE1" STRING(INT(nota-fiscal.nr-nota-fis), "999999") c-serie FORMAT "x(4)"
                  SUBSTRING(STRING(YEAR(nota-fiscal.dt-emis-nota)),3,2) + STRING(MONTH(nota-fiscal.dt-emis-nota), "99") + STRING(DAY(nota-fiscal.dt-emis-nota), "99")
                  STRING(qty_it, "999")
                  REPLACE(STRING(nota-fiscal.vl-tot-nota, "999999999999999.99"), ",","")
                  "4"
                  STRING(INT(REPLACE(natur-oper.cod-cfop,",","")) , "99999")
                  REPLACE(STRING(tot_icm, "999999999999999.99"), ",","")
                  dtvenc
                  especie /* vede anexo ? */
                  REPLACE(STRING(totipi,  "999999999999999.99"), ",","")
                  fabrica
                  SUBSTRING(STRING(YEAR(nota-fiscal.dt-emis-nota + 1)),3,2) + STRING(MONTH(nota-fiscal.dt-emis-nota + 1), "99") + STRING(DAY(nota-fiscal.dt-emis-nota + 1), "99")
                  "0000"
                  FILL(" ",15)
                  SUBSTRING(STRING(YEAR(nota-fiscal.dt-emis-nota)),3,2) + STRING(MONTH(nota-fiscal.dt-emis-nota), "99") + STRING(DAY(nota-fiscal.dt-emis-nota), "99")
                  "0000"
                  FILL(" ",3) /* filler */ 
                  SKIP /* SKIP*/.
    
        nlinhas = nlinhas + 1.
    
        /*
           NF2
        */
        PUT STREAM arqcatnf UNFORMATTED  
                  "NF2" 
                  FILL("0", 48) 
                  REPLACE(STRING(tot_bicm, "9999999999.99"), ",","")
                  FILL(" ", 65) 
                  SKIP /* SKIP*/.
    
        nlinhas = nlinhas + 1.
    
        /*
           NF3
        */
        PUT STREAM arqcatnf UNFORMATTED  
                 "NF3" 
                 dtvenc 
                 FILL("0", 54) 
                 FILL(" ", 65)
                 SKIP /* SKIP*/.
    
        nlinhas = nlinhas + 1.
    
        cont = 0.

        FIND FIRST edi-ccs NO-LOCK WHERE edi-ccs.nr-nota-fis  = nota-fiscal.nr-nota-fis
                                     AND edi-ccs.serie        = nota-fiscal.serie NO-ERROR.
        IF NOT AVAIL edi-ccs THEN DO:
            CREATE edi-ccs.
            ASSIGN edi-ccs.nr-nota-fis  = nota-fiscal.nr-nota-fis
                   edi-ccs.serie        = nota-fiscal.serie
                   edi-ccs.dt-emis-nota = nota-fiscal.dt-emis-nota
                   edi-ccs.hora         = STRING(TIME, "HH:MM").
        END.

    
        /*
         *************************** ZND DETALHE ****************************
        */
       FOR EACH it-nota-fisc NO-LOCK WHERE it-nota-fisc.cod-estabel = nota-fiscal.cod-estabel
                                        AND it-nota-fisc.nr-nota-fis = nota-fiscal.nr-nota-fis
                                        AND it-nota-fisc.serie       = nota-fiscal.serie :
    
            IF it-nota-fisc.it-codigo = "" THEN NEXT.    
            FIND FIRST ITEM OF it-nota-fisc NO-LOCK NO-ERROR.
            IF NOT AVAIL ITEM THEN NEXT.

            cont = cont + 1.

            IF ITEM.codigo-refer = "" THEN DO:
                ASSIGN it-cliente = "S/CODIGO"
                       rev   = "".
            END. ELSE DO:
                it-cliente = TRIM(REPLACE(REPLACE( ENTRY(1, ITEM.codigo-refer, "#") ,"-","")," ","")) NO-ERROR.
                it-cliente = REPLACE( it-cliente ,".","").
                IF cli <> 10002155 AND cli <> 10009063 THEN
                    rev   = ENTRY(2, ITEM.codigo-refer, "#" ) NO-ERROR.
            END.


            ponbr = "".
            IF cli = 10002395 THEN DO:
                FOR FIRST ped-item NO-LOCK WHERE ped-item.nr-pedcli = it-nota-fisc.nr-pedcli ,
                    FIRST ped-ent OF ped-item NO-LOCK :
                    ponbr = TRIM(REPLACE(ped-ent.observacao, "OC", "")).
                END.
            END. ELSE DO:
                ponbr = SUBSTRING(it-nota-fisc.nr-pedcli,1,9).
            END.

            IF cli = 10002155 AND NOT ponbr BEGINS "#" THEN ponbr = "5500008945".
            ASSIGN ponbr = REPLACE(ponbr, "#","")
                   un = SUBSTRING(it-nota-fisc.un[1],1,2).

            /*un = "EA".*/
            IF cli = 10000159 THEN
                un = "01".
    
            /*
               AE2 
            */
            PUT STREAM arqcatnf UNFORMATTED  
                     "AE2" 
                     STRING(cont, "999")
                     UPPER(ponbr + FILL(" ", 12 - LENGTH(ponbr)))
                     UPPER(it-cliente + FILL(" ", 30 - LENGTH(it-cliente)))
                     REPLACE(STRING(it-nota-fisc.qt-faturada[1], "99999.9999"), ",","")
                     un FILL(" ", 2 - LENGTH(un))
                     STRING( INT(REPLACE(it-nota-fisc.class-fiscal, ",","")), "9999999999" ).

            IF totipi = 0 THEN 
                 PUT STREAM arqcatnf UNFORMATTED "0000".
            ELSE
                 PUT STREAM arqcatnf UNFORMATTED REPLACE(STRING(it-nota-fisc.aliquota-ipi, "99.99"), ",","").

            PUT STREAM arqcatnf UNFORMATTED  
                     REPLACE(STRING(TRUNCATE(ABS(it-nota-fisc.vl-preuni),4), "9999999.99999"), ",","")
                     REPLACE(STRING(it-nota-fisc.qt-faturada[1], "99999.9999"), ",","")
                     un FILL(" ", 2 - LENGTH(un))
                     FILL("0", 9) + FILL(" ", 2)
                     tipo 
                     REPLACE(STRING(ABS(it-nota-fisc.val-pct-desconto-total), "99.99"), ",","")
                     REPLACE(STRING(ABS(it-nota-fisc.vl-desconto), "999999999.99"), ",","")
                     FILL(" ", 4 - LENGTH( SUBSTRING( rev ,1,4) ) )  SUBSTRING( rev , 1, 4) 
                     FILL(" ", 1)
                     SKIP /* SKIP*/ .
    
            nlinhas = nlinhas + 1.

            IF totipi = 0 THEN
                itipi = 0.
            ELSE
                itipi = it-nota-fisc.vl-ipi-it.
        
            /*
                AE4
            */
            PUT STREAM arqcatnf UNFORMATTED  
                     "AE4"
                     REPLACE(STRING(it-nota-fisc.aliquota-icm, "99.99"), ",","")
                     REPLACE(STRING(it-nota-fisc.vl-bicms-it, "999999999999999.99"), ",","")
                     REPLACE(STRING(it-nota-fisc.vl-icms-it , "999999999999999.99"), ",","")
                     REPLACE(STRING(itipi  , "999999999999999.99"), ",","")
                     "00"
                     FILL(" ", 30) 
                     "000000" 
                     FILL(" ", 13) FILL(" ", 5)
                     " "
                     REPLACE(STRING(it-nota-fisc.vl-tot-item, "9999999999.99"), ",","")
                     " "
                     SKIP /* SKIP*/.
    
            nlinhas = nlinhas + 1.
           
            FIND FIRST natur-oper OF it-nota-fisc NO-LOCK NO-ERROR.

            /*
               AE7
            */
            PUT STREAM arqcatnf UNFORMATTED  
                      "AE7"
                      FILL("0", 12)
                      STRING(INT(REPLACE(natur-oper.cod-cfop,",","")), "99999") 
                      FILL("0", 48)
                      FILL(" ", 5) /* filler */
                      FILL(" ", 54) /* filler */
                      SKIP /* SKIP*/.
    
            nlinhas = nlinhas + 1.

            /*
               AE8
            */
            IF nota-fiscal.ind-tip-nota = 3 OR nota-fiscal.ind-tip-nota = 4 THEN DO:

                ASSIGN c-nfae8  = it-nota-fisc.nr-docum
                       c-serae8 = it-nota-fisc.serie-docum.

                FIND FIRST doc-fiscal NO-LOCK WHERE doc-fiscal.cod-estabel  = nota-fiscal.cod-estabel
                                                AND doc-fiscal.serie        = c-serae8
                                                AND doc-fiscal.nr-doc-fis   = c-nfae8
                                                AND doc-fiscal.cod-emitente = nota-fiscal.cod-emitente
                                                AND doc-fiscal.nat-oper     = it-nota-fisc.nat-docum NO-ERROR.
                IF AVAIL doc-fiscal THEN
                    ASSIGN d-dtae8 = doc-fiscal.dt-emis-doc.

                IF TRIM(c-nfae8) <> "" THEN DO:
                    ASSIGN c-nfae8 = STRING(INT(c-nfae8),"999999").                

                    PUT STREAM arqcatnf UNFORMATTED  
                              "AE8"
                              c-nfae8
                              c-serae8
                              SUBSTRING(STRING(YEAR(d-dtae8)),3,2) + STRING(MONTH(d-dtae8), "99") + STRING(DAY(d-dtae8), "99")  
                              "000"
                              FILL(" ", 16)
                              FILL(" ", 17)
                              FILL(" ", 10)
                              FILL(" ", 63) /* filler */
                              SKIP.
            
                    nlinhas = nlinhas + 1.
                END.
            END.
    
        END.

        /*IF nota-fiscal.ind-tip-nota = 3 OR nota-fiscal.ind-tip-nota = 4 THEN DO:

            c-te1 = fn-free-accent(natur-oper.denominacao) + " Referente Ös NFS: " + REPLACE(fi-achaNF(nota-fiscal.observ-nota), " ", ",").
            IF LENGTH(c-te1) > 120 THEN
                c-te1 = SUBSTR(c-te1, 1, 120).
            /*
               TE1
            */
            PUT STREAM arqcatnf UNFORMATTED  
                      "TE1"
                      c-te1
                      FILL(" ", 120 - LENGTH(c-te1))
                      FILL(" ", 5) /* filler */
                      SKIP.
    
            nlinhas = nlinhas + 1.

        END.*/

    END. /* nota-fiscal */        


    nlinhas = nlinhas + 1.

    /*
        FTP
    */
    PUT STREAM arqcatnf UNFORMATTED
            "FTP"
            STRING(ctrl, "99999")
            STRING(nlinhas, "999999999")
            FILL("0", 17)
            "D"
            FILL(" ", 93) /* filler */
            SKIP /* SKIP*/.

    OUTPUT STREAM arqcatnf CLOSE.

END PROCEDURE.


PROCEDURE geraEDI06:
    DEF INPUT PARAM cod-estabel LIKE nota-fiscal.cod-estabel NO-UNDO.
    DEF INPUT PARAM nr-nota     LIKE nota-fiscal.nr-nota-fis NO-UNDO.
    DEF INPUT PARAM serie       LIKE nota-fiscal.serie NO-UNDO.

    DEF VAR cli LIKE emitente.cod-emitente NO-UNDO.

    DEF VAR ccs   AS CHAR NO-UNDO.
    /*DEF VAR c-te1 AS CHAR NO-UNDO.*/
    DEF VAR c-nfae8  AS CHAR NO-UNDO.
    DEF VAR c-serae8 AS CHAR NO-UNDO.
    DEF VAR d-dtae8  AS DATE NO-UNDO.

    DEF VAR totnf  AS DEC NO-UNDO.
    DEF VAR totipi AS DEC NO-UNDO.
    DEF VAR itipi  AS DEC NO-UNDO.

    DEF VAR cont    AS DECIMAL NO-UNDO.
    DEF VAR qty_it  AS DECIMAL NO-UNDO.
    DEF VAR dtvenc  AS CHAR FORMAT "x(6)" INITIAL "AAMMDD" NO-UNDO.
    DEF VAR especie AS CHAR FORMAT "x(2)" INITIAL "02" NO-UNDO.
    DEF VAR fabrica AS CHAR FORMAT "x(3)" INITIAL "000" NO-UNDO.
    /*? P=Producao R=Reposicao T=Triangulacao E=Exportacao X=Outros A=Amostra F=Ferramentas e Solucoes */
    DEF VAR tipo    AS CHAR FORMAT "x(1)" INITIAL "P" NO-UNDO.
    /* */
    DEF VAR it-cliente   AS CHAR NO-UNDO.
    DEF VAR ponbr   AS CHAR NO-UNDO.
    DEF VAR nlinhas AS INTEGER NO-UNDO.
    DEF VAR ctrl    AS INTEGER NO-UNDO.
    DEF VAR arq     AS CHAR NO-UNDO.

    DEF VAR rev   AS CHAR NO-UNDO.
    DEF VAR cgc   AS CHAR NO-UNDO.
    DEF VAR pasta AS CHAR NO-UNDO.

    DEF VAR un AS CHAR NO-UNDO FORMAT "x(2)".

    DEF VAR tot_icm  AS DECIMAL NO-UNDO.
    DEF VAR tot_bicm AS DECIMAL NO-UNDO.
    DEF VAR nome-abrev AS CHAR NO-UNDO.

    DEF VAR c-serie AS CHAR NO-UNDO.

    ASSIGN c-serie = serie.
    IF c-serie = "" THEN c-serie = "0".

    FIND FIRST nota-fiscal NO-LOCK WHERE nota-fiscal.cod-estabel = cod-estabel 
                                     AND nota-fiscal.nr-nota-fis = nr-nota  
                                     AND nota-fiscal.serie       = serie NO-ERROR.
    IF NOT AVAIL nota-fiscal THEN
        LEAVE.
    ELSE
        cli = nota-fiscal.cod-emitente.

    IF cli = 10000158 THEN 
        cli = 10000159.

    ccs = "Q9994G0 ".
    IF cli = 10002155 THEN ccs = "00034857".
    IF cli = 10009063 THEN ccs = "00008499".
    IF cli = 10006602 THEN ccs = "Q9994G6 ".
                                          

    FIND FIRST emitente NO-LOCK WHERE emitente.cod-emitente = cli NO-ERROR.
    IF NOT AVAIL emitente THEN LEAVE.

    ASSIGN cgc = emitente.cgc
           nome-abrev = SUBSTR(emitente.nome-emit,1,25).

    IF cli = 10000124 OR cli = 10005825 OR cli = 10006602 THEN
        pasta = "\\192.0.0.149\ediccs$\AV\".
    ELSE IF cli = 10002395 OR cli = 10005276 OR cli = 10005705 OR cli = 10005725 THEN
            pasta = "\\192.0.0.149\ediccs$\sintel\snd\".
    ELSE
        pasta = "\\192.0.0.149\ediccs$\EDI\".

    arq = pasta + cgc + "_RND00406_00964350000178_" +
          STRING(YEAR(TODAY)) + STRING(MONTH(TODAY),"99") + STRING(DAY(TODAY),"99") +
          REPLACE(STRING(TIME,"hh:mm:ss"),":","") + ".TXT".

    FILE-INFO:FILE-NAME = arq.

    DO WHILE FILE-INFO:PATHNAME <> ?:

        arq = pasta + cgc + "_RND00406_00964350000178_" +
              STRING(YEAR(TODAY)) + STRING(MONTH(TODAY),"99") + STRING(DAY(TODAY),"99") +
              STRING(INTEGER(REPLACE(STRING(TIME,"hh:mm:ss"),":","")) + 1, "999999")
              + ".TXT".
        
        FILE-INFO:FILE-NAME = arq.

    END.
    
    OUTPUT STREAM arqcatnf TO VALUE(arq).
    
    nlinhas = 0.

    FOR EACH nota-fiscal NO-LOCK WHERE nota-fiscal.cod-estabel = cod-estabel 
                                   AND nota-fiscal.nr-nota-fis = nr-nota  
                                   AND nota-fiscal.serie       = serie :
        
        /*IF zn_type = "D" THEN NEXT. 
        IF zn_cancel_date <> ? THEN NEXT.*/
    
        ctrl = 0.
        RUN contaEnvios(nota-fiscal.dt-emis-nota, OUTPUT ctrl).

        /*
           ITP
        */
        PUT STREAM arqcatnf UNFORMATTED
                "ITP" 
                "00406"
                STRING(ctrl, "99999")
                SUBSTRING(STRING(YEAR(TODAY)),3,2) STRING(MONTH(TODAY),"99") STRING(DAY(TODAY),"99") 
                REPLACE(STRING(TIME,"hh:mm:ss"),":","")
                "00964350000178" + cgc + ccs + "        CCS TENOLOGIA INDUSTRIAL " nome-abrev + FILL(" ", 25 - LENGTH(nome-abrev))                                          
                FILL(" ",9)
                SKIP /* SKIP*/ .
    
        nlinhas = nlinhas + 1.
    
    
        qty_it = 0.
        RUN contaItems(INPUT nota-fiscal.cod-estabel, 
                       INPUT nota-fiscal.nr-nota-fis, 
                       INPUT nota-fiscal.serie, 
                       OUTPUT qty_it,
                       OUTPUT tot_icm,
                       OUTPUT tot_bicm,
                       OUTPUT dtvenc
                      ).
                   
        FIND FIRST fat-duplic WHERE fat-duplic.cod-estabel = nota-fiscal.cod-estabel 
                                AND fat-duplic.serie       = nota-fiscal.serie  
                                AND fat-duplic.nr-fatura   = nota-fiscal.nr-nota-fis NO-LOCK NO-ERROR.
        IF NOT AVAIL fat-duplic THEN
            dtvenc = "000000".
        ELSE DO:
            IF nota-fiscal.cod-emitente = 10002155 OR nota-fiscal.cod-emitente = 10009063 THEN
                dtvenc = SUBSTRING(STRING(YEAR(fat-duplic.dt-venciment)),3,2) + STRING(MONTH(fat-duplic.dt-venciment),"99") +  STRING(DAY(fat-duplic.dt-venciment),"99").
            ELSE
                dtvenc = STRING(DAY(fat-duplic.dt-venciment),"99") + STRING(MONTH(fat-duplic.dt-venciment),"99") + SUBSTRING(STRING(YEAR(fat-duplic.dt-venciment)),3,2) .
        END.
    
        IF SUBSTRING(nota-fiscal.nr-pedcli,4,1) = "P" THEN
            fabrica = "028".
        ELSE IF SUBSTRING(nota-fiscal.nr-pedcli,4,1) = "T" THEN
            fabrica = "081".
        ELSE IF SUBSTRING(nota-fiscal.nr-pedcli,4,1) = "C" THEN
            fabrica = "010".
        ELSE
            fabrica = "000".
    
        IF nota-fiscal.ind-tip-nota = 3 OR nota-fiscal.ind-tip-nota = 4 THEN 
            ASSIGN tipo = "X"
                   especie = "04".
        ELSE
            ASSIGN tipo = "P"
                   especie = "02".

        totipi = 0.
        FIND FIRST natur-oper OF nota-fiscal NO-LOCK NO-ERROR.
        IF AVAIL natur-oper AND natur-oper.cd-trib-ipi = 1 THEN
            totipi = nota-fiscal.vl-tot-ipi.

        /*
           AE1
        */
        PUT STREAM arqcatnf UNFORMATTED  
                  "AE1" STRING(INT(nota-fiscal.nr-nota-fis), "999999") c-serie FORMAT "x(4)"
                  SUBSTRING(STRING(YEAR(nota-fiscal.dt-emis-nota)),3,2) + STRING(MONTH(nota-fiscal.dt-emis-nota), "99") + STRING(DAY(nota-fiscal.dt-emis-nota), "99")
                  STRING(qty_it, "999")
                  REPLACE(STRING(nota-fiscal.vl-tot-nota, "999999999999999.99"), ",","")
                  "4"
                  SUBSTR(STRING(INT(REPLACE(natur-oper.cod-cfop,",","")) , "99999"),1,3) /* ? */
                  REPLACE(STRING(tot_icm, "999999999999999.99"), ",","")
                  dtvenc
                  especie /* vede anexo ? */
                  REPLACE(STRING(totipi,  "999999999999999.99"), ",","")
                  fabrica
                  SUBSTRING(STRING(YEAR(nota-fiscal.dt-emis-nota + 1)),3,2) + STRING(MONTH(nota-fiscal.dt-emis-nota + 1), "99") + STRING(DAY(nota-fiscal.dt-emis-nota + 1), "99")
                  "0000"
                  FILL(" ",20)
                  FILL(" ",10) /* filler */ 
                  SKIP /* SKIP*/.
    
        nlinhas = nlinhas + 1.
    
        cont = 0.

        FIND FIRST edi-ccs NO-LOCK WHERE edi-ccs.nr-nota-fis  = nota-fiscal.nr-nota-fis
                                     AND edi-ccs.serie        = nota-fiscal.serie NO-ERROR.
        IF NOT AVAIL edi-ccs THEN DO:
            CREATE edi-ccs.
            ASSIGN edi-ccs.nr-nota-fis  = nota-fiscal.nr-nota-fis
                   edi-ccs.serie        = nota-fiscal.serie
                   edi-ccs.dt-emis-nota = nota-fiscal.dt-emis-nota
                   edi-ccs.hora         = STRING(TIME, "HH:MM").
        END.
    
        /*
         *************************** ZND DETALHE ****************************
        */
       FOR EACH it-nota-fisc NO-LOCK WHERE it-nota-fisc.cod-estabel = nota-fiscal.cod-estabel
                                        AND it-nota-fisc.nr-nota-fis = nota-fiscal.nr-nota-fis
                                        AND it-nota-fisc.serie       = nota-fiscal.serie :
    
            IF it-nota-fisc.it-codigo = "" THEN NEXT.    
            FIND FIRST ITEM OF it-nota-fisc NO-LOCK NO-ERROR.
            IF NOT AVAIL ITEM THEN NEXT.

            cont = cont + 1.

            IF ITEM.codigo-refer = "" THEN DO:
                ASSIGN it-cliente = "S/CODIGO"
                       rev   = "".
            END. ELSE DO:
                it-cliente = TRIM(REPLACE(REPLACE( ENTRY(1, ITEM.codigo-refer, "#") ,"-","")," ","")) NO-ERROR.
                it-cliente = REPLACE( it-cliente ,".","").
                IF cli <> 10002155 AND cli <> 10009063 THEN
                    rev   = ENTRY(2, ITEM.codigo-refer, "#" ) NO-ERROR.
            END.


            ponbr = "".
            IF cli = 10002395 THEN DO:
                FOR FIRST ped-item NO-LOCK WHERE ped-item.nr-pedcli = it-nota-fisc.nr-pedcli ,
                    FIRST ped-ent OF ped-item NO-LOCK :
                    ponbr = TRIM(REPLACE(ped-ent.observacao, "OC", "")).
                END.
            END. ELSE DO:
                ponbr = SUBSTRING(it-nota-fisc.nr-pedcli,1,9).
            END.

            IF cli = 10002155 AND NOT ponbr BEGINS "#" THEN ponbr = "5500008945".
            ASSIGN ponbr = REPLACE(ponbr, "#","")
                   un = it-nota-fisc.un[1].

            /*un = "EA".*/
            IF cli = 10000159 THEN
                un = "01".
    
            /*
               AE2 
            */
            PUT STREAM arqcatnf UNFORMATTED  
                     "AE2" 
                     STRING(cont, "999")
                     UPPER(ponbr + FILL(" ", 12 - LENGTH(ponbr)))
                     UPPER(it-cliente + FILL(" ", 30 - LENGTH(it-cliente)))
                     REPLACE(STRING(it-nota-fisc.qt-faturada[1], "99999.9999"), ",","")
                     un FILL(" ", 2 - LENGTH(un))
                     STRING( INT(REPLACE(it-nota-fisc.class-fiscal, ",","")), "9999999999" ).

            IF totipi = 0 THEN 
                 PUT STREAM arqcatnf UNFORMATTED "0000".
            ELSE
                 PUT STREAM arqcatnf UNFORMATTED REPLACE(STRING(it-nota-fisc.aliquota-ipi, "99.99"), ",","").

            PUT STREAM arqcatnf UNFORMATTED  
                     REPLACE(STRING(TRUNCATE(ABS(it-nota-fisc.vl-preuni),4), "9999999.99999"), ",","")
                     REPLACE(STRING(it-nota-fisc.qt-faturada[1], "99999.9999"), ",","")
                     un
                     FILL("0", 9) + FILL(" ", 2)
                     tipo 
                     REPLACE(STRING(ABS(it-nota-fisc.val-pct-desconto-total), "99.99"), ",","")
                     REPLACE(STRING(ABS(it-nota-fisc.vl-desconto), "999999999.99"), ",","")
                     FILL(" ", 5)
                     SKIP /* SKIP*/ .
    
            nlinhas = nlinhas + 1.

            IF totipi = 0 THEN
                itipi = 0.
            ELSE
                itipi = it-nota-fisc.vl-ipi-it.
        
            /*
                AE4
            */
            PUT STREAM arqcatnf UNFORMATTED  
                     "AE4"
                     REPLACE(STRING(it-nota-fisc.aliquota-icm, "99.99"), ",","")
                     REPLACE(STRING(it-nota-fisc.vl-bicms-it, "999999999999999.99"), ",","")
                     REPLACE(STRING(it-nota-fisc.vl-icms-it , "999999999999999.99"), ",","")
                     REPLACE(STRING(itipi  , "999999999999999.99"), ",","")
                     "00"
                     FILL(" ", 30) 
                     "000000" 
                     FILL(" ", 13) 
                     FILL(" ", 19)
                     SKIP /* SKIP*/.
    
            nlinhas = nlinhas + 1.
           
        END.

    END. /* nota-fiscal */        

    nlinhas = nlinhas + 1.

    /*
        FTP
    */
    PUT STREAM arqcatnf UNFORMATTED
            "FTP"
            STRING(ctrl, "99999")
            STRING(nlinhas, "999999999")
            FILL("0", 17)
            "D"
            FILL(" ", 93) /* filler */
            SKIP /* SKIP*/.

    OUTPUT STREAM arqcatnf CLOSE.

END PROCEDURE.


PROCEDURE geraEDI04:
    DEF INPUT PARAM cod-estabel LIKE nota-fiscal.cod-estabel NO-UNDO.
    DEF INPUT PARAM nr-nota     LIKE nota-fiscal.nr-nota-fis NO-UNDO.
    DEF INPUT PARAM serie       LIKE nota-fiscal.serie NO-UNDO.

    DEF VAR cli LIKE emitente.cod-emitente NO-UNDO.

    DEF VAR ccs   AS CHAR NO-UNDO.
    DEF VAR c-te1 AS CHAR NO-UNDO.

    DEF VAR totnf  AS DEC NO-UNDO.
    DEF VAR totipi AS DEC NO-UNDO.
    DEF VAR itipi  AS DEC NO-UNDO.

    DEF VAR cont    AS DECIMAL NO-UNDO.
    DEF VAR qty_it  AS DECIMAL NO-UNDO.
    DEF VAR dtvenc  AS CHAR FORMAT "x(6)" INITIAL "AAMMDD" NO-UNDO.
    DEF VAR especie AS CHAR FORMAT "x(2)" INITIAL "02" NO-UNDO.
    DEF VAR fabrica AS CHAR FORMAT "x(3)" INITIAL "000" NO-UNDO.
    /*? P=Producao R=Reposicao T=Triangulacao E=Exportacao X=Outros A=Amostra F=Ferramentas e Solucoes */
    DEF VAR tipo    AS CHAR FORMAT "x(1)" INITIAL "P" NO-UNDO.
    /* */
    DEF VAR it-cliente   AS CHAR NO-UNDO.
    DEF VAR ponbr   AS CHAR NO-UNDO.
    DEF VAR nlinhas AS INTEGER NO-UNDO.
    DEF VAR ctrl    AS INTEGER NO-UNDO.
    DEF VAR arq     AS CHAR NO-UNDO.

    DEF VAR rev   AS CHAR NO-UNDO.
    DEF VAR cgc   AS CHAR NO-UNDO.
    DEF VAR pasta AS CHAR NO-UNDO.

    DEF VAR un AS CHAR NO-UNDO FORMAT "x(2)".

    DEF VAR tot_icm  AS DECIMAL NO-UNDO.
    DEF VAR tot_bicm AS DECIMAL NO-UNDO.
    DEF VAR nome-abrev AS CHAR NO-UNDO.

    DEF VAR c-serie AS CHAR NO-UNDO.

    ASSIGN c-serie = serie.
    IF c-serie = "" THEN c-serie = "0".

    FIND FIRST nota-fiscal NO-LOCK WHERE nota-fiscal.cod-estabel = cod-estabel 
                                     AND nota-fiscal.nr-nota-fis = nr-nota  
                                     AND nota-fiscal.serie       = serie NO-ERROR.
    IF NOT AVAIL nota-fiscal THEN
        LEAVE.
    ELSE
        cli = nota-fiscal.cod-emitente.

    IF cli = 10000158 THEN 
        cli = 10000159.

    ccs = "Q9994G0 ".
    IF cli = 10002155 THEN ccs = "00034857".
    IF cli = 10009063 THEN ccs = "00008499".
    IF cli = 10006602 THEN ccs = "Q9994G6 ".
                                          
    FIND FIRST emitente NO-LOCK WHERE emitente.cod-emitente = cli NO-ERROR.
    IF NOT AVAIL emitente THEN LEAVE.

    ASSIGN cgc = emitente.cgc
           nome-abrev = SUBSTR(emitente.nome-emit,1,25).

    IF cli = 10000124 OR cli = 10005825 OR cli = 10006602 THEN
        pasta = "\\192.0.0.149\ediccs$\AV\".
    ELSE IF cli = 10002395 OR cli = 10005276 OR cli = 10005705 OR cli = 10005725 THEN
            pasta = "\\192.0.0.149\ediccs$\sintel\snd\".
    ELSE
        pasta = "\\192.0.0.149\ediccs$\EDI\".

    arq = pasta + cgc + "_RND00404_00964350000178_" +
          STRING(YEAR(TODAY)) + STRING(MONTH(TODAY),"99") + STRING(DAY(TODAY),"99") +
          REPLACE(STRING(TIME,"hh:mm:ss"),":","") + ".TXT".

    FILE-INFO:FILE-NAME = arq.

    DO WHILE FILE-INFO:PATHNAME <> ?:

        arq = pasta + cgc + "_RND00404_00964350000178_" +
              STRING(YEAR(TODAY)) + STRING(MONTH(TODAY),"99") + STRING(DAY(TODAY),"99") +
              STRING(INTEGER(REPLACE(STRING(TIME,"hh:mm:ss"),":","")) + 1, "999999")
              + ".TXT".
        
        FILE-INFO:FILE-NAME = arq.

    END.
    
    OUTPUT STREAM arqcatnf TO VALUE(arq).
    
    nlinhas = 0.

    FOR EACH nota-fiscal NO-LOCK WHERE nota-fiscal.cod-estabel = cod-estabel 
                                   AND nota-fiscal.nr-nota-fis = nr-nota  
                                   AND nota-fiscal.serie       = serie :
        
        /*IF zn_type = "D" THEN NEXT. 
        IF zn_cancel_date <> ? THEN NEXT.*/
    
        ctrl = 0.
        RUN contaEnvios(nota-fiscal.dt-emis-nota, OUTPUT ctrl).

        /*
           ITP
        */
        PUT STREAM arqcatnf UNFORMATTED
                "ITP" 
                "00404"
                STRING(ctrl, "99999")
                SUBSTRING(STRING(YEAR(TODAY)),3,2) STRING(MONTH(TODAY),"99") STRING(DAY(TODAY),"99") 
                REPLACE(STRING(TIME,"hh:mm:ss"),":","")
                "00964350000178" + cgc + ccs + "        CCS TENOLOGIA INDUSTRIAL " nome-abrev + FILL(" ", 25 - LENGTH(nome-abrev))                                          
                FILL(" ",9)
                SKIP /* SKIP*/ .
    
        nlinhas = nlinhas + 1.
    
    
        qty_it = 0.
        RUN contaItems(INPUT nota-fiscal.cod-estabel, 
                       INPUT nota-fiscal.nr-nota-fis, 
                       INPUT nota-fiscal.serie, 
                       OUTPUT qty_it,
                       OUTPUT tot_icm,
                       OUTPUT tot_bicm,
                       OUTPUT dtvenc
                      ).
                   
        FIND FIRST fat-duplic WHERE fat-duplic.cod-estabel = nota-fiscal.cod-estabel 
                                AND fat-duplic.serie       = nota-fiscal.serie  
                                AND fat-duplic.nr-fatura   = nota-fiscal.nr-nota-fis NO-LOCK NO-ERROR.
        IF NOT AVAIL fat-duplic THEN
            dtvenc = "000000".
        ELSE DO:
            IF nota-fiscal.cod-emitente = 10002155 OR nota-fiscal.cod-emitente = 10009063 THEN
                dtvenc = SUBSTRING(STRING(YEAR(fat-duplic.dt-venciment)),3,2) + STRING(MONTH(fat-duplic.dt-venciment),"99") +  STRING(DAY(fat-duplic.dt-venciment),"99").
            ELSE
                dtvenc = STRING(DAY(fat-duplic.dt-venciment),"99") + STRING(MONTH(fat-duplic.dt-venciment),"99") + SUBSTRING(STRING(YEAR(fat-duplic.dt-venciment)),3,2) .
        END.
    
        IF SUBSTRING(nota-fiscal.nr-pedcli,4,1) = "P" THEN
            fabrica = "028".
        ELSE IF SUBSTRING(nota-fiscal.nr-pedcli,4,1) = "T" THEN
            fabrica = "081".
        ELSE IF SUBSTRING(nota-fiscal.nr-pedcli,4,1) = "C" THEN
            fabrica = "010".
        ELSE
            fabrica = "000".

        IF nota-fiscal.ind-tip-nota = 3 OR nota-fiscal.ind-tip-nota = 4 THEN 
            ASSIGN tipo = "X"
                   especie = "04".
        ELSE
            ASSIGN tipo = "P"
                   especie = "02".
    
        totipi = 0.
        FIND FIRST natur-oper OF nota-fiscal NO-LOCK NO-ERROR.
        IF AVAIL natur-oper AND natur-oper.cd-trib-ipi = 1 THEN
            totipi = nota-fiscal.vl-tot-ipi.

        /*
           AE1
        */
        PUT STREAM arqcatnf UNFORMATTED  
                  "AE1" STRING(INT(nota-fiscal.nr-nota-fis), "999999") c-serie FORMAT "x(4)"
                  SUBSTRING(STRING(YEAR(nota-fiscal.dt-emis-nota)),3,2) + STRING(MONTH(nota-fiscal.dt-emis-nota), "99") + STRING(DAY(nota-fiscal.dt-emis-nota), "99")
                  STRING(qty_it, "999")
                  REPLACE(STRING(nota-fiscal.vl-tot-nota, "999999999999999.99"), ",","")
                  "0"
                  SUBSTR(STRING(INT(REPLACE(natur-oper.cod-cfop,",","")) , "99999"), 1, 3)
                  REPLACE(STRING(tot_icm, "999999999999999.99"), ",","")
                  dtvenc
                  especie /* vede anexo ? */
                  REPLACE(STRING(totipi,  "999999999999999.99"), ",","")
                  fabrica
                  SUBSTRING(STRING(YEAR(nota-fiscal.dt-emis-nota + 1)),3,2) + STRING(MONTH(nota-fiscal.dt-emis-nota + 1), "99") + STRING(DAY(nota-fiscal.dt-emis-nota + 1), "99")
                  "0000"
                  SUBSTRING(fn-free-accent(natur-oper.denominacao), 1, 20) FORMAT "x(20)" 
                  FILL(" ",10) /* filler */ 
                  SKIP /* SKIP*/.
    
        nlinhas = nlinhas + 1.
    
        /* n∆o tem no manual da cummins
        /*
           NF2
        */
        PUT STREAM arqcatnf UNFORMATTED  
                  "NF2" 
                  FILL("0", 48) 
                  REPLACE(STRING(tot_bicm, "9999999999.99"), ",","")
                  FILL(" ", 65) 
                  SKIP /* SKIP*/.
    
        nlinhas = nlinhas + 1.
        */
    
        /* n∆o tem no manual da cummins
        /*
           NF3
        */
        PUT STREAM arqcatnf UNFORMATTED  
                 "NF3" 
                 dtvenc 
                 FILL("0", 54) 
                 FILL(" ", 65)
                 SKIP /* SKIP*/.
    
        nlinhas = nlinhas + 1.
        */
    
        cont = 0.
    
        /*
         *************************** DETALHE ****************************
        */
       FOR EACH it-nota-fisc NO-LOCK WHERE it-nota-fisc.cod-estabel = nota-fiscal.cod-estabel
                                        AND it-nota-fisc.nr-nota-fis = nota-fiscal.nr-nota-fis
                                        AND it-nota-fisc.serie       = nota-fiscal.serie :
    
            IF it-nota-fisc.it-codigo = "" THEN NEXT.    
            FIND FIRST ITEM OF it-nota-fisc NO-LOCK NO-ERROR.
            IF NOT AVAIL ITEM THEN NEXT.


            cont = cont + 1.

            /* TODO: colocar como funáao ??
            CREATE edi_Cat.
            ASSIGN edicat_part    = it-nota-fisc.it-codigo
                   edicat_nf      = nota-fiscal.nr-nota-fis
                   edicat_linha   = cont
                   edicat_control = ctrl
                   edicat_hora    = STRING(TIME, "HH:MM")
                   edicat_emis    = nota-fiscal.dt-emis-nota
                   edicat_cliente = nota-fiscal.cod-emitente  NO-ERROR.
            */

            IF ITEM.codigo-refer = "" THEN DO:
                ASSIGN it-cliente = "S/CODIGO"
                       rev   = "".
            END. ELSE DO:
                it-cliente = TRIM(REPLACE(REPLACE( ENTRY(1, ITEM.codigo-refer, "#") ,"-","")," ","")) NO-ERROR.
                it-cliente = REPLACE( it-cliente ,".","").
                IF cli <> 10002155 AND cli <> 10009063 THEN
                    rev   = ENTRY(2, ITEM.codigo-refer, "#" ) NO-ERROR.
            END.

            ponbr = "".
            IF cli = 10002395 THEN DO:
                FOR FIRST ped-item NO-LOCK WHERE ped-item.nr-pedcli = it-nota-fisc.nr-pedcli ,
                    FIRST ped-ent OF ped-item NO-LOCK :
                    ponbr = TRIM(REPLACE(ped-ent.observacao, "OC", "")).
                END.
            END. ELSE DO:
                ponbr = SUBSTRING(it-nota-fisc.nr-pedcli,1,9).
            END.

            IF cli = 10002155 AND NOT ponbr BEGINS "#" THEN ponbr = "5500008945".
            ASSIGN ponbr = REPLACE(ponbr, "#","").

            /*un = "EA".*/
            un = it-nota-fisc.un[1].
            IF cli = 10000159 THEN
                un = "01".
    
            /*
               AE2 
            */
            PUT STREAM arqcatnf UNFORMATTED  
                     "AE2" 
                     STRING(cont, "999")
                     UPPER(ponbr + FILL(" ", 12 - LENGTH(ponbr)))
                     UPPER(it-cliente + FILL(" ", 30 - LENGTH(it-cliente)))
                     STRING(it-nota-fisc.qt-faturada[1], "999999999")
                     un FILL(" ", 2 - LENGTH(un))
                     STRING( INT(REPLACE(it-nota-fisc.class-fiscal, ",","")), "9999999999" ).

            IF totipi = 0 THEN 
                 PUT STREAM arqcatnf UNFORMATTED "0000".
            ELSE
                 PUT STREAM arqcatnf UNFORMATTED REPLACE(STRING(it-nota-fisc.aliquota-ipi, "99.99"), ",","").

            PUT STREAM arqcatnf UNFORMATTED  
                     REPLACE(STRING(ABS(it-nota-fisc.vl-preuni), "999999999999999.99"), ",","")
                     STRING(it-nota-fisc.qt-faturada[1], "999999999") 
                     un
                     FILL("0", 9) + FILL(" ", 2)
                     tipo 
                     REPLACE(STRING(ABS(it-nota-fisc.val-pct-desconto-total), "99.99"), ",","")
                     REPLACE(STRING(ABS(it-nota-fisc.vl-desconto), "999.99"), ",","")
                     FILL(" ", 6)
                     SKIP /* SKIP*/ .
    
            nlinhas = nlinhas + 1.

            IF totipi = 0 THEN
                itipi = 0.
            ELSE
                itipi = it-nota-fisc.vl-ipi-it.
        
            /*
                AE4
            */
            PUT STREAM arqcatnf UNFORMATTED  
                     "AE4"
                     REPLACE(STRING(it-nota-fisc.aliquota-icm, "99.99"), ",","")
                     REPLACE(STRING(it-nota-fisc.vl-bicms-it, "999999999999999.99"), ",","")
                     REPLACE(STRING(it-nota-fisc.vl-icms-it , "999999999999999.99"), ",","")
                     REPLACE(STRING(itipi  , "999999999999999.99"), ",","")
                     "00"
                     FILL(" ", 30) 
                     "000000" 
                     FILL(" ", 32)
                     SKIP /* SKIP*/.
    
            nlinhas = nlinhas + 1.
            
            /* n∆o tem no manual da cummins
            FIND FIRST natur-oper OF it-nota-fisc NO-LOCK NO-ERROR.
            /*
               AE7
            */
            PUT STREAM arqcatnf UNFORMATTED  
                      "AE7"
                      FILL("0", 12)
                      STRING(INT(REPLACE(natur-oper.cod-cfop,",","")), "99999") 
                      FILL("0", 48)
                      FILL(" ", 5) /* filler */
                      FILL(" ", 54) /* filler */
                      SKIP /* SKIP*/.
    
            nlinhas = nlinhas + 1.
            */
    
        END.

        IF nota-fiscal.ind-tip-nota = 3 OR nota-fiscal.ind-tip-nota = 4 THEN DO:

            c-te1 = fn-free-accent(natur-oper.denominacao) + " Referente Ös NFS: " + REPLACE(fi-achaNF(nota-fiscal.observ-nota), " ", ",").
            IF LENGTH(c-te1) > 120 THEN
                c-te1 = SUBSTR(c-te1, 1, 120).
            /*
               TE1
            */
            PUT STREAM arqcatnf UNFORMATTED  
                      "TE1"
                      c-te1
                      FILL(" ", 120 - LENGTH(c-te1))
                      FILL(" ", 5) /* filler */
                      SKIP.
    
            nlinhas = nlinhas + 1.

        END.

    END. /* nota-fiscal */        


    nlinhas = nlinhas + 1.

    /*
        FTP
    */
    PUT STREAM arqcatnf UNFORMATTED
            "FTP"
            STRING(ctrl, "99999")
            STRING(nlinhas, "999999999")
            FILL("0", 17)
            "D"
            FILL(" ", 93) /* filler */
            SKIP /* SKIP*/.

    OUTPUT STREAM arqcatnf CLOSE.

END PROCEDURE.


PROCEDURE contaItems:
    DEF INPUT  PARAM cod-estabel AS CHAR NO-UNDO.
    DEF INPUT  PARAM nbr         AS CHAR NO-UNDO.
    DEF INPUT  PARAM ser         AS CHAR NO-UNDO.
    DEF OUTPUT PARAM nnf         AS INTEGER NO-UNDO.
    DEF OUTPUT PARAM tot_icm     AS DECIMAL NO-UNDO.
    DEF OUTPUT PARAM tot_bicm    AS DECIMAL NO-UNDO.
    DEF OUTPUT PARAM dtduedt     AS CHAR NO-UNDO.

    DEF BUFFER bit-nota-fisc FOR it-nota-fisc.

    ASSIGN nnf      = 0
           tot_icm  = 0
           tot_bicm = 0.

    FOR EACH bit-nota-fisc NO-LOCK WHERE bit-nota-fisc.cod-estabel = cod-estabel 
                                     AND bit-nota-fisc.nr-nota-fis = nbr 
                                     AND bit-nota-fisc.serie = ser :
        IF bit-nota-fisc.it-codigo = "" THEN NEXT.
        ASSIGN nnf      = nnf + 1.

        IF bit-nota-fisc.cd-trib-icm <> 3 THEN
            ASSIGN  tot_icm  = tot_icm  + bit-nota-fisc.vl-icms-it
                    tot_bicm = tot_bicm + bit-nota-fisc.vl-bicms-it.

        FIND FIRST ped-ent NO-LOCK WHERE ped-ent.nr-pedcli = bit-nota-fisc.nr-pedcli 
                                     AND ped-ent.it-codigo = bit-nota-fisc.it-codigo
                                     AND ped-ent.nr-sequencia = bit-nota-fisc.nr-seq-ped NO-ERROR.
        IF AVAIL ped-ent THEN
            dtduedt = SUBSTRING(STRING(YEAR(ped-ent.dt-entrega)),3,2) + STRING(MONTH(ped-ent.dt-entrega), "99") + STRING(DAY(ped-ent.dt-entrega), "99").
        ELSE 
            dtduedt = "000000".
    END.

END PROCEDURE.

PROCEDURE contaEnvios:
    DEF INPUT  PARAM data AS DATE.
    DEF OUTPUT PARAM c AS INTEGER.

    DEF BUFFER bnota-fiscal FOR nota-fiscal.

    c = 1.
    FOR EACH bnota-fiscal NO-LOCK WHERE bnota-fiscal.dt-emis-nota = data:
        c = c + 1.
    END.

END PROCEDURE.


/*********** EDI EMAIL MAXION **********/
PROCEDURE geraNOTEDI:
    DEF INPUT PARAM cod-estabel LIKE nota-fiscal.cod-estabel NO-UNDO.
    DEF INPUT PARAM nr-nota     LIKE nota-fiscal.nr-nota-fis NO-UNDO.
    DEF INPUT PARAM serie       LIKE nota-fiscal.serie NO-UNDO.

    DEF VAR cli LIKE emitente.cod-emitente NO-UNDO.

    DEF VAR ccs AS CHAR NO-UNDO.

    DEF VAR dtvenc  AS CHAR FORMAT "x(6)" INITIAL "AAMMDD" NO-UNDO.
    DEF VAR it-cliente   AS CHAR NO-UNDO.
    DEF VAR ponbr   AS CHAR NO-UNDO.
    DEF VAR ctrl    AS INTEGER NO-UNDO.
    DEF VAR arq     AS CHAR NO-UNDO.

    DEF VAR cgc   AS CHAR NO-UNDO.
    DEF VAR pasta AS CHAR NO-UNDO.

    DEF VAR nome-abrev AS CHAR NO-UNDO.

    FIND FIRST nota-fiscal NO-LOCK WHERE nota-fiscal.cod-estabel = cod-estabel 
                                     AND nota-fiscal.nr-nota-fis = nr-nota  
                                     AND nota-fiscal.serie       = serie NO-ERROR.
    IF NOT AVAIL nota-fiscal THEN
        LEAVE.
    ELSE
        cli = nota-fiscal.cod-emitente.

    IF cli = 10000158 THEN 
        cli = 10000159.

    FIND FIRST emitente NO-LOCK WHERE emitente.cod-emitente = cli NO-ERROR.
    IF NOT AVAIL emitente THEN LEAVE.

    ASSIGN cgc = emitente.cgc
           nome-abrev = SUBSTR(emitente.nome-emit,1,25).

    pasta = "\\192.0.0.149\ediccs$\EDI\".

    arq = pasta + cgc + "_EMAIL_00964350000178_" +
          STRING(YEAR(TODAY)) + STRING(MONTH(TODAY),"99") + STRING(DAY(TODAY),"99") +
          REPLACE(STRING(TIME,"hh:mm:ss"),":","") + ".TXT".

    FILE-INFO:FILE-NAME = arq.
    DO WHILE FILE-INFO:PATHNAME <> ?:
        arq = pasta + cgc + "_EMAIL_00964350000178_" +
              STRING(YEAR(TODAY)) + STRING(MONTH(TODAY),"99") + STRING(DAY(TODAY),"99") +
              STRING(INTEGER(REPLACE(STRING(TIME,"hh:mm:ss"),":","")) + 1, "999999")
              + ".TXT".
        FILE-INFO:FILE-NAME = arq.
    END.
    
    OUTPUT STREAM arqcatnf TO VALUE(arq).
    

    FOR EACH nota-fiscal NO-LOCK WHERE nota-fiscal.cod-estabel = cod-estabel 
                                   AND nota-fiscal.nr-nota-fis = nr-nota  
                                   AND nota-fiscal.serie       = serie :
        
        PUT STREAM arqcatnf UNFORMATTED
                "NF : " nota-fiscal.nr-nota-fis "/"  nota-fiscal.serie SKIP.

        FIND FIRST fat-duplic WHERE fat-duplic.cod-estabel = nota-fiscal.cod-estabel 
                                AND fat-duplic.serie       = nota-fiscal.serie  
                                AND fat-duplic.nr-fatura   = nota-fiscal.nr-nota-fis NO-LOCK NO-ERROR.
        IF NOT AVAIL fat-duplic THEN
            dtvenc = "000000".
        ELSE DO:
            dtvenc = STRING(DAY(fat-duplic.dt-venciment),"99") + STRING(MONTH(fat-duplic.dt-venciment),"99") + SUBSTRING(STRING(YEAR(fat-duplic.dt-venciment)),3,2) .
        END.
    
        PUT STREAM arqcatnf UNFORMATTED  
                  " Emissao : "  nota-fiscal.dt-emis-nota 
                  " Vencimento : " dtvenc SKIP.
    
       FOR EACH it-nota-fisc NO-LOCK WHERE it-nota-fisc.cod-estabel = nota-fiscal.cod-estabel
                                        AND it-nota-fisc.nr-nota-fis = nota-fiscal.nr-nota-fis
                                        AND it-nota-fisc.serie       = nota-fiscal.serie :
    
            IF it-nota-fisc.it-codigo = "" THEN NEXT.    
            FIND FIRST ITEM OF it-nota-fisc NO-LOCK NO-ERROR.
            IF NOT AVAIL ITEM THEN NEXT.

            ponbr = it-nota-fisc.nr-pedcli.
            IF cli = 10002155 AND NOT ponbr BEGINS "#" THEN ponbr = "5500008945".
            ASSIGN ponbr = REPLACE(ponbr, "#","").

            FOR FIRST ped-item NO-LOCK WHERE ped-item.nr-pedcli = it-nota-fisc.nr-pedcli ,
                FIRST ped-ent OF ped-item NO-LOCK :
                    ponbr = TRIM(REPLACE(ped-ent.observacao, "OC", "")).
            END.

            PUT STREAM arqcatnf UNFORMATTED  
                     " Item : " ITEM.codigo-refer " Item CCS : " it-nota-fisc.it-codigo SKIP
                     " Pedido : " UPPER(ponbr) SKIP
                     " Qtd : " it-nota-fisc.qt-faturada[1] " Valor Unit. : " it-nota-fisc.vl-preuni SKIP.

        END.

    END. /* nota-fiscal */        

    OUTPUT STREAM arqcatnf CLOSE.

END PROCEDURE.
/******************/
FUNCTION fi-achaNF RETURNS CHAR ( obs AS CHAR ):

    DEF VAR nfs AS CHAR    NO-UNDO INIT "".
    DEF VAR i   AS INTEGER NO-UNDO.

    DO i = 1 TO LENGTH(obs) - 6:
        IF (isNumeric(SUBSTR(obs, i, 1)) AND
            isNumeric(SUBSTR(obs, i + 1, 1)) AND
            isNumeric(SUBSTR(obs, i + 2, 1)) AND
            isNumeric(SUBSTR(obs, i + 3, 1)) AND
            isNumeric(SUBSTR(obs, i + 4, 1)) AND
            isNumeric(SUBSTR(obs, i + 5, 1)))
         OR
            (SUBSTR(obs, i, 1) = "." AND 
             isNumeric(SUBSTR(obs, i + 1, 1)) AND
             isNumeric(SUBSTR(obs, i + 2, 1)) AND
             isNumeric(SUBSTR(obs, i + 3, 1)) AND
             isNumeric(SUBSTR(obs, i + 4, 1)))
            THEN DO:

            IF SUBSTR(obs, i, 1) = "." THEN DO:
                nfs = nfs + SUBSTRING(obs, i + 1, INDEX(SUBSTR(obs, i + 1, 7)," ")).
            END. ELSE DO:
    
                IF isNumeric(SUBSTRING(obs, i + 6, 1)) THEN DO:
                    nfs = nfs + SUBSTRING(obs, i, 7) + " ".
                    ASSIGN i = i + 7.
                END. ELSE DO:
                    nfs = nfs + SUBSTRING(obs, i, 6) + " ".
                    ASSIGN i = i + 6.
                END.
            END.
        END.
 /*       IF isNumeric(SUBSTR(obs, i, 1)) AND
           isNumeric(SUBSTR(obs, i + 1, 1)) AND
           isNumeric(SUBSTR(obs, i + 2, 1)) AND
           isNumeric(SUBSTR(obs, i + 3, 1)) AND
           isNumeric(SUBSTR(obs, i + 4, 1)) AND
           isNumeric(SUBSTR(obs, i + 5, 1)) THEN DO:

            IF isNumeric(SUBSTRING(obs, i + 6, 1)) THEN DO:
                nfs = nfs + SUBSTRING(obs, i, 7) + " ".
                ASSIGN i = i + 7.
            END. ELSE DO:
                nfs = nfs + SUBSTRING(obs, i, 6) + " ".
                ASSIGN i = i + 6.
            END.
        END.*/
    END.
    RETURN TRIM(nfs).
END FUNCTION.

FUNCTION fi-achaData RETURNS DATE ( cod-estabel AS CHAR ,
                                    nf AS CHAR,
                                    serie AS CHAR ):

    IF LENGTH(nf) < 7 THEN
        nf = STRING(int(nf), "9999999").

    FIND FIRST notafiscal NO-LOCK WHERE notafiscal.cod-estabel = cod-estabel
                                    AND notafiscal.nr-nota-fis = nf
                                    AND notafiscal.serie = serie NO-ERROR.
    IF AVAIL notafiscal THEN
        RETURN notafiscal.dt-emis-nota.
    
END FUNCTION.

FUNCTION fi-achaDataT RETURNS CHAR ( obs AS CHAR ):

    DEF VAR data AS CHAR    NO-UNDO INIT "".
    DEF VAR i   AS INTEGER NO-UNDO.

    DO i = 1 TO LENGTH(obs):

        IF isNumeric(SUBSTR(obs, i, 1)) AND
           isNumeric(SUBSTR(obs, i + 1, 1)) AND
           SUBSTR(obs, i + 2, 1) = "/" AND
           isNumeric(SUBSTR(obs, i + 3, 1)) AND
           isNumeric(SUBSTR(obs, i + 4, 1)) AND
           SUBSTR(obs, i + 5, 1) = "/"  THEN DO:

            data = data + SUBSTRING(obs, i, 10) + " ".
            ASSIGN i = i + 6.

        END.
    END.
    RETURN TRIM(data).
END FUNCTION.

/******************************************************************/
FUNCTION isNumeric RETURNS LOGICAL ( num AS CHAR ):
    RETURN INDEX("0123456789", num) > 0.
END FUNCTION.

/******************************************************************/
PROCEDURE pi-acumFat:
    DEF INPUT PARAM c-cod-estabel AS CHAR NO-UNDO.
    DEF INPUT PARAM c-nr-nota-fis AS CHAR NO-UNDO.
    DEF INPUT PARAM c-serie       AS CHAR NO-UNDO.

    FOR EACH it-nota-fisc NO-LOCK WHERE it-nota-fisc.cod-estabel = c-cod-estabel
                                    AND it-nota-fisc.nr-nota-fis = c-nr-nota-fis
                                    AND it-nota-fisc.serie       = c-serie:
        IF it-nota-fisc.it-codigo = "" OR it-nota-fisc.nr-pedcli = "" THEN NEXT.

        FIND FIRST it-fat-acum WHERE it-fat-acum.it-codigo = it-nota-fisc.it-codigo
                                 AND it-fat-acum.nr-pedcli = it-nota-fisc.nr-pedcli NO-ERROR.
        IF NOT AVAIL it-fat-acum THEN DO:
            CREATE it-fat-acum.
            ASSIGN it-fat-acum.it-codigo = it-nota-fisc.it-codigo
                   it-fat-acum.nr-pedcli = it-nota-fisc.nr-pedcli.
        END.
        ASSIGN it-fat-acum.dt-ult-fat = it-nota-fisc.dt-emis-nota
               it-fat-acum.quantidade = it-fat-acum.quantidade + it-nota-fisc.qt-faturada[1].
    END.
END.
/******************************************************************/
FUNCTION espaco RETURNS CHAR (n AS INTEGER):
    DEF VAR i AS INTEGER NO-UNDO.
    DEF VAR ret AS CHAR NO-UNDO.
    DO i = 1 TO n:
        ret = ret + " ".
    END.
    RETURN ret.
END FUNCTION.
