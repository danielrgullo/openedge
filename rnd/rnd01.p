/**
    le-EDI
        executa pi-le-arquivo
        executa pi-processa-EDI
    
    pi-le-arquivo
    	chama: pi-limpa-tabelas
    	abre arquivo
    	"opera" pi-grava-* para cada linha
    	
    pi-limpa-tabelas
    	limpa todas temp-tables
    
    pi-grava-itp
    pi-grava-pe1
    pi-grava-pe2
    pi-grava-pe3
    pi-grava-pe5
    pi-grava-pe6
    pi-grava-te1

**/
DEFINE NEW GLOBAL SHARED VAR h-acomp-edi AS HANDLE NO-UNDO.

/* temp-table */
{c:/work/desenv/rnd/tt-rnd01.i}
/**************/
{c:/work/desenv/edi/edi-util.p}
/**************/
DEF VAR txt AS CLASS ccs.textoUtil.
txt = NEW ccs.textoUtil("").

DEF STREAM st.

DEF VAR pedido-no-pe5 AS LOGICAL NO-UNDO INIT NO.

/*************************************************************************************************************/

PROCEDURE pi-le-arquivo:
    DEF INPUT PARAM nomeArquivo AS CHAR NO-UNDO.

    DEF VAR c-tipoRegistro AS CHAR NO-UNDO FORMAT "x(3)".
    DEF VAR c-linha   AS CHAR NO-UNDO.
    DEF VAR c-estado  AS CHAR NO-UNDO.
    DEF VAR i-pe3     AS INTEGER NO-UNDO.
    /* */
    DEF VAR pe1-rowid AS ROWID   NO-UNDO.
    /* */
    DEF VAR numLinha  AS INTEGER NO-UNDO INIT 0.

    DEF VAR l-acomp   AS LOGICAL NO-UNDO INIT FALSE.
    ASSIGN  l-acomp = VALID-HANDLE(h-acomp-edi).

    RUN pi-limpa-tabelas.

    IF naoExisteArquivo(nomeArquivo) THEN DO:
        RETURN "NOK".
    END.

    DO 
        ON STOP UNDO, LEAVE :
    
        INPUT STREAM st FROM VALUE(nomeArquivo) NO-ECHO NO-MAP NO-CONVERT.
        looop_arquivo:
        REPEAT:
            ASSIGN c-linha = "".
            IMPORT STREAM st UNFORMATTED c-linha.
    
            IF l-acomp THEN DO:
                ASSIGN numLinha = numLinha + 1.
                RUN pi-acompanhar IN h-acomp-edi (INPUT  "Lendo linha : " + STRING(numLinha))  NO-ERROR.
            END.
    
            ASSIGN c-tipoRegistro = SUBSTR(c-linha, 1, 3).        
            CASE c-tipoRegistro:
                WHEN "ITP" THEN DO:
                    ASSIGN c-estado = "INI".
                    RUN pi-grava-itp(c-linha).
                END.
                WHEN  "PE1" THEN DO:
                    ASSIGN c-estado = c-tipoRegistro
                           i-pe3    = 0.
                    RUN pi-grava-pe1(c-linha, OUTPUT pe1-rowid).
                END.
                WHEN  "PE2" THEN DO:
                    IF c-estado <> "PE1" THEN STOP.
                        
                    c-estado = c-tipoRegistro.
                    RUN pi-grava-pe2(c-linha, pe1-rowid).
                END.
                WHEN  "PE3" THEN DO:

                    IF c-estado <> "PE2" AND c-estado <> "PE3" AND c-estado <> "PE5"  THEN 
                        STOP.

                    /* temporario isso 
                    IF c-estado <> "PE2" AND c-estado <> "PE3" AND c-estado <> "PE5"  THEN DO:
                        ASSIGN c-estado = c-tipoRegistro.
                        NEXT. 
                    END.
                    */

                    ASSIGN c-estado = c-tipoRegistro
                           i-pe3    = i-pe3 + 1.
                    RUN pi-grava-pe3(c-linha, i-pe3, pe1-rowid).
                END.
                WHEN  "PE5" THEN DO:
                    IF c-estado <> "PE3" THEN STOP.
                    ASSIGN c-estado = c-tipoRegistro.
                    RUN pi-grava-pe5(c-linha, i-pe3, pe1-rowid). 
                END.
                WHEN "PE6" THEN DO:
                    ASSIGN c-estado = c-tipoRegistro.
                    RUN pi-grava-pe6(c-linha, pe1-rowid).
                END.
                WHEN "PE7" THEN DO:
                    ASSIGN c-estado = c-tipoRegistro.
                END.
                WHEN "PE8" THEN DO:
                    ASSIGN c-estado = c-tipoRegistro.
                END.
                WHEN "PE4" THEN DO:
                    ASSIGN c-estado = c-tipoRegistro.
                END.
                WHEN "TE1" THEN DO:
                    ASSIGN c-estado = c-tipoRegistro.
                    RUN pi-grava-te1(c-linha, pe1-rowid).
                END.
                WHEN "ITP" THEN DO:
                    ASSIGN c-estado = "FIM".
                END.
            END.
        END. /* </repeat> looop_arquivo */
        INPUT STREAM st CLOSE.
    
        IF l-acomp THEN DO:
            RUN pi-acompanhar IN h-acomp-edi (INPUT  "Arquivo lido.")  NO-ERROR.
        END.
        
        RETURN "OK".
    END.

    INPUT STREAM st CLOSE.
    
    RETURN "NOK".
END PROCEDURE.

PROCEDURE pi-limpa-tabelas:
    EMPTY TEMP-TABLE tt-itp.
    EMPTY TEMP-TABLE tt-PE1.
    EMPTY TEMP-TABLE tt-PE2.
    EMPTY TEMP-TABLE tt-PE3.
    EMPTY TEMP-TABLE tt-PE5.
    EMPTY TEMP-TABLE tt-PE6.
END.

PROCEDURE pi-grava-itp:
    DEF INPUT PARAM c-linha AS CHAR NO-UNDO.
    txt:setText(c-linha).
    CREATE tt-itp.
    ASSIGN tt-itp.id-rnd         = txt:getIntSlice(4, 3)
           tt-itp.versao-rnd     = txt:getIntSlice(7, 2)
           tt-itp.num-controle   = txt:getIntSlice(9, 5)
           tt-itp.id-movimento   = txt:getChrSlice(14, 12)
           tt-itp.ident-transmis = txt:getChrSlice(26, 14)
           tt-itp.ident-receptor = txt:getChrSlice(40, 14)
           tt-itp.cod-transmis   = txt:getChrSlice(54, 8)
           tt-itp.cod-receptor   = txt:getChrSlice(62, 8)
           tt-itp.nome-transmis  = txt:getChrSlice(70, 25)
           tt-itp.nome-receptor  = txt:getChrSlice(95, 25).
    /* pensar numa forma de melhorar isso a seguir : */
    ASSIGN pedido-no-pe5 = NO.
    FOR FIRST emitente NO-LOCK
            WHERE emitente.cgc = tt-itp.ident-transmis,
        FIRST ccs-edi-conf OF emitente NO-LOCK:
        ASSIGN pedido-no-pe5 = (ccs-edi-conf.l-oc-pe5 = YES).
    END.
END PROCEDURE.                                     

PROCEDURE pi-grava-pe1:
    DEF INPUT  PARAM c-linha   AS CHAR  NO-UNDO.
    DEF OUTPUT PARAM pe1-rowid AS ROWID NO-UNDO.
    txt:setText(c-linha).
    CREATE tt-pe1.
    ASSIGN tt-pe1.cod-fabrica-dest   = txt:getChrSlice(4, 3)
           tt-pe1.id-programa        = txt:getChrSlice(7, 9)
           tt-pe1.dt-prg-atu         = txt:getDateSlice(16, 6)
           tt-pe1.id-programa-ant    = txt:getChrSlice(22, 9)
           tt-pe1.dt-prg-ant         = txt:getDateSlice(31, 6)
           tt-pe1.cod-it-cliente     = txt:getChrSlice(37, 30)
           tt-pe1.cod-it-fornecedor  = txt:getChrSlice(67, 30)
           tt-pe1.num-ped-compra     = txt:getChrSlice(97, 12)
           tt-pe1.cod-local-destino  = txt:getChrSlice(109,5)
           tt-pe1.ident-contato      = txt:getChrSlice(114, 11)
           tt-pe1.cod-unidade-medida = txt:getChrSlice(125, 2)
           tt-pe1.qt-casas-decimais  = txt:getIntSlice(127, 1)
           tt-pe1.cod-tipo-fornec    = txt:getChrSlice(128,1).
    ASSIGN pe1-rowid = ROWID(tt-pe1).
END.

PROCEDURE pi-grava-pe2:
    DEF INPUT PARAM c-linha   AS CHAR NO-UNDO.
    DEF INPUT PARAM pe1-rowid AS ROWID NO-UNDO.

    IF NOT AVAIL tt-itp THEN
        FIND FIRST tt-itp NO-LOCK NO-ERROR.
    
    IF tt-itp.versao-rnd = 60 THEN DO: /* A Volvo utilzia essa versÆo. Arq: 001v60-9P.doc */
        RUN pi-grava-pe2-v60(c-linha, pe1-rowid).
    END. ELSE DO:
        RUN pi-grava-pe2-normal(c-linha, pe1-rowid).
    END.
END.
PROCEDURE pi-grava-pe2-normal:
    DEF INPUT PARAM c-linha   AS CHAR NO-UNDO.
    DEF INPUT PARAM pe1-rowid AS ROWID NO-UNDO.
    txt:setText(c-linha).    
    CREATE tt-pe2.
    ASSIGN tt-pe2.dt-ult-entrega    = txt:getDateSlice(4, 6)
           tt-pe2.num-ult-nf        = txt:getChrSlice(10, 6)
           tt-pe2.serie-ult-nf      = txt:getChrSlice(16, 4)
           tt-pe2.dt-ult-nf         = txt:getDateSlice(20, 6)
           tt-pe2.qt-ult-entrega    = txt:getDecSlice(26, 12) / 1000
           tt-pe2.qt-entrega-acu    = txt:getDecSlice(38, 14) / 1000
           tt-pe2.qt-necessa-acu    = txt:getDecSlice(52, 14) / 1000
           tt-pe2.qt-lote-minimo    = txt:getDecSlice(66, 12) / 1000
           tt-pe2.cod-freq-fornec   = txt:getChrSlice(78, 3)
           tt-pe2.dt-liberacao      = txt:getDateSlice(81, 6)
           tt-pe2.dt-libera-mp      = txt:getDateSlice(87, 6)
           tt-pe2.cod-local-desc    = txt:getChrSlice(93, 7)
           tt-pe2.periodo-embarq    = txt:getChrSlice(100, 4)
           tt-pe2.cod-sit-item      = txt:getChrSlice(104, 2)
           tt-pe2.id-tipo-prg       = txt:getChrSlice(106, 1)
           tt-pe2.pedido-reven      = txt:getChrSlice(107, 13)
           tt-pe2.qualific-prg      = txt:getChrSlice(120, 1)
           tt-pe2.tipo-ped-reven    = txt:getChrSlice(121, 2)
           tt-pe2.cod-via-trans     = txt:getChrSlice(123, 3)
           tt-pe2.r-rowid = pe1-rowid.

END PROCEDURE.
PROCEDURE pi-grava-pe2-v60:
    DEF INPUT PARAM c-linha   AS CHAR NO-UNDO.
    DEF INPUT PARAM pe1-rowid AS ROWID NO-UNDO.
    txt:setText(c-linha).    
    CREATE tt-pe2.
    ASSIGN tt-pe2.dt-ult-entrega    = txt:getDateSlice(4, 6)
           tt-pe2.num-ult-nf        = txt:getChrSlice(10, 9)
           tt-pe2.serie-ult-nf      = txt:getChrSlice(19, 4)
           tt-pe2.dt-ult-nf         = txt:getDateSlice(23, 6)
           tt-pe2.qt-ult-entrega    = txt:getDecSlice(29, 12) / 1000
           tt-pe2.qt-entrega-acu    = txt:getDecSlice(41, 14) / 1000
           tt-pe2.qt-necessa-acu    = txt:getDecSlice(55, 14) / 1000
           tt-pe2.qt-lote-minimo    = txt:getDecSlice(69, 12) / 1000
           tt-pe2.cod-freq-fornec   = txt:getChrSlice(81, 3) 
           tt-pe2.dt-liberacao      = txt:getAnoMesSlice(84, 4)
           tt-pe2.dt-libera-mp      = txt:getAnoMesSlice(88, 4)
           tt-pe2.cod-local-desc    = txt:getChrSlice(92, 7)
           tt-pe2.periodo-embarq    = txt:getChrSlice(99, 4)
           tt-pe2.cod-sit-item      = txt:getChrSlice(103, 2)
           tt-pe2.id-tipo-prg       = txt:getChrSlice(105, 1)
           tt-pe2.pedido-reven      = txt:getChrSlice(106, 13)
           tt-pe2.qualific-prg      = txt:getChrSlice(119, 1)
           tt-pe2.tipo-ped-reven    = txt:getChrSlice(120, 2)
           tt-pe2.cod-via-trans     = txt:getChrSlice(122, 3)
           tt-pe2.r-rowid = pe1-rowid.
END PROCEDURE.

PROCEDURE pi-grava-pe3:
    DEF INPUT PARAM c-linha AS CHAR NO-UNDO.
    DEF INPUT PARAM i-pe3   AS INT  NO-UNDO.
    DEF INPUT PARAM pe1-rowid AS ROWID NO-UNDO.

    DEF VAR divisor-casas-decimais AS INTEGER NO-UNDO INIT 1.
    DEF VAR tamanhoLinha AS INTEGER NO-UNDO.

    DEF VAR id-programacao AS CHAR NO-UNDO.
    DEF VAR c-pe5 AS CHAR NO-UNDO INIT "PE5".

    txt:setText(c-linha).
    tamanhoLinha = txt:getTextSize().

    DEF VAR i AS INTEGER NO-UNDO.
    DEF VAR c AS INTEGER NO-UNDO INIT 4.

    IF NOT AVAIL tt-itp THEN
        FIND FIRST tt-itp NO-LOCK NO-ERROR.

    FIND FIRST tt-pe1 NO-LOCK 
        WHERE ROWID(tt-pe1) = pe1-rowid NO-ERROR.
    IF AVAIL tt-pe1 THEN
        ASSIGN divisor-casas-decimais = EXP(10, tt-pe1.qt-casas-decimais). /* TODO: e se der erro ? */

    CREATE tt-pe3.
    ASSIGN tt-pe3.ind = i-pe3
           tt-pe3.r-rowid = pe1-rowid.
    looop:
    DO i = 1 TO 7:

        IF tt-itp.versao-rnd = 3 OR tt-itp.versao-rnd = 6 THEN DO: /* gambiarra para funcionar corretamente para a maxion e volvo (3)*/
        
            IF AVAIL emitente AND (emitente.cod-emitente = 10003835 OR emitente.cod-emitente = 10002155 OR emitente.cod-emitente = 10009063) THEN 
                ASSIGN id-programacao = "4".
            ELSE
                ASSIGN id-programacao = "1".
                
            IF txt:getChrSlice(c + 4, 2) = "00" THEN DO:
                txt:setChrSlice(c + 4, 2, "15").
                ASSIGN id-programacao = "4".
            END.
            ASSIGN c-pe5 = c-pe5 + txt:getChrSlice(c, 6)
                                 + id-programacao
                                 + "         ".
        END.

        ASSIGN tt-pe3.dt-entrega[i] = txt:getDateSlice(c, 6)
               tt-pe3.hr-entrega[i] = txt:getIntSlice(c + 6, 2)
               tt-pe3.qt-entrega[i] = txt:getIntSlice(c + 8, 9) / divisor-casas-decimais
               c = c + 17.        
        
        IF c > tamanhoLinha THEN LEAVE looop.
    END.

    /* gambiarra para funcionar corretamente para a maxion rnd_vers = 06 */
    IF tt-itp.versao-rnd = 3 OR tt-itp.versao-rnd = 6 THEN DO: 
        RUN pi-grava-pe5(c-pe5, i-pe3, pe1-rowid).
    END.
END PROCEDURE.

PROCEDURE pi-grava-pe5:
    DEF INPUT PARAM c-linha AS CHAR NO-UNDO.
    DEF INPUT PARAM i-pe3   AS INT  NO-UNDO.
    DEF INPUT PARAM pe1-rowid AS ROWID NO-UNDO.

    /*EF VAR c-numero-pedido AS CHAR NO-UNDO.*/

    DEF VAR tamanhoLinha AS INTEGER NO-UNDO.    

    txt:setText(c-linha).
    tamanhoLinha = txt:getTextSize().

    DEF VAR i AS INTEGER NO-UNDO.
    DEF VAR c AS INTEGER NO-UNDO INIT 4.

    /*ASSIGN c-numero-pedido = SUBSTR(c-linha, c + 7, 9).*/

    CREATE tt-pe5.
    ASSIGN tt-pe5.ind = i-pe3
           tt-pe5.r-rowid = pe1-rowid.
    looop:
    DO i = 1 TO 7:
        ASSIGN tt-pe5.dt-inic-entrega[i] = txt:getDateSlice(c, 6)
               tt-pe5.id-programacao[i]  = txt:getChrSlice(c + 6, 1)
               tt-pe5.id-prg-atual[i]    = txt:getChrSlice(c + 7, 9)
               c = c + 16.
        IF c > tamanhoLinha THEN 
            LEAVE looop.
    END.
END PROCEDURE.

PROCEDURE pi-grava-pe6:
    DEF INPUT PARAM c-linha AS CHAR NO-UNDO.
    DEF INPUT PARAM pe1-rowid AS ROWID NO-UNDO.

    txt:setText(c-linha).

    CREATE tt-pe6.
    ASSIGN tt-pe6.fat-conversao  = txt:getDecSlice(4, 10) / 100000
           tt-pe6.altera-tecnica = txt:getChrSlice(14, 4)
           tt-pe6.cod-material   = txt:getChrSlice(18, 10)
           tt-pe6.peso-item      = txt:getDecSlice(28, 12) / 1000
           tt-pe6.un-medida-peso = txt:getChrSlice(40, 2)
           tt-pe6.r-rowid = pe1-rowid.
END PROCEDURE.

PROCEDURE pi-grava-te1:
    DEF INPUT PARAM c-linha AS CHAR NO-UNDO.
    DEF INPUT PARAM pe1-rowid AS ROWID NO-UNDO.

    txt:setText(c-linha).

    FIND FIRST tt-te1
        WHERE tt-te1.r-rowid = pe1-rowid NO-ERROR.
    IF NOT AVAIL tt-te1 THEN DO:
        CREATE tt-te1.
        ASSIGN tt-te1.txt-informa = TRIM(txt:getChrSlice(4, 120))
               tt-te1.r-rowid = pe1-rowid.
    END. ELSE DO:
        ASSIGN tt-te1.txt-informa = tt-te1.txt-informa + TRIM(txt:getChrSlice(4, 120)).
    END.

END PROCEDURE.


/*
teste : *
DEF VAR arquivo AS CHAR NO-UNDO 
    INIT "\\servcitrix\edi-rec-sawluz\RND-001-08-53796098.EDI".

RUN pi-le-arquivo(arquivo).
DISP RETURN-VALUE.

FOR EACH tt-te1 NO-LOCK:
    DISP tt-te1
        EXCEPT r-ROWID
        WITH SCROLLABLE.
END.

PROCEDURE pi-processa-edi :
    
    FOR EACH tt-pe5 NO-LOCK:
        DISP tt-pe5
            EXCEPT tt-pe5.r-rowid
            WITH SCROLLABLE.
    END.
END PROCEDURE.

* */
