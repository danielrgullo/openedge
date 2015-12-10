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
    pi-grava-pd1
    pi-grava-pd2
    pi-grava-ep1
    pi-grava-pd3

**/

/* temp-table */
{c:/work/desenv/rnd/tt-rnd12.i}
/**************/
{c:/work/desenv/edi/edi-util.p}
/**************/
DEFINE NEW GLOBAL SHARED VAR h-acomp-edi AS HANDLE NO-UNDO.

DEF VAR txt AS CLASS ccs.textoUtil.
txt = NEW ccs.textoUtil("").
DEF STREAM st.

PROCEDURE le-EDI:
    DEF INPUT PARAM nomeArquivo AS CHAR NO-UNDO.

    RUN pi-le-arquivo(INPUT nomeArquivo).
    IF RETURN-VALUE = "NOK" THEN 
        RETURN RETURN-VALUE.
    ELSE
        RUN pi-processa-EDI.
END.

/*************************************************************************************************************/

PROCEDURE pi-le-arquivo:
    DEF INPUT PARAM nomeArquivo AS CHAR NO-UNDO.

    DEF VAR c-tipoRegistro AS CHAR NO-UNDO FORMAT "x(3)".
    DEF VAR c-linha   AS CHAR NO-UNDO.
    DEF VAR c-estado  AS CHAR NO-UNDO.
    /* */
    DEF VAR pd1-rowid AS ROWID   NO-UNDO.
    DEF VAR pd2-rowid AS ROWID   NO-UNDO.
    /* */
    DEF VAR numLinha  AS INTEGER NO-UNDO INIT 0.

    DEF VAR l-acomp   AS LOGICAL NO-UNDO INIT FALSE.
    ASSIGN  l-acomp = VALID-HANDLE(h-acomp-edi).

    RUN pi-limpa-tabelas.

    IF naoExisteArquivo(nomeArquivo) THEN DO:        
        RETURN "NOK".
    END.

    DO ON STOP UNDO, LEAVE:
    
        INPUT STREAM st FROM VALUE(nomeArquivo) NO-ECHO NO-MAP NO-CONVERT.
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
                WHEN  "PD1" THEN DO:
                    ASSIGN c-estado = c-tipoRegistro.
                    RUN pi-grava-pd1(c-linha, OUTPUT pd1-rowid).
                END.
                WHEN  "EP1" THEN DO:
                    IF c-estado <> "PD1" AND c-estado <> "EP1" THEN STOP.
                    ASSIGN c-estado = c-tipoRegistro.
                    RUN pi-grava-ep1(c-linha, pd1-rowid).
                END.
                WHEN  "PD2" THEN DO:
                    IF c-estado <> "PD1" AND c-estado <> "PD2" AND c-estado <> "PD3" AND c-estado <> "EP1" THEN STOP.
                    c-estado = c-tipoRegistro.
                    RUN pi-grava-pd2(c-linha, pd1-rowid, OUTPUT pd2-rowid).
                END.
                WHEN  "PD3" THEN DO:
                    IF c-estado <> "PD2" AND c-estado <> "PD3" THEN STOP.
                    ASSIGN c-estado = c-tipoRegistro.
                    RUN pi-grava-pd3(c-linha, pd2-rowid). 
                END.
                /*
                WHEN  "PD4" THEN DO:
                    IF c-estado <> "PD3" AND c-estado <> "PD4" THEN STOP.
                    ASSIGN c-estado = c-tipoRegistro.
                END.
                WHEN  "PD5" THEN DO:
                    ASSIGN c-estado = c-tipoRegistro.
                END.
                */
                WHEN "TE1" THEN DO:
                    ASSIGN c-estado = c-tipoRegistro.
                    RUN pi-grava-te1(c-linha, pd1-rowid).
                END.
                WHEN "ITP" THEN DO:
                    ASSIGN c-estado = "FIM".
                END.
            END.
        END. /* </repeat> */
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
    EMPTY TEMP-TABLE tt-PD1.
    EMPTY TEMP-TABLE tt-PD2.
    EMPTY TEMP-TABLE tt-EP1.
    EMPTY TEMP-TABLE tt-PD3.
    EMPTY TEMP-TABLE tt-TE1.
END.

PROCEDURE pi-grava-itp:
    DEF INPUT PARAM c-linha AS CHAR NO-UNDO.
    txt:setText(c-linha).
    CREATE tt-itp.
    ASSIGN tt-itp.id-rnd        = txt:getIntSlice(4, 3)
           tt-itp.versao-rnd    = txt:getIntSlice(7, 2)
           tt-itp.num-controle  = txt:getIntSlice(9, 5)
           tt-itp.id-movimento  = txt:getChrSlice(14, 12)
           tt-itp.id-transmis   = txt:getChrSlice(26, 14)
           tt-itp.id-receptor   = txt:getChrSlice(40, 14)
           tt-itp.cod-transmis  = txt:getChrSlice(54, 8)
           tt-itp.cod-receptor  = txt:getChrSlice(62, 8)
           tt-itp.nome-transmis = txt:getChrSlice(70, 25)
           tt-itp.nome-receptor = txt:getChrSlice(95, 25).
END PROCEDURE.                                     

PROCEDURE pi-grava-PD1:
    DEF INPUT  PARAM c-linha   AS CHAR  NO-UNDO.
    DEF OUTPUT PARAM pd1-rowid AS ROWID NO-UNDO.
    txt:setText(c-linha).
    CREATE tt-PD1.
    ASSIGN tt-PD1.codigo-refer      = txt:getChrSlice(4, 30)
           tt-PD1.resp-embalagem    = txt:getChrSlice(34, 1)
           tt-PD1.it-codigo         = txt:getChrSlice(35, 30)
           tt-PD1.cod-local-destino = txt:getChrSlice(65, 5)
           tt-PD1.num-ult-nf        = txt:getChrSlice(70, 6)
           tt-PD1.serie-ult-nf      = txt:getChrSlice(76, 4)
           tt-PD1.dt-ult-nf         = txt:getDateSlice(80, 6)
           tt-PD1.qt-entrega-acum   = txt:getIntSlice(86, 14) / 1000
           tt-PD1.dt-entrega-acum   = txt:getDateSlice(100, 6)
           tt-PD1.qt-casas-decimais = txt:getIntSlice(106, 1)
           tt-PD1.outras-neces      = txt:getIntSlice(107, 9)
           tt-PD1.id-tipo-programa  = txt:getIntSlice(116, 1)
           tt-PD1.proposito-trans   = txt:getChrSlice(117, 2)
           tt-PD1.unid-medida       = txt:getChrSlice(119, 2)
           tt-PD1.cod-tipo-forn     = txt:getChrSlice(121, 1)
           tt-PD1.altera-tecnica    = txt:getChrSlice(122, 4)
           .
    ASSIGN pd1-rowid = ROWID(tt-PD1).
END.

PROCEDURE pi-grava-PD2:
    DEF INPUT PARAM c-linha    AS CHAR NO-UNDO.
    DEF INPUT PARAM pd1-rowid  AS ROWID NO-UNDO.
    DEF OUTPUT PARAM pd2-rowid AS ROWID NO-UNDO.

    DEF VAR divisor-casas-decimais AS INTEGER NO-UNDO INIT 1.
    DEF VAR tamanhoLinha AS INTEGER NO-UNDO.

    DEF VAR id-programacao AS CHAR NO-UNDO.

    txt:setText(c-linha).
    tamanhoLinha = txt:getTextSize().

    DEF VAR i AS INTEGER NO-UNDO.
    DEF VAR c AS INTEGER NO-UNDO INIT 4.

    CREATE tt-pd2.
    ASSIGN /*tt-pd2.ind = i-pe3*/
           tt-pd2.r-rowid = pd1-rowid
           .
    looop:
    DO i = 1 TO EXTENT(tt-pd2.dt-entrega):

        ASSIGN tt-pd2.dt-entrega[i] = txt:getDateSlice(c, 6)
               tt-pd2.hr-entrega[i] = txt:getIntSlice(c + 6, 4)
               tt-pd2.qt-entrega[i] = txt:getIntSlice(c + 10, 9)
               c = c + 19.

        IF c > tamanhoLinha THEN LEAVE looop.
    END.

    ASSIGN pd2-rowid = ROWID(tt-PD2).
END.

PROCEDURE pi-grava-EP1:
    DEF INPUT PARAM c-linha AS CHAR NO-UNDO.
    DEF INPUT PARAM pd1-rowid AS ROWID NO-UNDO.
    txt:setText(c-linha).
    CREATE tt-EP1.
    ASSIGN tt-ep1.cod-fab-destino   = txt:getChrSlice(4, 3)
           tt-ep1.id-programa-atu   = txt:getChrSlice(7, 9)
           tt-ep1.dt-programa-atu   = txt:getDateSlice(16, 6)
           tt-ep1.id-programa-ant   = txt:getChrSlice(22, 9)
           tt-ep1.dt-programa-ant   = txt:getDateSlice(31, 6)
           tt-ep1.qt-nesses-acum    = txt:getIntSlice(37, 14) / 1000
           tt-ep1.cod-freq-forn     = txt:getChrSlice(51, 3)
           tt-ep1.nr-ped-compra     = txt:getChrSlice(54, 12)
           tt-ep1.cod-loc-material  = txt:getChrSlice(66, 10)
           tt-EP1.r-rowid = pd1-rowid
           .
END PROCEDURE.

PROCEDURE pi-grava-PD3:
    DEF INPUT PARAM c-linha AS CHAR NO-UNDO.
    DEF INPUT PARAM pd2-rowid AS ROWID NO-UNDO.
    txt:setText(c-linha).
    CREATE tt-PD3.
    ASSIGN tt-pd3.chassi            = txt:getChrSlice(4, 21)
           tt-pd3.pedido-cliente    = txt:getChrSlice(25, 12)
           tt-pd3.pedido-fornecedor = txt:getChrSlice(37, 15)
           tt-pd3.descricao-item    = txt:getChrSlice(52, 25)
           tt-pd3.local-descarga    = txt:getChrSlice(77, 7)
           tt-pd3.num-seq-montagem  = txt:getChrSlice(84, 10)
           tt-pd3.esp-tecnica-mod   = txt:getChrSlice(94, 25)
           tt-PD3.r-rowid = pd2-rowid
           NO-ERROR.
END PROCEDURE.

PROCEDURE pi-grava-te1:
    DEF INPUT PARAM c-linha AS CHAR NO-UNDO.
    DEF INPUT PARAM r-rowid AS ROWID NO-UNDO.

    txt:setText(c-linha).

    CREATE tt-te1.
    ASSIGN tt-te1.txt-informa = txt:getChrSlice(4, 120)
           tt-te1.r-rowid = r-rowid.
END PROCEDURE.
