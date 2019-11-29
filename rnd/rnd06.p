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
    pi-grava-pp1
    pi-grava-pp2
    pi-grava-ae3
    pi-grava-pe4
    pi-grava-te1

**/

/* temp-table */
{c:/work/desenv/rnd/tt-rnd06.i}
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
    DEF VAR pp1-rowid AS ROWID   NO-UNDO.
    DEF VAR pp2-rowid AS ROWID   NO-UNDO.
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
                WHEN  "PP1" THEN DO:
                    ASSIGN c-estado = c-tipoRegistro.
                    RUN pi-grava-pp1(c-linha, OUTPUT pp1-rowid).
                END.
                WHEN  "AE3" THEN DO:
                    IF c-estado <> "PP1" AND c-estado <> "AE3" THEN STOP.
                    ASSIGN c-estado = c-tipoRegistro.
                    RUN pi-grava-ae3(c-linha, pp1-rowid).
                END.
                WHEN  "PP2" THEN DO:
                    /*IF c-estado <> "PP1" AND c-estado <> "PP2" AND c-estado <> "AE3" THEN STOP.*/
                    c-estado = c-tipoRegistro.
                    RUN pi-grava-pp2(c-linha, pp1-rowid, OUTPUT pp2-rowid).
                END.
                WHEN  "PE4" THEN DO:
                    IF c-estado <> "PP2" AND c-estado <> "PE4" THEN STOP.
                    ASSIGN c-estado = c-tipoRegistro.
                    RUN pi-grava-pe4(c-linha, pp2-rowid). 
                END.
                WHEN "TE1" THEN DO:                    
                    /*IF c-estado <> "PP2" AND c-estado <> "PE4" AND c-estado <> "TE1" THEN STOP.
                    ASSIGN c-estado = c-tipoRegistro.*/
                    IF c-estado = "PP1" OR c-estado = "AE3" THEN
                        RUN pi-grava-te1(c-linha, pp1-rowid).
                    IF c-estado = "PP2" OR c-estado = "PE4"  OR c-estado = "TE1" THEN
                        RUN pi-grava-te1(c-linha, pp2-rowid).                    
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
    RETURN "OK".
END PROCEDURE.

PROCEDURE pi-limpa-tabelas:
    EMPTY TEMP-TABLE tt-itp.
    EMPTY TEMP-TABLE tt-PP1.
    EMPTY TEMP-TABLE tt-PP2.
    EMPTY TEMP-TABLE tt-AE3.
    EMPTY TEMP-TABLE tt-PE4.
END.

PROCEDURE pi-grava-itp:
    DEF INPUT PARAM c-linha AS CHAR NO-UNDO.
    txt:setText(c-linha).
    CREATE tt-itp.
    ASSIGN tt-itp.id-rnd         = txt:getIntSlice(4, 3)
           tt-itp.versao-rnd     = txt:getIntSlice(7, 2)
           tt-itp.num-controle   = txt:getIntSlice(9, 5)
           tt-itp.id-movimento   = txt:getChrSlice(14, 12)
           tt-itp.id-transmis    = txt:getChrSlice(26, 14)
           tt-itp.ident-receptor = txt:getChrSlice(40, 14)
           tt-itp.cod-transmis   = txt:getChrSlice(54, 8)
           tt-itp.cod-receptor   = txt:getChrSlice(62, 8)
           tt-itp.nome-transmis  = txt:getChrSlice(70, 25)
           tt-itp.nome-receptor  = txt:getChrSlice(95, 25).
END PROCEDURE.                                     

PROCEDURE pi-grava-pp1:
    DEF INPUT  PARAM c-linha   AS CHAR  NO-UNDO.
    DEF OUTPUT PARAM pp1-rowid AS ROWID NO-UNDO.
    txt:setText(c-linha).
    CREATE tt-pp1.
    ASSIGN tt-pp1.qtd-itens          = txt:getIntSlice(4, 4)
           tt-pp1.qtd-decimais       = txt:getIntSlice(8, 1)
           tt-pp1.numero-pedido      = txt:getChrSlice(9, 12)
           tt-pp1.data-emissao       = txt:getDateSlice(21, 6)
           tt-pp1.id-versao-pedido   = txt:getChrSlice(27, 12)
           tt-pp1.dt-inicio-pedido   = txt:getDateSlice(39, 6)
           tt-pp1.id-contato         = txt:getChrSlice(45, 11)
           tt-pp1.via-transporte     = txt:getChrSlice(56, 3)
           tt-pp1.condicao-frete     = txt:getChrSlice(59, 3)
           tt-pp1.cod-unidade-moeda  = txt:getChrSlice(62, 3)
           tt-pp1.cod-cond-pagto     = txt:getChrSlice(65, 4)
           tt-pp1.percent-desconto   = txt:getDecSlice(69, 4) / 100.
    ASSIGN pp1-rowid = ROWID(tt-pp1).
END.

PROCEDURE pi-grava-pp2:
    DEF INPUT PARAM c-linha    AS CHAR NO-UNDO.
    DEF INPUT PARAM pp1-rowid  AS ROWID NO-UNDO.
    DEF OUTPUT PARAM pp2-rowid AS ROWID NO-UNDO.
    txt:setText(c-linha).
    CREATE tt-pp2.
    ASSIGN tt-pp2.codigo-refer      = txt:getChrSlice(4, 30)
           tt-pp2.qtd-entrega       = txt:getIntSlice(34, 9)
           tt-pp2.descricao         = txt:getChrSlice(43, 25)
           tt-pp2.valor-unitario    = txt:getDecSlice(68, 12) / 100000
           tt-pp2.cod-lista-preco   = txt:getChrSlice(80, 6)
           tt-pp2.numero-item       = txt:getIntSlice(86, 4) 
           tt-pp2.data-entrega      = txt:getDateSlice(90, 6)
           tt-pp2.cod-class-fiscal  = txt:getIntSlice(96, 10)
           tt-pp2.aliquota-ipi      = txt:getDecSlice(106, 4) / 100
           tt-pp2.percent-desconto  = txt:getDecSlice(110, 4) / 100
           tt-pp2.cod-cond-pagto    = txt:getChrSlice(114, 4)
           tt-pp2.r-rowid = pp1-rowid.
    ASSIGN pp2-rowid = ROWID(tt-pp2).
END.

PROCEDURE pi-grava-ae3:
    DEF INPUT PARAM c-linha AS CHAR NO-UNDO.
    DEF INPUT PARAM pp1-rowid AS ROWID NO-UNDO.
    txt:setText(c-linha).
    CREATE tt-ae3.
    ASSIGN tt-ae3.local-faturamento = txt:getIntSlice(4, 14)
           tt-ae3.local-cobranca    = txt:getIntSlice(18, 14)
           tt-ae3.local-entrega     = txt:getIntSlice(32, 14)
           tt-ae3.tipo-transporte   = txt:getChrSlice(46, 4)
           tt-ae3.local-vai-vem     = txt:getChrSlice(50, 14)
           tt-ae3.fluxo-carro       = txt:getChrSlice(64, 1)
           tt-ae3.r-rowid = pp1-rowid.
END PROCEDURE.

PROCEDURE pi-grava-pe4:
    DEF INPUT PARAM c-linha AS CHAR NO-UNDO.
    DEF INPUT PARAM pp2-rowid AS ROWID NO-UNDO.
    txt:setText(c-linha).
    CREATE tt-pe4.
    ASSIGN tt-pe4.embalagem2-cli    = txt:getChrSlice(4, 30)
           tt-pe4.embalagem2-for    = txt:getChrSlice(34, 30)
           tt-pe4.capac-embalagem2  = txt:getDecSlice(64, 12) / 1000
           tt-pe4.embalagem1-for    = txt:getChrSlice(76,  30)
           tt-pe4.capac-embalagem1  = txt:getDecSlice(106, 12) / 1000
           tt-pe4.respons-embalagem = txt:getChrSlice(118, 1)
           tt-pe4.r-rowid = pp2-rowid.
END PROCEDURE.

PROCEDURE pi-grava-te1:
    DEF INPUT PARAM c-linha AS CHAR NO-UNDO.
    DEF INPUT PARAM r-rowid AS ROWID NO-UNDO.

    txt:setText(c-linha).

    FIND FIRST tt-te1 
        WHERE tt-te1.r-rowid = r-rowid NO-ERROR.
    IF NOT AVAIL tt-te1 THEN DO:
        CREATE tt-te1.
        ASSIGN tt-te1.r-rowid = r-rowid
               tt-te1.txt-informa = ''.
    END.
    ASSIGN tt-te1.txt-informa = tt-te1.txt-informa + txt:getChrSlice(4, 120) + ' '.
END PROCEDURE.
