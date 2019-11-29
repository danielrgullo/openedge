{C:/work/desenv/edi/tt-ediccs.i}

DEF VAR h-edi AS HANDLE NO-UNDO.

// CSV CNH
PROCEDURE pi-leArquivoCNH:
   DEFINE INPUT PARAMETER c-arquivo AS CHARACTER NO-UNDO.

   RUN c:/work/desenv/edi/csv/cnh.p PERSISTENT SET h-edi.
   RUN pi-processa-arquivo (c-arquivo).
END PROCEDURE.

// RND 012
PROCEDURE pi-leArquivoRND012:
    DEFINE INPUT PARAMETER c-arquivo AS CHARACTER NO-UNDO.

    RUN c:/work/desenv/edi/rnd/rnd12.p PERSISTENT SET h-edi.
    RUN pi-processa-arquivo (c-arquivo).
END PROCEDURE.

// RND 601
PROCEDURE pi-leArquivoRND601:
   DEFINE INPUT PARAMETER c-arquivo AS CHARACTER NO-UNDO.

   RUN c:/work/desenv/edi/rnd/rnd06.p PERSISTENT SET h-edi.
   RUN pi-processa-arquivo (c-arquivo).
END PROCEDURE.

// RND 001
PROCEDURE pi-leArquivoRND001:
    DEF INPUT PARAM c-arquivo AS CHAR NO-UNDO.

    RUN c:/work/desenv/edi/rnd/rnd01.p PERSISTENT SET h-edi.
    RUN pi-processa-arquivo (c-arquivo).
END PROCEDURE.

// le o arquivo e retorna tt's
PROCEDURE pi-processa-arquivo:
    DEF INPUT PARAM c-arquivo AS CHAR NO-UNDO.

    IF NOT VALID-HANDLE(h-edi) THEN DO:
        MESSAGE 'Erro inesperado!'
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        LEAVE.
    END.

    RUN le-EDI IN h-edi (c-arquivo).

    IF RETURN-VALUE = "NOK" THEN DO:

        MESSAGE "Erro lendo arquivo ou arquivo n∆o encontrado."
            VIEW-AS ALERT-BOX INFO BUTTONS OK.

    END. ELSE DO:
        
        RUN get-temp-tables in h-edi 
                   (OUTPUT TABLE tt-edi, 
                    OUTPUT TABLE tt-ped-edi,
                    OUTPUT TABLE tt-ped-edi-ent).

    END.
    
    DELETE PROCEDURE h-edi.
END PROCEDURE.


/**
 * TESTE
 *****************************************************************************/
/*
DEF VAR c-arquivo-edi AS CHARACTER NO-UNDO.

// ASSIGN c-arquivo-edi = '\\servcitrix\edi-rec-cat\20191029\ADM.0810.000001.RND00107_61064911000177_20191029080503_068.txt'.
// RUN pi-leArquivoRND001 (c-arquivo-edi).
   
// ASSIGN c-arquivo-edi = '\\server-dados\pcp\EDI CLIENTES\EDI JOHN DEERE\ARQUIVO DIR\2019\10 - OUTUBRO\190930194144249144-19242893789-JOHD-JOHNDEERECATALAO-CCST-CCSLIMEIRA-RND-00109-.DIR'.
// RUN pi-leArquivoRND001 (c-arquivo-edi).
   
// ASSIGN c-arquivo-edi = '\\server-dados\pcp\EDI CLIENTES\EDI VOLVO\ARQUIVO\2019\VCE PEDERNEIRAS (8499-34857) - 0131247502-AS-2 22102019.TXT'.
// RUN pi-leArquivoRND001 (c-arquivo-edi).
   
// ASSIGN c-arquivo-edi = '\\server-dados\pcp\EDI CLIENTES\EDI AGCO\Arquivo TXT\CONFERIDOS\CANOAS - RND-001-08-66756426.EDI'.
// RUN pi-leArquivoRND001 (c-arquivo-edi).
   
// ASSIGN c-arquivo-edi = '\\servcitrix\edi-rec-sintel\191029142038791473-19245175344-JACT-JACTOCENTRO-CCST-CCSLIMEIRA-RND-01206-.MDI'.
// RUN pi-leArquivoRND012 (c-arquivo-edi).
   
// ASSIGN c-arquivo-edi = '\\server-dados\pcp\EDI CLIENTES\EDI CNH\ARQUIVOS\CNH_MATERIAL_RELEASE_PI_BQP01_25_out_2019.csv'.
// RUN pi-leArquivoCNH (c-arquivo-edi).

FOR EACH tt-ped-edi-ent NO-LOCK:

    DISP tt-ped-edi-ent.id-movimento
         tt-ped-edi-ent.situacao.
END.


FOR EACH tt-ped-edi NO-LOCK:
        //WHERE tt-ped-edi.codigo-refer BEGINS '48173339':
    DISP tt-ped-edi
        WITH SCROLLABLE .
END.
*/
