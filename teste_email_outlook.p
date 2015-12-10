DEF VAR outlook AS COM-HANDLE NO-UNDO.
DEF VAR mail    AS COM-HANDLE NO-UNDO.
DEF VAR assinatura AS CHAR NO-UNDO.

main_prg:
DO 
    ON ERROR UNDO, LEAVE
    ON STOP  UNDO, LEAVE:
    
    CREATE "Outlook.Application" outlook.
    
    mail = outlook:CreateItem(0). /* 0 = Email */
    mail:TO = '"Daniel R Gullo" <daniel@ccstec.com.br>'.
    mail:cc = ''.
    mail:bcc = ''.
    mail:Subject = 'Teste de e-mail pelo sistema'.
    ASSIGN assinatura = mail:body.
    mail:body = 'Teste' + CHR(13) + CHR(10) + 'Teste de e-mail pelo sistema ' + assinatura .
    
    /*mail:Attachments:ADD("C:\temp\teste.txt").*/
    
    mail:SEND().

END.

RELEASE OBJECT mail.
RELEASE OBJECT outlook.
ASSIGN mail = ?
       outlook = ?.
