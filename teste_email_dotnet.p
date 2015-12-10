USING System.Net.Mail.*.

DEF VAR client AS CLASS SmtpClient.
DEF VAR mail AS CLASS MailMessage.

DEF VAR to_   AS CLASS MailAddress.
DEF VAR from_ AS CLASS MailAddress.

DEF VAR anexo AS CLASS Attachment.

from_ = NEW MailAddress('no-reply@ccstec.com.br', 'Email automatico CCS').
to_   = NEW MailAddress('daniel@ccstec.com.br').

mail = NEW MailMessage (from_, to_).
mail:Subject = 'Teste email dotNet OpenEdge'.
mail:Body = 'TESTE' + CHR(13) + CHR(10) + 'TESTE'.

/*
cc = NEW MailAddress('auditoria@ccstec.com.br').
mail:CC:ADD(cc).
*/
/*
anexo = NEW ATTACHMENT( 'c:\temp\file.xlsx' ).
mail:Attachments:ADD(anexo).
*/

client = NEW SmtpClient(server_smtp).
client:SEND(mail).
