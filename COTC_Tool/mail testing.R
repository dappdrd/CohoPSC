library(sendmailR)

SendMail <- function(from="derek.dapp.dfw@gmail.com",to="derek.dapp@dfw.wa.gov",text=list("hey there","hi"),subject="Sag Hallo",smtp="smtp.my.server.de",user="me.myself.and.i",pw="123"){
  require(stringr)
  part1 <- "Const cdoSendUsingPickup = 1 'Send message using the local SMTP service pickup directory. 
  Const cdoSendUsingPort = 2 'Send the message using the network (SMTP over the network). 
  Const cdoAnonymous = 0 'Do not authenticate
  Const cdoBasic = 1 'basic (clear-text) authentication 
  Const cdoNTLM = 2 'NTLM "
  
  part2 <- paste(paste("Set objMessage = CreateObject(",'"',"CDO.Message",'"',")" ,sep=""),
                 paste("objMessage.Subject = ",'"',subject,'"',sep=""),
                 paste("objMessage.From = ",'"',from,'"',sep=""),
                 paste("objMessage.To = ",'"',to,'"',sep=""),
                 paste("objMessage.TextBody = ",'"',text,'"',sep=""),
                 sep="\n")
  
  part3 <- paste(
    "'==This section provides the configuration information for the remote SMTP server. 
    
    objMessage.Configuration.Fields.Item _ 
    (\"http://schemas.microsoft.com/cdo/configuration/sendusing\") = 2
    
    'Name or IP of Remote SMTP Server 
    objMessage.Configuration.Fields.Item _ 
    (\"http://schemas.microsoft.com/cdo/configuration/smtpserver\") = ",'"',smtp,'"'," 
    
    'Type of authentication, NONE, Basic (Base64 encoded), NTLM 
    objMessage.Configuration.Fields.Item _ 
    (\"http://schemas.microsoft.com/cdo/configuration/smtpauthenticate\") = cdoBasic 
    
    'Your UserID on the SMTP server 
    objMessage.Configuration.Fields.Item _ 
    (\"http://schemas.microsoft.com/cdo/configuration/sendusername\") = ",'"',user,'"'," 
    
    'Your password on the SMTP server 
    objMessage.Configuration.Fields.Item _ 
    (\"http://schemas.microsoft.com/cdo/configuration/sendpassword\") = ",'"',pw,'"', "
    
    'Server port (typically 25) 
    objMessage.Configuration.Fields.Item _ 
    (\"http://schemas.microsoft.com/cdo/configuration/smtpserverport\") = 465
    
    'Use SSL for the connection (False or True) 
    objMessage.Configuration.Fields.Item _ 
    (\"http://schemas.microsoft.com/cdo/configuration/smtpusessl\") = TRUE 
    
    'Connection Timeout in seconds (the maximum time CDO will try to establish a connection to the SMTP server) 
    objMessage.Configuration.Fields.Item _ 
    (\"http://schemas.microsoft.com/cdo/configuration/smtpconnectiontimeout\") = 60 
    objMessage.Configuration.Fields.Update
    
    '==End remote SMTP server configuration section== 
    
    objMessage.Send 
    ",sep="")
  
  vbsscript <- paste(part1,part2,part3,sep="\n\n\n")
  str_split(vbsscript,"\n")
  writeLines(vbsscript, "sendmail.vbs")
  shell("sendmail.vbs")
  unlink("sendmail.vbs")
}


TEST <<- data.frame(Test = 1, Test2 = 2)
attachementObject <- mime_part(x = TEST, name = "Test.csv")
MainText <- "Oh hi there"
bodyWithAttachement <- list(MainText, attachementObject)


SendMail(
  from="derek.dapp.dfw@gmail.com",
  to="derek.r.dapp@gmail.com",
  text=bodyWithAttachement,   
  subject="readThis",
  smtp="smtp.gmail.com",
  user="derek.dapp.dfw@gmail.com",
  pw="FakePass2"
)


























library(mailR)
send.mail(from = "derek.dapp.dfw@gmail.com",
          to = "derek.r.dapp@gmail.com",
          subject = "Subject of the email",
          body = "Body of the email",
          smtp = list(host.name = "smtp.gmail.com", port = 465, user.name = "derek.dapp.dfw@gmail.com", passwd = "FakePass2", ssl = TRUE),
          authenticate = TRUE,
          send = TRUE,
          attach.files = c("./Chinook.jpg", "./test.xlsx"),
          # file.names = c("Download log.log", "Upload log.log", "DropBox File.rtf"), # optional parameter
          file.descriptions = c("Random picture", "text excel file"), # optional parameter
          debug = TRUE)

