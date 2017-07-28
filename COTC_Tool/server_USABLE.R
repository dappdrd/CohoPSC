#used for sending emails
library(mailR)
#used for connecting to db
library(RODBC)
#used for plotting
library(ggplot2)
#used for shiny
library(shiny)
library(shinyFiles)
library(rdrop2)

token <- readRDS("droptoken.rds")
drop_acc(dtoken = token)

Password <<- "rjdio"

shinyServer(
function(input, output, session){
  
  
  #Creates a function for plotting pictures
  plot_jpeg = function(path, add=FALSE)
  {
    require('jpeg')
    jpg = readJPEG(path, native=T) # read the file
    res = dim(jpg)[1:2] # get the resolution
    if (!add) # initialize an empty plot area if add==FALSE
      plot(1,1,xlim=c(1,res[1]),ylim=c(1,res[2]),asp=1,type='n',xaxs='i',yaxs='i',xaxt='n',yaxt='n',xlab='',ylab='',bty='n')
    rasterImage(jpg,1,1,res[1],res[2])
  }
  
   observe({
     # Take a dependency on input$EmailButton
     if (input$EmailButton == 0 | input$PasswordAdd != Password)
       return(NULL)
     # Use isolate() to avoid dependency on input$EmailButton
     isolate({
       
       #sends a mail from my dummy email address.
       send.mail(from = "derek.dapp.dfw@gmail.com",
                 to = input$EmailAdd,
                 subject = "Coho PSC Report Data",
                 body = "Please see the attached for the PSC report files",
                 smtp = list(host.name = "smtp.gmail.com", port = 465, user.name = "derek.dapp.dfw@gmail.com", passwd = "FakePass2", ssl = TRUE),
                 authenticate = TRUE,
                 send = TRUE,
                 attach.files = c("https://dl.dropboxusercontent.com/s/kpc9ipny5hhbvh3/Chinook.jpg", "https://dl.dropboxusercontent.com/s/67cekqliabs52ja/testfile.csv"),
                 file.descriptions = c("Random picture", "Random csv"), # optional parameter
                 debug = TRUE)
     })
   })
   
   observe({
     #If the button hasn't been pressed or the pass is wrong do nothing
     if (input$DataProcessButton == 0 | input$PasswordAdd != Password)
       return(NULL)
     isolate({
       
       #these 2 lines are for code testing purposes and mean nothing.
       test <<- data.frame(Test = 1, test2 = 2)
       write.csv(test, file = "MyData.csv")
       
       #fram.conn <- odbcConnectAccess("./FramVS2-PSC-Coho-Backwards-for 2015.mdb")
       #cohorttab <- sqlQuery(fram.conn, 'select * from Cohort')
       
     })
   })
  
  
  #Plot1 refers to the plot in the ui.R, main panel section
  output$Plot1 <- renderPlot({
    #If the password is incorrect, display PSCLogo
    if(input$PasswordAdd != Password){
      plot_jpeg('PSCLogo.jpg')
    }
    
    #password is correct
    else{
      #This is set by the user in ui.R from the select input button in the side panel
      Stock <- input$Stock
    
      plot(1, type="n", xlab="", ylab="", xlim=c(0, 2), ylim=c(0, 2))
      text(Stock, x = 1, y = 1)
    }
  })
}
  
)