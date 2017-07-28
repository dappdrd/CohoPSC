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

shinyServer(
  #Sets up our page, this one has a header
  #Side bar (inputs)
  #Main panel (graphics)
  pageWithSidebar(
    #Adds a header at the top of the page
    headerPanel("Coho PSC Report Tool"),
    
    
    #This adds a side bar that we'll use to put inputs in
    sidebarPanel(
      
      textInput("PasswordAdd", "Please enter password to access program functions", ""),
      #Adds a dropdown box
      selectInput("Stock", "Please Select a Stock",
                  choices = c("Nooksack Springs", "Dungeness")),
    

      actionButton("DataProcessButton",label = "Process data"),
      textInput("EmailAdd", "Email to send to:", ""),
      actionButton("EmailButton",label = "Send data to my email")
    ),
    
    #Main panel is for graphics
    mainPanel(
      #outputs a plot
      plotOutput("Plot1")
    )
  )
  
)
