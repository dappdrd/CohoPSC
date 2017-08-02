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
#for data frame manipulation
library(plyr)
#For data frame manipulation
library(reshape2)
#For prepping html tables
library(knitr)
library(kableExtra)

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
      actionButton("DataProcessButton",label = "Process data"),
      selectInput("graphic", "Please choose a figure to display",
                  choices = c("None", "Figure 4.1", "Figure 4.2", "Figure 4.3", "Figure 5.1")),
    
      selectInput("table", "Please choose a table to display",
                  choices = c("None","Table 4.1", "Table 4.2", "Testing Table")),
      
      textInput("EmailAdd", "Email to send to:", ""),
      actionButton("EmailButton",label = "Send data to my email")
    ),
    
    #Main panel is for graphics
    mainPanel(
      #outputs a plot
      plotOutput("Plot1"),
      
      
      ####
      dataTableOutput("Table")
    )
  )
  
)
