#options(rsconnect.max.bundle.size=4000000000)
#options(shiny.maxRequestSize=4000000000)

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
library(later)
library(pool)
#library(rsconnect)
library(readxl)

#options(rsconnect.max.bundle.size=3145728000)
#options(shiny.maxRequestSize=3145728000)
# set instance size for an application
#configureApp("cotc_tool", size="3X-Large", account = "salmonid")
#options(java.parameters = "-Xss2560k")

token <- readRDS("droptoken_PSC_CoTC_Dropbox.rds")
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
      selectInput("TAMMAdd", "Use TAMM to update coastal stocks/catch?", choice = c("Yes - All Stocks", "Yes - Queets Only", "No")),
      #Adds a dropdown box
      actionButton("DataProcessButton",label = "Process Data/Create Figures & Tables"),
      actionButton("FTableButton",label = "Produce Appendix F Tables"),
      selectInput("graphic", "Please choose a figure to display",
                  choices = c("None", "Figure 4.1", "Figure 4.2", "Figure 4.3", "Figure 5.1", "Figure 7.1", "Figure 7.2", "Figure 7.3", "Figure 7.4", "Figure 7.5",
                              "Figure 7.6", "Figure 7.7", "Figure 7.8", "Figure 7.9", "Figure 7.10", "Figure 7.11", "Figure 7.12", "Figure 7.13")),
    
      selectInput("table", "Please choose a table to display",
                  choices = c("None","Table 4.1", "Table 4.2", "Table 6.1", "Table 6.2", "Table 6.3", "Table 9", "Table E - Lower Fraser",
                              "Table E - Interior Fraser", "Table E - St of Geo ML", "Table E - St of Geo VI", "Table E - Skagit",
                              "Table E - Stillaguamish", "Table E - Snohomish", "Table E - Hood Canal", "Table E - US JDF",
                              "Table E - Quillayute", "Table E - Hoh", "Table E - Queets","Table E - Grays Harbor","Table F - Lower Fraser",
                              "Table F - Interior Fraser", "Table F - St of Geo ML", "Table F - St of Geo VI", "Table F - Skagit",
                              "Table F - Stillaguamish", "Table F - Snohomish", "Table F - Hood Canal", "Table F - US JDF",
                              "Table F - Quillayute", "Table F - Hoh", "Table F - Queets", "Table F - Grays Harbor")),
      
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
