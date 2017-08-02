#used for sending emails
library(mailR)
#used for connecting to db
library(RODBC)
#used for plotting
library(ggplot2)
#used for shiny
library(shiny)
library(shinyFiles)
#for drop box interfacing
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

Password <<- "rjdio"

BlankDF <<- data.frame(Stock = as.character())

FinalRunList <<- c("BASE.Cmd", "CB86.Cmd", "CB87.Cmd", "CB88.Cmd", "CB89.Cmd", "CB90.Cmd", "CB91.Cmd", "CB92.Cmd", "CB93.Cmd", "CB94.Cmd", "CB95.Cmd",
                  "CB96.Cmd", "CB97.Cmd", "BK98 w UF H&W", "BK99 w UF H&W", "BK00 w UF H&W", "BK01 w UF H&W Reload Catches", "BK02 w UF H&W Reload catches",
                  "BK03 w catches and BC cohorts", "BK04 w catches BCcohorts", "BK05 w catches and BC cohorts", "BK06 w catches and BC cohorts", 
                  "BK07 catches and BC cohorts", "BK08.cmd", "BK09 New CNR", "bk10PSCFeb14", "Coho2011Post_PSC 2013", "Coho2012Post_PSC SSNPx2 Q Aug 22 BC MSF corrected",
                  "bk 2013 Feb 11 2015 adjust GB recruits", "bc-BK2014 w TAMM inputs final#2", "bc-bkCoho2015 Final")

#This sets up a stock DF that has all the necessary stock information to be used later
#LAC = Low Abundance Cohort; UAC = Upper Abundance Cohort; LAMO = Low Abundance Management Objective (cap); MAMO = Moderate Abundance Management Objective,
#AAMO = Abundant Abundance Management Objective
SkagitRows <- data.frame(PSCStock = 1, FRAMWildStocks = c(17,18,23,24), StockName = "Skagit",LAC = 22857, UAC = 62500, Cap.Meth = "imu", LAMO = .2, MAMO = .35, AAMO = .6, LEG = NA, UEG = NA, MU = "US Inside")
StillyRows <- data.frame(PSCStock = 2, FRAMWildStocks = c(29,30), StockName = "Stillaguamish",LAC = 9385, UAC = 20000, Cap.Meth = "imu", LAMO = .2, MAMO = .35, AAMO = .5, LEG = NA, UEG = NA, MU = "US Inside")
SnohomishRows <- data.frame(PSCStock = 3, FRAMWildStocks = c(35,36), StockName = "Snohomish",LAC = 51667, UAC = 125000, Cap.Meth = "imu", LAMO = .2, MAMO = .4, AAMO = .6, LEG = NA, UEG = NA, MU = "US Inside")
HoodCanalRows <- data.frame(PSCStock = 4, FRAMWildStocks = c(45,46,55,56,59,60), StockName = "Hood Canal",LAC = 19545, UAC = 41000, Cap.Meth = "imu", LAMO = .2, MAMO = .45, AAMO = .65, LEG = NA, UEG = NA, MU = "US Inside")
JDFRows <- data.frame(PSCStock = 5, FRAMWildStocks = c(115,116,117,118), StockName = "US Strait JDF",LAC = 11679, UAC = 27445, Cap.Meth = "imu", LAMO = .2, MAMO = .4, AAMO = .6, LEG = NA, UEG = NA, MU = "US Inside")

QuilRows <- data.frame(PSCStock = 6, FRAMWildStocks = c(131, 132), StockName = "Quillayute",LAC = 7875, UAC = 10500, Cap.Meth = "omu", LAMO = .2, MAMO = NA, AAMO = NA, LEG = 6300, UEG = 15800, MU = "US Outside")
HohRows <- data.frame(PSCStock = 7, FRAMWildStocks = c(135, 136), StockName = "Hoh",LAC = 2500, UAC = 3333, Cap.Meth = "omu", LAMO = .2, MAMO = NA, AAMO = NA, LEG = 2000, UEG = 5000, MU = "US Outside")
QueetsRows <- data.frame(PSCStock = 8, FRAMWildStocks = c(139,140), StockName = "Queets",LAC = 7250, UAC = 9667, Cap.Meth = "omu", LAMO = .2, MAMO = NA, AAMO = NA, LEG = 5800, UEG = 14500, MU = "US Outside")
GraysHarbRows <- data.frame(PSCStock = 9, FRAMWildStocks = c(149, 150, 153, 154, 157, 158), StockName = "Grays Harbor",LAC = 44250, UAC = 59000, Cap.Meth = "omu", LAMO = NA, MAMO = NA, AAMO = .6, LEG = 35400, UEG = 35400, MU = "US Outside")

LowFRRows <- data.frame(PSCStock = 10, FRAMWildStocks = c(227, 228), StockName = "Lower Fraser",LAC = NA, UAC = NA, Cap.Meth = NA, LAMO = NA, MAMO = NA, AAMO = NA, LEG = NA, UEG = NA, MU = "Canada")
IntFRRows <- data.frame(PSCStock = 11, FRAMWildStocks = c(231, 232), StockName = "Interior Fraser",LAC = NA, UAC = NA, Cap.Meth = "fixed", LAMO = .2, MAMO = NA, AAMO = NA, LEG = NA, UEG = NA, MU = "Canada")
GeoStMLRows <- data.frame(PSCStock = 12, FRAMWildStocks = c(207,208), StockName = "Georgia Strait ML",LAC = NA, UAC = NA, Cap.Meth = NA, LAMO = NA, MAMO = NA, AAMO = NA, LEG = NA, UEG = NA, MU = "Canada")
GeoStVIRows <- data.frame(PSCStock = 13, FRAMWildStocks = c(211, 212), StockName = "Georgia Strait VI",LAC = NA, UAC = NA, Cap.Meth = NA, LAMO = NA, MAMO = NA, AAMO = NA, LEG = NA, UEG = NA, MU = "Canada")

StockDF <<- rbind(SkagitRows, StillyRows, SnohomishRows, HoodCanalRows, JDFRows, QuilRows, HohRows, QueetsRows, GraysHarbRows, LowFRRows, IntFRRows, GeoStMLRows, GeoStVIRows)


#List of Stocks
StockList <<- as.character(unique(StockDF$StockName))

test <<- data.frame(Test = 1, test2 = 4)

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
       
       withProgress(message = 'Sending Email', value = 0, {
         incProgress(1/1, detail = "Attaching Files")
          #sends a mail from my dummy email address.
          send.mail(from = "derek.dapp.dfw@gmail.com",
                 to = input$EmailAdd,
                 subject = "Coho PSC Report Data",
                 body = "Please see the attached for the PSC report files.  Note that tables are encoded as html files.  These can be converted to tables in a PDF or word format by using an online converter.",
                 smtp = list(host.name = "smtp.gmail.com", port = 465, user.name = "derek.dapp.dfw@gmail.com", passwd = "FakePass2", ssl = TRUE),
                 authenticate = TRUE,
                 send = TRUE,
                 attach.files = c("https://dl.dropboxusercontent.com/s/jti9io6bufr0k4j/Figure%204.1.jpg",
                                  "https://dl.dropboxusercontent.com/s/2twmtoi8m1sur4r/Figure%204.2.jpg",
                                  "https://dl.dropboxusercontent.com/s/9t2ozw5ir86covl/Figure%204.3.jpg", 
                                  "https://dl.dropboxusercontent.com/s/3cmw8yda3677ls5/Table%204.1%20html.txt",
                                  "https://dl.dropboxusercontent.com/s/cih9ma0vtly8a72/Table%204.2%20html.txt",
                                  "https://dl.dropboxusercontent.com/s/4g48qbf6kstpb6d/Figure%205.1.jpg"),
                 file.descriptions = c("Figure 4.1", "Figure 4.2", "Figure 4.3", "Table 4.1","Table 4.2", "Figure 5.1"), # optional parameter
                 debug = TRUE)
       })
     })
   })
   
   observe({
     #If the button hasn't been pressed or the pass is wrong do nothing
     if (input$DataProcessButton == 0 | input$PasswordAdd != Password)
       return(NULL)
     isolate({
       withProgress(message = 'Loading Data', value = 0, {
          #Grabs FRAM RunID Table
          
          incProgress(1/4, detail = "Loading RunID Table")
         
          RunIDTab = read.csv("https://dl.dropboxusercontent.com/s/ntgampj1d8dyp26/RunList.csv?dl=1")
       
          #removes useless columns in the data
          Drops <- c("PrimaryKey", "SpeciesName", "RunTitle",
                  "RunComments", "CreationDate", "ModifyInputDate", "RunTimeDate")
          RunIDTab<- RunIDTab[ , !(names(RunIDTab) %in% Drops)]
       
       
       
       
          incProgress(2/4, detail = "Loading Escapement Table")
          #Grabs FRAM Escapement Table
          EscTab = read.csv("https://dl.dropboxusercontent.com/s/y5f8kx4v4pxnnrh/Escapement.csv?dl=1")
       
          #Adds run info to EscTab
          EscTab <- merge(EscTab, RunIDTab, by= "RunID")
       
          #Convert integers to characters
          EscTab$RunID <- as.character(EscTab$RunID)
          EscTab$RunYear <- as.character(EscTab$RunYear)
          EscTab$StockID <- as.character(EscTab$StockID)
          EscTab$Age <- as.character(EscTab$Age)
          EscTab$TimeStep <- as.character(EscTab$TimeStep)
          EscTab$PrimaryKey <- as.character(EscTab$PrimaryKey)
          EscTab$BasePeriodID <- as.character(EscTab$BasePeriodID)
       
       
       
          incProgress(3/4, detail = "Loading Mortality Table - this may take a few minutes")       
          #Grab FRAM Mortality Table
          MortTab = read.csv("https://dl.dropboxusercontent.com/s/w0qv3o4bqro7o31/Mortality.csv?dl=1")
       
          #Get summarized Mortalities
          MortTab$TotMort <- MortTab$LandedCatch + MortTab$NonRetention + MortTab$Shaker + MortTab$DropOff + MortTab$MSFLandedCatch + MortTab$MSFNonRetention + MortTab$MSFShaker + MortTab$MSFDropOff
       
          #Remove columns no longer of interest
          Keeps <- c("RunID", "StockID", "Age", "FisheryID", "TimeStep", "TotMort")
          MortTab <- MortTab[, (names(MortTab) %in% Keeps)]
       
          #Adds run info to Mort Tab
          MortTab <- merge(MortTab, RunIDTab, by= "RunID")
       
          #Convert integers to characters
          MortTab$RunID <- as.character(MortTab$RunID)
          MortTab$RunYear <- as.character(MortTab$RunYear)
          MortTab$StockID <- as.character(MortTab$StockID)
          MortTab$Age <- as.character(MortTab$Age)
          MortTab$TimeStep <- as.character(MortTab$TimeStep)
          MortTab$BasePeriodID <- as.character(MortTab$BasePeriodID)
          MortTab$FisheryID <- as.character(MortTab$FisheryID)
       
       
       
          #This is the Esc DF to which all ESC data is saved
          MainEscDF <- data.frame(RunYear = as.character(), Escapement= as.double(), Stock = as.character())
          #This is the Mort DF to which all mort data is saved
          MainMortDF <- data.frame(RunYear = as.character(), TotMort= as.double(), Stock = as.character())
          
          #Sets up table 4.1
          Tab41DF <<- data.frame(Management.Unit = as.character(), Low.Moderate.Abundance.Category = as.integer(), Moderate.Abundant.Abundance.Category = as.integer())
          
          #Sets up table 4.2
          Tab42DF <<- data.frame(Management.Unit = as.character(), Escapement.Goal.Range = as.character(), Low.Moderate.Abundance.Category = as.integer(), Moderate.Abundant.Abundance.Category = as.integer())
          
       })
       
       withProgress(message = 'Processing Data/Preparing figures', value = 0, {
          incProgress(1/2, detail = "Processing Data")
          #This is the main processing loop (gets escapements, cohorts, fishery mortality by stock)
          for(i in 1:length(StockList)){
         
         
            # Subsets the stock DF to get the stock of interest
            SubStockDF <- subset(StockDF, StockName == StockList[i])
         
            #Fram stock list
            FRAMStks <- unique(SubStockDF$FRAMWildStocks)
         
            # Subsets escapement DF to get the stock of interest
            SubEscDF <- subset(EscTab, StockID %in% FRAMStks)
         
            StockEscRows <- ddply(SubEscDF, "RunYear",  numcolwise(sum))
         
            StockEscRows$Stock <- StockList[i]
         
            MainEscDF <- rbind(MainEscDF, StockEscRows)
         
         
            # Subsets Mortality DF to get the stock of interest
            SubMortDF <- subset(MortTab, StockID %in% FRAMStks)
         
            StockMortRows <- ddply(SubMortDF, "RunYear",  numcolwise(sum))
            StockMortRows$Stock <- StockList[i]
            
            #Subsets Mortality DF to get stock/only SUS fisheries
            SubMortDFSUS <- subset(MortTab, StockID %in% FRAMStks & FisheryID < 167)
            
            #Gets the column number with mortalities in it, renames it to SUS Mort
            ColIndex <- which( colnames(SubMortDFSUS)=="TotMort" )
            colnames(SubMortDFSUS)[ColIndex] <- "SUSMort"
            
            #Sums everything by run year
            StockMortSUSRows <- ddply(SubMortDFSUS, "RunYear",  numcolwise(sum))
            
            #Subsets Mortality DF to get stock/only CA fisheries
            SubMortDFCA <- subset(MortTab, StockID %in% FRAMStks & FisheryID > 166 & FisheryID < 194)
            
            #Gets the column number with mortalities in it, renames it to CA Mort
            ColIndex <- which( colnames(SubMortDFCA)=="TotMort" )
            colnames(SubMortDFCA)[ColIndex] <- "CAMort"
            
            StockMortCARows <- ddply(SubMortDFCA, "RunYear",  numcolwise(sum))
            
            #Subsets Mortality DF to get stock/only AK fisheries
            SubMortDFAK <- subset(MortTab, StockID %in% FRAMStks & FisheryID > 193)
            
            #Gets the column number with mortalities in it, renames it to CA Mort
            ColIndex <- which( colnames(SubMortDFAK)=="TotMort" )
            colnames(SubMortDFAK)[ColIndex] <- "AKMort"
            
            StockMortAKRows <- ddply(SubMortDFAK, "RunYear",  numcolwise(sum))
            
            #Merge in SUS,CA, AK
            StockMortRows <- merge(StockMortRows, StockMortSUSRows, by= "RunYear")
            StockMortRows <- merge(StockMortRows, StockMortCARows, by= "RunYear")
            StockMortRows <- merge(StockMortRows, StockMortAKRows, by= "RunYear")
            
         
            MainMortDF <- rbind(MainMortDF, StockMortRows)
            
            if(SubStockDF$MU[1] == "US Inside"){
              tab41Row <- data.frame(Management.Unit = SubStockDF$StockName[1], Low.Moderate.Abundance.Category = SubStockDF$LAC[1], Moderate.Abundant.Abundance.Category = SubStockDF$UAC[1])
              Tab41DF <<- rbind(Tab41DF, tab41Row)
            }
            if(SubStockDF$MU[1] == "US Outside"){
              if(SubStockDF$StockName[1] == "Grays Harbor"){
                ESCGoal = as.character(SubStockDF$LEG[1])
              }
              else{
                ESCGoal = paste(SubStockDF$LEG[1],"-", SubStockDF$UEG[1])
              }
              
              tab42Row <- data.frame(Management.Unit = SubStockDF$StockName[1],Escapement.Goal.Range = ESCGoal, 
                                     Low.Moderate.Abundance.Category = SubStockDF$LAC[1], Moderate.Abundant.Abundance.Category = SubStockDF$UAC[1])
              Tab42DF <<- rbind(Tab42DF, tab42Row)
            }
         
         
          }
       
          #Merging data frames
          MainDataDF <<- merge(MainEscDF, MainMortDF, by= c("Stock","RunYear"))
       
          #Ocean Cohort = Escapement + Mortality
          MainDataDF$OceanCohort <- MainDataDF$Escapement + MainDataDF$TotMort
       
          #Ocean Cohort Figures (4.1, 4.2, 4.3)
          incProgress(1/2, detail = "Preparing figures")
       
          for (i in 1:3){
            if (i == 1){
              FigStocks <- c("Lower Fraser", "Interior Fraser", "Georgia Strait ML", "Georgia Strait VI")
           
              FigName <- "Figure 4.1.jpg"
           
              FigDF <- subset(MainDataDF, Stock %in% FigStocks)
           
              #Excludes 1998-2003 from the figure
              FigDF$OceanCohort[FigDF$RunYear %in% c("1998","1999","2000","2001","2002","2003")] <- NA
           
              FigDF$OceanCohortThous <- FigDF$OceanCohort/1000
           
              Fig41 <<- ggplot(FigDF, aes(x=RunYear, y = OceanCohortThous, group = Stock, linetype = Stock, colour = Stock, size = Stock))+
                geom_line() + theme_bw()+
                theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                   panel.background = element_blank(), axis.line = element_line(colour = "black")) +
                scale_color_manual(values=c("black", "black", "black", "gray", "gray"))+
                scale_linetype_manual(values=c("solid", "dashed", "dotted", "solid", "dashed"))+
                scale_size_manual(values=c(1.2, 1.2, 1.2, 1.2, 1.2))+
                theme(text = element_text(size=16))+
                xlab('Catch Year') + ylab('Cohort Size (thousands)') +
                theme(axis.text.x = element_text(angle = 90, hjust = 1))+
                guides(colour = guide_legend(override.aes = list(size=.6)))
           
              filePath <- file.path(tempdir(), FigName)
              ggsave(Fig41, file = filePath, width = 12, height = 6)
           
              drop_upload(filePath)
           
            }
            if (i == 2){
              FigStocks <- c("Skagit", "Stillaguamish", "Snohomish", "Hood Canal", "US Strait JDF")
           
              FigName <- "Figure 4.2.jpg"
           
              FigDF <- subset(MainDataDF, Stock %in% FigStocks)
           
              FigDF$OceanCohortThous <- FigDF$OceanCohort/1000
           
              Fig42 <<- ggplot(FigDF, aes(x=RunYear, y = OceanCohortThous, group = Stock, linetype = Stock, colour = Stock, size = Stock))+
                geom_line() + theme_bw()+
                theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                   panel.background = element_blank(), axis.line = element_line(colour = "black")) +
                scale_color_manual(values=c("black", "black", "black", "gray", "gray"))+
                scale_linetype_manual(values=c("solid", "dashed", "dotted", "solid", "dashed"))+
                scale_size_manual(values=c(1.2, 1.2, 1.2, 1.2, 1.2))+
                theme(text = element_text(size=16))+
                xlab('Catch Year') + ylab('Cohort Size (thousands)') +
                theme(axis.text.x = element_text(angle = 90, hjust = 1))+
                guides(colour = guide_legend(override.aes = list(size=.6)))
           
              filePath <- file.path(tempdir(), FigName)
              ggsave(Fig42, file = filePath, width = 12, height = 6)
           
              drop_upload(filePath)
            }
            if (i == 3){
              FigStocks <- c("Quillayute", "Hoh", "Queets", "Grays Harbor")
           
              FigName <- "Figure 4.3.jpg"
           
              FigDF <- subset(MainDataDF, Stock %in% FigStocks)
           
              FigDF$OceanCohortThous <- FigDF$OceanCohort/1000
           
              Fig43 <<- ggplot(FigDF, aes(x=RunYear, y = OceanCohortThous, group = Stock, linetype = Stock, colour = Stock, size = Stock))+
                geom_line() + theme_bw()+
                theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                   panel.background = element_blank(), axis.line = element_line(colour = "black")) +
                scale_color_manual(values=c("black", "black", "black", "gray", "gray"))+
                scale_linetype_manual(values=c("solid", "dashed", "dotted", "solid", "dashed"))+
                scale_size_manual(values=c(1.2, 1.2, 1.2, 1.2, 1.2))+
                theme(text = element_text(size=16))+
                xlab('Catch Year') + ylab('Cohort Size (thousands)') +
                theme(axis.text.x = element_text(angle = 90, hjust = 1))+
                guides(colour = guide_legend(override.aes = list(size=.6)))
           
              filePath <- file.path(tempdir(), FigName)
              ggsave(Fig43, file = filePath, width = 12, height = 6)
           
              drop_upload(filePath)
            }
          }
          
          #Preparing figure 5.1 (total fishery mortality of all management units combined, by CA and US)
          Fig51DF <- ddply(MainDataDF, "RunYear",  numcolwise(sum))
          Fig51DF$USMort <- Fig51DF$SUSMort + Fig51DF$AKMort
          
          #Reorganizes the data for plotting
          Fig51DF <- melt(Fig51DF, id.vars = "RunYear", measure.vars = c("CAMort", "USMort"))
          
          Fig51 <<- ggplot(Fig51DF, aes(x=RunYear, y = (value/1000000),  size = variable, linetype = variable, group = variable))+
            geom_line() + theme_bw()+
            theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                  panel.background = element_blank(), axis.line = element_line(colour = "black")) +
            scale_linetype_manual(values=c("dashed", "solid"), name = "", labels = c("Canada", "US"))+
            scale_size_manual(values=c(1.2, 1.2), name="", label=c("Canada", "US"))+
            theme(text = element_text(size=16))+
            xlab('Catch Year') + ylab('Total Mortality (Millions)') +
            theme(axis.text.x = element_text(angle = 90, hjust = 1))+
            guides(linetype = guide_legend(override.aes = list(size=.6)))
          
          filePath <- file.path(tempdir(), "Figure 5.1.jpg")
          ggsave(Fig51, file = filePath, width = 12, height = 6)
          
          drop_upload(filePath)
          
          #Options added - can be changed to Latex but not PDF
          options(knitr.table.format = "html") 
          
          #Rename column headers
          ColIndex <- which(colnames(Tab41DF)=="Management.Unit" )
          colnames(Tab41DF)[ColIndex] <- "Management Unit"
          ColIndex <- which(colnames(Tab41DF)=="Low.Moderate.Abundance.Category" )
          colnames(Tab41DF)[ColIndex] <- "Low/Moderate"
          ColIndex <- which(colnames(Tab41DF)=="Moderate.Abundant.Abundance.Category" )
          colnames(Tab41DF)[ColIndex] <- "Moderate/Abundant"
          
          Tab41DFhtml <- kable (Tab41DF) %>%
            kable_styling("bordered") %>%
            #add header rows
            add_header_above(c(" ", "Abundance Category Breakpoints" = 2), bold = T)
          
          
          ColIndex <- which(colnames(Tab42DF)=="Management.Unit" )
          colnames(Tab42DF)[ColIndex] <- "Management Unit"
          ColIndex <- which(colnames(Tab42DF)=="Escapement.Goal.Range" )
          colnames(Tab42DF)[ColIndex] <- "Escapement Goal/Range"
          ColIndex <- which(colnames(Tab42DF)=="Low.Moderate.Abundance.Category" )
          colnames(Tab42DF)[ColIndex] <- "Low/Moderate"
          ColIndex <- which(colnames(Tab42DF)=="Moderate.Abundant.Abundance.Category" )
          colnames(Tab42DF)[ColIndex] <- "Moderate/Abundant"
          
          Tab42DFhtml <- kable (Tab42DF) %>%
            kable_styling("bordered") %>%
            #add header rows
            add_header_above(c(" ", " ", "Abundance Category Breakpoints" = 2), bold = T)
          
          #upload tables 4.1, 4.2
          filePath <- file.path(tempdir(), "Table 4.1 html.txt")
          writeLines(Tab41DFhtml,filePath)
          
          drop_upload(filePath)
          
          filePath <- file.path(tempdir(), "Table 4.2 html.txt")
          writeLines(Tab42DFhtml,filePath)
          
          drop_upload(filePath)
          
       })
     })
   })
  
  
  #Plot1 refers to the plot in the ui.R, main panel section
  output$Plot1 <- renderPlot({
    #If the password is incorrect, display PSCLogo
    if(input$PasswordAdd != Password){
      plot_jpeg('PSCLogo.jpg')
    }
    
    
    #password is correct
    else {
      #This is set by the user in ui.R from the select input button in the side panel
      graphic <- input$graphic
      

      if (graphic == "None"){

        WelcomeText <- "Welcome to the PSC CoTC periodic report automation tool

        Please click the button to the left  to process data
        Data processing will take several minutes
        
        
        Rules for running this program:
        - Files on dropbox must be up-to-date
        - There must only be one run in the data base per run year
        
        Thanks for using the tool! 
        
        Please email Derek Dapp at derek.dapp@dfw.wa.gov
        If you have any questions related to the tool"
        par(mar = c(0,0,0,0))
        plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
        text(x = 0.5, y = 0.5, WelcomeText, 
             cex = 1.6, col = "black")
      }
      else if (graphic == "Figure 4.1"){
        Fig41
      }
      else if (graphic == "Figure 4.2"){
        Fig42
      }
      else if (graphic == "Figure 4.3"){
        Fig43
      }
      else if (graphic == "Figure 5.1"){
        Fig51
      }
    }
  })
  output$Table <- renderDataTable({
    #If the password is incorrect, display PSCLogo
    if(input$PasswordAdd != Password | input$table == "None"){
      BlankDF
    }
    else if (input$table == "Table 4.1"){
      Tab41DF
    }
    else if (input$table == "Table 4.2"){
      Tab42DF
    }
    else if (input$table == "Testing Table"){
      MainDataDF
    }
  }) 
}
  
)