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
SkagitRows <- data.frame(PSCStock = 1, FRAMWildStocks = c(17,18,23,24), StockName = "Skagit",LAC = 22857, UAC = 62500, Cap.Meth = as.character("imu"), LAMO = .2, MAMO = .35, AAMO = .6, LEG = NA, UEG = NA, MU = "US Inside")
StillyRows <- data.frame(PSCStock = 2, FRAMWildStocks = c(29,30), StockName = "Stillaguamish",LAC = 9385, UAC = 20000, Cap.Meth = as.character("imu"), LAMO = .2, MAMO = .35, AAMO = .5, LEG = NA, UEG = NA, MU = "US Inside")
SnohomishRows <- data.frame(PSCStock = 3, FRAMWildStocks = c(35,36), StockName = "Snohomish",LAC = 51667, UAC = 125000, Cap.Meth = as.character("imu"), LAMO = .2, MAMO = .4, AAMO = .6, LEG = NA, UEG = NA, MU = "US Inside")
HoodCanalRows <- data.frame(PSCStock = 4, FRAMWildStocks = c(45,46,55,56,59,60), StockName = "Hood Canal",LAC = 19545, UAC = 41000, Cap.Meth = as.character("imu"), LAMO = .2, MAMO = .45, AAMO = .65, LEG = NA, UEG = NA, MU = "US Inside")
JDFRows <- data.frame(PSCStock = 5, FRAMWildStocks = c(115,116,117,118), StockName = "US Strait JDF",LAC = 11679, UAC = 27445, Cap.Meth = as.character("imu"), LAMO = .2, MAMO = .4, AAMO = .6, LEG = NA, UEG = NA, MU = "US Inside")

QuilRows <- data.frame(PSCStock = 6, FRAMWildStocks = c(131, 132), StockName = "Quillayute",LAC = 7875, UAC = 10500, Cap.Meth = as.character("omu"), LAMO = .2, MAMO = NA, AAMO = NA, LEG = 6300, UEG = 15800, MU = "US Outside")
HohRows <- data.frame(PSCStock = 7, FRAMWildStocks = c(135, 136), StockName = "Hoh",LAC = 2500, UAC = 3333, Cap.Meth = as.character("omu"), LAMO = .2, MAMO = NA, AAMO = NA, LEG = 2000, UEG = 5000, MU = "US Outside")
QueetsRows <- data.frame(PSCStock = 8, FRAMWildStocks = c(139,140), StockName = "Queets",LAC = 7250, UAC = 9667, Cap.Meth = as.character("omu"), LAMO = .2, MAMO = NA, AAMO = NA, LEG = 5800, UEG = 14500, MU = "US Outside")
GraysHarbRows <- data.frame(PSCStock = 9, FRAMWildStocks = c(149, 150, 153, 154, 157, 158), StockName = "Grays Harbor",LAC = 44250, UAC = 59000, Cap.Meth = as.character("omu"), LAMO = NA, MAMO = NA, AAMO = .6, LEG = 35400, UEG = 35400, MU = "US Outside")

LowFRRows <- data.frame(PSCStock = 10, FRAMWildStocks = c(227, 228), StockName = "Lower Fraser",LAC = NA, UAC = NA, Cap.Meth = as.character("None"), LAMO = NA, MAMO = NA, AAMO = NA, LEG = NA, UEG = NA, MU = "Canada")
IntFRRows <- data.frame(PSCStock = 11, FRAMWildStocks = c(231, 232), StockName = "Interior Fraser",LAC = NA, UAC = NA, Cap.Meth = as.character("fixed"), LAMO = .2, MAMO = NA, AAMO = NA, LEG = NA, UEG = NA, MU = "Canada Fixed")
GeoStMLRows <- data.frame(PSCStock = 12, FRAMWildStocks = c(207,208), StockName = "Georgia Strait ML",LAC = NA, UAC = NA, Cap.Meth = as.character("None"), LAMO = NA, MAMO = NA, AAMO = NA, LEG = NA, UEG = NA, MU = "Canada")
GeoStVIRows <- data.frame(PSCStock = 13, FRAMWildStocks = c(211, 212), StockName = "Georgia Strait VI",LAC = NA, UAC = NA, Cap.Meth = as.character("None"), LAMO = NA, MAMO = NA, AAMO = NA, LEG = NA, UEG = NA, MU = "Canada")

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
                 body = "Please see the attached for the PSC report files.  Note that tables are encoded as html files.  These can be converted to tables by saving the files as a .html rather than .txt and opening the file with your web browser.",
                 smtp = list(host.name = "smtp.gmail.com", port = 465, user.name = "derek.dapp.dfw@gmail.com", passwd = "FakePass2", ssl = TRUE),
                 authenticate = TRUE,
                 send = TRUE,
                 attach.files = c("https://dl.dropboxusercontent.com/s/jti9io6bufr0k4j/Figure%204.1.jpg",
                                  "https://dl.dropboxusercontent.com/s/2twmtoi8m1sur4r/Figure%204.2.jpg",
                                  "https://dl.dropboxusercontent.com/s/9t2ozw5ir86covl/Figure%204.3.jpg", 
                                  "https://dl.dropboxusercontent.com/s/3cmw8yda3677ls5/Table%204.1%20html.txt",
                                  "https://dl.dropboxusercontent.com/s/cih9ma0vtly8a72/Table%204.2%20html.txt",
                                  "https://dl.dropboxusercontent.com/s/4g48qbf6kstpb6d/Figure%205.1.jpg",
                                  "https://dl.dropboxusercontent.com/s/38bwwvqhavtgw5p/Table%206.1%20html.txt",
                                  "https://dl.dropboxusercontent.com/s/42hz2sen7szcfps/Table%206.2%20html.txt",
                                  "https://dl.dropboxusercontent.com/s/4n3k8gatiskppa2/Table%206.3%20html.txt",
                                  "https://dl.dropboxusercontent.com/s/kuhyt00y4kr2ivf/Fig71.jpg",
                                  "https://dl.dropboxusercontent.com/s/kqz9jx32h6aiek0/Fig72.jpg",
                                  "https://dl.dropboxusercontent.com/s/tuya8vhn9ayrg1f/Fig73.jpg",
                                  "https://dl.dropboxusercontent.com/s/nvx6tjaqat1wc8n/Fig74.jpg",
                                  "https://dl.dropboxusercontent.com/s/3uk4kktxlt9kdtp/Fig75.jpg",
                                  "https://dl.dropboxusercontent.com/s/dnal6uhxcyughbe/Fig76.jpg",
                                  "https://dl.dropboxusercontent.com/s/bst9kdx5pnya8zi/Fig77.jpg",
                                  "https://dl.dropboxusercontent.com/s/8vvmquab1z6o7od/Fig78.jpg",
                                  "https://dl.dropboxusercontent.com/s/izk583dv0v39vi9/Fig79.jpg",
                                  "https://dl.dropboxusercontent.com/s/rwvdcqwk410mrrv/Fig710.jpg",
                                  "https://dl.dropboxusercontent.com/s/tri09bacabye2kq/Fig711.jpg",
                                  "https://dl.dropboxusercontent.com/s/p8t646v5rwnqaga/Fig712.jpg",
                                  "https://dl.dropboxusercontent.com/s/f0l65a9pit6fi0d/Fig713.jpg",
                                  "https://dl.dropboxusercontent.com/s/a2mtb0dh15njfl3/Table%209.csv",
                                  
                                  "https://dl.dropboxusercontent.com/s/xa24cjc8tj3cy3c/TableE1%20Lower%20Fraser.csv",
                                  "https://dl.dropboxusercontent.com/s/y324609sm51ck3l/TableE2%20Interior%20Fraser.csv",
                                  "https://dl.dropboxusercontent.com/s/39vu1uexj7q2x23/TableE3%20Georgia%20Strait%20ML.csv",
                                  "https://dl.dropboxusercontent.com/s/k3n0icabqyt7c0x/TableE4%20Georgia%20Strait%20VI.csv",
                                  "https://dl.dropboxusercontent.com/s/azdffjrn23qqwyf/TableE5%20Skagit.csv",
                                  "https://dl.dropboxusercontent.com/s/r3pwdwf6aol0yei/TableE6%20Stillaguamish.csv",
                                  "https://dl.dropboxusercontent.com/s/oks451x06msb3m9/TableE7%20Snohomish.csv",
                                  "https://dl.dropboxusercontent.com/s/viy586snvllk5ep/TableE8%20Hood%20Canal.csv",
                                  "https://dl.dropboxusercontent.com/s/tzhb6x4irwfl1wv/TableE9%20US%20Strait%20JDF.csv",
                                  "https://dl.dropboxusercontent.com/s/hj89pnyt9ool026/TableE10%20Quillayute.csv",
                                  "https://dl.dropboxusercontent.com/s/ldjm30ovei8vw7q/TableE11%20Hoh.csv",
                                  "https://dl.dropboxusercontent.com/s/oinc0ocu4lc4a5d/TableE12%20Queets.csv",
                                  "https://dl.dropboxusercontent.com/s/681e4dv68u62nz4/TableE13%20Grays%20Harbor.csv",

                                  "https://dl.dropboxusercontent.com/s/3gup4wp347syq6d/Table%20F%20-%20Lower%20Fraser.csv",
                                  "https://dl.dropboxusercontent.com/s/77g6vdw95ggqp1w/Table%20F%20-%20Interior%20Fraser.csv",
                                  "https://dl.dropboxusercontent.com/s/21nczz5t82mf14p/Table%20F%20-%20St%20of%20Geo%20ML.csv",
                                  "https://dl.dropboxusercontent.com/s/lkgmddt7jc60bok/Table%20F%20-%20St%20of%20Geo%20VI.csv",
                                  "https://dl.dropboxusercontent.com/s/che8dnucqxjjty2/Table%20F%20-%20Skagit.csv",
                                  "https://dl.dropboxusercontent.com/s/s1eizqt9k49objd/Table%20F%20-%20Stillaguamish.csv",
                                  "https://dl.dropboxusercontent.com/s/5mm6b7h3r8gd1nz/Table%20F%20-%20Snohomish.csv",
                                  "https://dl.dropboxusercontent.com/s/4m40ow5m1dw4svw/Table%20F%20-%20Hood%20Canal.csv",
                                  "https://dl.dropboxusercontent.com/s/qvetwz7er2d6pgf/Table%20F%20-%20US%20JDF.csv",
                                  "https://dl.dropboxusercontent.com/s/t8ohzyk0sh37r9e/Table%20F%20-%20Quillayute.csv",
                                  "https://dl.dropboxusercontent.com/s/c5cgq9cbtrzlecm/Table%20F%20-%20Hoh.csv",
                                  "https://dl.dropboxusercontent.com/s/8pvivydowzu8ic4/Table%20F%20-%20Queets.csv",
                                  "https://dl.dropboxusercontent.com/s/xu44i9ot1sss08x/Table%20F%20-%20Grays%20Harbor.csv"),
                 file.descriptions = c("Figure 4.1", "Figure 4.2", "Figure 4.3", "Table 4.1","Table 4.2", "Figure 5.1", "Table 6.1","Table 6.2",
                                       "Table 6.3","Figure 7.1","Figure 7.2", "Figure 7.3", "Figure 7.4", "Figure 7.5", "Figure 7.6", "Figure 7.7", "Figure 7.8",
                                       "Figure 7.9", "Figure 7.10", "Figure 7.11", "Figure 7.12", "Figure 7.13", "Table 9",
                                       "Table E - Lower Fraser", "Table E - Interior Fraser", "Table E - St of Geo ML",
                                       "Table E - St of Geo VI", "Table E - Skagit", "Table E - Stillaguamish",
                                       "Table E - Snohomish", "Table E - Hood Canal", "Table E - US JDF",
                                       "Table E - Quillayute", "Table E - Hoh", "Table E - Queets", "Table E - Grays Harbor",
                                       "Table F - Lower Fraser", "Table F - Interior Fraser", "Table F - St of Geo ML", 
                                       "Table F - St of Geo VI", "Table F - Skagit", "Table F - Stillaguamish",
                                       "Table F - Snohomish", "Table F - Hood Canal", "Table F - US JDF",
                                       "Table F - Quillayute", "Table F - Hoh", "Table F - Queets", "Table F - Grays Harbor"), # optional parameter
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
          
          incProgress(1/2, detail = "Loading Post-Season Data - this may take a few minutes")
          
          RunIDTab = read.csv("https://dl.dropboxusercontent.com/s/ntgampj1d8dyp26/RunList.csv?dl=1")
       
          #removes useless columns in the data
          Drops <- c("PrimaryKey", "SpeciesName", "RunTitle",
                  "RunComments", "CreationDate", "ModifyInputDate", "RunTimeDate")
          RunIDTab<- RunIDTab[ , !(names(RunIDTab) %in% Drops)]
       
          YearList<<- unique(RunIDTab$RunYear)
          YearList <<- sort(YearList, decreasing = FALSE)
          
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
          
          #make them global variables for use in the F Tables
          MortTab <<- MortTab
          EscTab <<- EscTab
       
          #This is the Esc DF to which all ESC data is saved
          MainEscDF <- data.frame(RunYear = as.character(), Escapement= as.double(), Stock = as.character())
          #This is the Mort DF to which all mort data is saved
          MainMortDF <- data.frame(RunYear = as.character(), TotMort= as.double(), Stock = as.character())
          
          #Sets up table 4.1
          Tab41DF <<- data.frame(Management.Unit = as.character(), Low.Moderate.Abundance.Category = as.integer(), Moderate.Abundant.Abundance.Category = as.integer())
          
          #Sets up table 4.2
          Tab42DF <<- data.frame(Management.Unit = as.character(), Escapement.Goal.Range = as.character(), Low.Moderate.Abundance.Category = as.integer(), Moderate.Abundant.Abundance.Category = as.integer())
          
          
          
          
          
          incProgress(2/2, detail = "Loading Pre-Season Data - this may take a few minutes")    
          
          ################Pre-season data loading
          #Load Pre-season RunID, Escapement and Mortality Tables
          
          PreRunIDTab = read.csv("https://dl.dropboxusercontent.com/s/fnfnwv1ihg3xi3i/PreSeasonRunList.csv?dl=1")
          PreEscTab = read.csv("https://dl.dropboxusercontent.com/s/womabybbgs11lb9/PreSeasonEscapement.csv?dl=1")
          PreMortTab = read.csv("https://dl.dropboxusercontent.com/s/ruyewmxz8l1a7jj/PreSeasonMortality.csv?dl=1")
          
          #removes useless columns in the data
          Drops <- c("PrimaryKey", "SpeciesName", "RunTitle",
                     "RunComments", "CreationDate", "ModifyInputDate", "RunTimeDate")
          PreRunIDTab<- PreRunIDTab[ , !(names(PreRunIDTab) %in% Drops)]
          
          PreYearList<<- unique(PreRunIDTab$RunYear)
          PreYearList <<- sort(PreYearList, decreasing = FALSE)
          
          #Adds run info to EscTab
          PreEscTab <- merge(PreEscTab, PreRunIDTab, by= "RunID")
          
          #Convert integers to characters
          PreEscTab$RunID <- as.character(PreEscTab$RunID)
          PreEscTab$RunYear <- as.character(PreEscTab$RunYear)
          PreEscTab$StockID <- as.character(PreEscTab$StockID)
          PreEscTab$Age <- as.character(PreEscTab$Age)
          PreEscTab$TimeStep <- as.character(PreEscTab$TimeStep)
          PreEscTab$PrimaryKey <- as.character(PreEscTab$PrimaryKey)
          PreEscTab$BasePeriodID <- as.character(PreEscTab$BasePeriodID)
          
          #Get summarized Mortalities
          PreMortTab$TotMort <- PreMortTab$LandedCatch + PreMortTab$NonRetention + PreMortTab$Shaker + 
            PreMortTab$DropOff + PreMortTab$MSFLandedCatch + PreMortTab$MSFNonRetention + PreMortTab$MSFShaker + PreMortTab$MSFDropOff
          
          #Remove columns no longer of interest
          Keeps <- c("RunID", "StockID", "Age", "FisheryID", "TimeStep", "TotMort")
          PreMortTab <- PreMortTab[, (names(PreMortTab) %in% Keeps)]
          
          #Adds run info to Mort Tab
          PreMortTab <- merge(PreMortTab, PreRunIDTab, by= "RunID")
          
          #Convert integers to characters
          PreMortTab$RunID <- as.character(PreMortTab$RunID)
          PreMortTab$RunYear <- as.character(PreMortTab$RunYear)
          PreMortTab$StockID <- as.character(PreMortTab$StockID)
          PreMortTab$Age <- as.character(PreMortTab$Age)
          PreMortTab$TimeStep <- as.character(PreMortTab$TimeStep)
          PreMortTab$BasePeriodID <- as.character(PreMortTab$BasePeriodID)
          PreMortTab$FisheryID <- as.character(PreMortTab$FisheryID)
          
          #This is the Esc DF to which all ESC data is saved
          PreMainEscDF <- data.frame(RunYear = as.character(), Escapement= as.double(), Stock = as.character())
          #This is the Mort DF to which all mort data is saved
          PreMainMortDF <- data.frame(RunYear = as.character(), TotMort= as.double(), Stock = as.character())
          
          
          
          
          
       })
       
       withProgress(message = 'Processing Data/Preparing figures', value = 0, {
          incProgress(1/5, detail = "Processing Data")
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
            
            #Finds out years with 0s; add row so a zero gets merged in later
            for (j in YearList[1]:YearList[length(YearList)]){
              if(!(j %in% StockMortSUSRows$RunYear)){
                newrow <- data.frame(RunYear = j, SUSMort = 0)
                StockMortSUSRows <- rbind(StockMortSUSRows, newrow)
              }
            }
            
            #Subsets Mortality DF to get stock/only CA fisheries
            SubMortDFCA <- subset(MortTab, StockID %in% FRAMStks & FisheryID > 166 & FisheryID < 194)
            
            #Gets the column number with mortalities in it, renames it to CA Mort
            ColIndex <- which( colnames(SubMortDFCA)=="TotMort" )
            colnames(SubMortDFCA)[ColIndex] <- "CAMort"
            
            StockMortCARows <- ddply(SubMortDFCA, "RunYear",  numcolwise(sum))
            
            #Finds out years with 0s; add row so a zero gets merged in later
            for (j in YearList[1]:YearList[length(YearList)]){
              if(!(j %in% StockMortCARows$RunYear)){
                newrow <- data.frame(RunYear = j, CAMort = 0)
                StockMortCARows <- rbind(StockMortCARows, newrow)
              }
            }
            
            #Subsets Mortality DF to get stock/only AK fisheries
            SubMortDFAK <- subset(MortTab, StockID %in% FRAMStks & FisheryID > 193)
            
            #Gets the column number with mortalities in it, renames it to CA Mort
            ColIndex <- which( colnames(SubMortDFAK)=="TotMort" )
            colnames(SubMortDFAK)[ColIndex] <- "AKMort"
            
            StockMortAKRows <- ddply(SubMortDFAK, "RunYear",  numcolwise(sum))
            
            #Finds out years with 0s; add row so a zero gets merged in later
            for (j in YearList[1]:YearList[length(YearList)]){
              if(!(j %in% StockMortAKRows$RunYear)){
                newrow <- data.frame(RunYear = j, AKMort = 0)
                StockMortAKRows <- rbind(StockMortAKRows, newrow)
              }
            }
            
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
            
            
            
            
            
            
            ###################Pre season######################################
            
            # Subsets escapement DF to get the stock of interest
            PreSubEscDF <- subset(PreEscTab, StockID %in% FRAMStks)
            
            PreStockEscRows <- ddply(PreSubEscDF, "RunYear",  numcolwise(sum))
            
            PreStockEscRows$Stock <- StockList[i]
            
            PreMainEscDF <- rbind(PreMainEscDF, PreStockEscRows)
            
            
            # Subsets Mortality DF to get the stock of interest
            PreSubMortDF <- subset(PreMortTab, StockID %in% FRAMStks)
            
            PreStockMortRows <- ddply(PreSubMortDF, "RunYear",  numcolwise(sum))
            PreStockMortRows$Stock <- StockList[i]
            
            #Subsets Mortality DF to get stock/only SUS fisheries
            PreSubMortDFSUS <- subset(PreMortTab, StockID %in% FRAMStks & FisheryID < 167)
            
            #Gets the column number with mortalities in it, renames it to SUS Mort
            ColIndex <- which(colnames(PreSubMortDFSUS)=="TotMort" )
            colnames(PreSubMortDFSUS)[ColIndex] <- "SUSMort"
            
            #Sums everything by run year
            PreStockMortSUSRows <- ddply(PreSubMortDFSUS, "RunYear",  numcolwise(sum))
            
            #Finds out years with 0s; add row so a zero gets merged in later
            for (j in PreYearList[1]:PreYearList[length(PreYearList)]){
              if(!(j %in% PreStockMortSUSRows$RunYear)){
                newrow <- data.frame(RunYear = j, SUSMort = 0)
                PreStockMortSUSRows <- rbind(PreStockMortSUSRows, newrow)
              }
            }
            
            #Subsets Mortality DF to get stock/only CA fisheries
            PreSubMortDFCA <- subset(PreMortTab, StockID %in% FRAMStks & FisheryID > 166 & FisheryID < 194)
            
            #Gets the column number with mortalities in it, renames it to CA Mort
            ColIndex <- which(colnames(PreSubMortDFCA)=="TotMort" )
            colnames(PreSubMortDFCA)[ColIndex] <- "CAMort"
            
            PreStockMortCARows <- ddply(PreSubMortDFCA, "RunYear",  numcolwise(sum))
            
            #Finds out years with 0s; add row so a zero gets merged in later
            for (j in PreYearList[1]:PreYearList[length(PreYearList)]){
              if(!(j %in% PreStockMortCARows$RunYear)){
                newrow <- data.frame(RunYear = j, CAMort = 0)
                PreStockMortCARows <- rbind(PreStockMortCARows, newrow)
              }
            }
            
            #Subsets Mortality DF to get stock/only AK fisheries
            PreSubMortDFAK <- subset(PreMortTab, StockID %in% FRAMStks & FisheryID > 193)
            
            #Gets the column number with mortalities in it, renames it to CA Mort
            ColIndex <- which(colnames(PreSubMortDFAK)=="TotMort" )
            colnames(PreSubMortDFAK)[ColIndex] <- "AKMort"
            
            PreStockMortAKRows <- ddply(PreSubMortDFAK, "RunYear",  numcolwise(sum))
            
            #Finds out years with 0s; add row so a zero gets merged in later
            for (j in PreYearList[1]:PreYearList[length(PreYearList)]){
              if(!(j %in% PreStockMortAKRows$RunYear)){
                newrow <- data.frame(RunYear = j, AKMort = 0)
                PreStockMortAKRows <- rbind(PreStockMortAKRows, newrow)
              }
            }
            
            #Merge in SUS,CA, AK
            PreStockMortRows <- merge(PreStockMortRows, PreStockMortSUSRows, by= "RunYear")
            PreStockMortRows <- merge(PreStockMortRows, PreStockMortCARows, by= "RunYear")
            PreStockMortRows <- merge(PreStockMortRows, PreStockMortAKRows, by= "RunYear")
            
            
            PreMainMortDF <- rbind(PreMainMortDF, PreStockMortRows)
         
         
          }
       
          #Merging data frames
          MainDataDF <<- merge(MainEscDF, MainMortDF, by= c("Stock","RunYear"))
       
          #Ocean Cohort = Escapement + Mortality
          MainDataDF$OceanCohort <<- MainDataDF$Escapement + MainDataDF$TotMort
          
          #Merging data frames
          PreMainDataDF <<- merge(PreMainEscDF, PreMainMortDF, by= c("Stock","RunYear"))
          
          #Ocean Cohort = Escapement + Mortality
          PreMainDataDF$OceanCohort <<- PreMainDataDF$Escapement + PreMainDataDF$TotMort
          
          #To get abundance objectives, gets only unique rows for columns 3 to 11
          StockDFOBJ <- unique(StockDF[,3:11])
          
          StockDFOBJ$Stock <- StockDFOBJ$StockName
          
          PreMainDataDF <<- merge(PreMainDataDF, StockDFOBJ, by = "Stock")
          
          #Gets the abundance category
          PreMainDataDF$PreAbund <<- NA
          
          for (i in 1:nrow(PreMainDataDF)){
            if(PreMainDataDF$Cap.Meth[i] == "None"){
              #do nothing - just makes sure that the loop isn't failing as it checks NAs
            }
            else if(PreMainDataDF$Cap.Meth[i] == "omu" | PreMainDataDF$Cap.Meth[i] == "imu"){
               if (PreMainDataDF$LAC[i] > PreMainDataDF$OceanCohort[i]){
                 PreMainDataDF$PreAbund[i] <<- "(L)"
               }
               else if(PreMainDataDF$LAC[i] < PreMainDataDF$OceanCohort[i] & PreMainDataDF$UAC[i] > PreMainDataDF$OceanCohort[i]){
                 PreMainDataDF$PreAbund[i] <<- "(M)"
               }
               else{
                 PreMainDataDF$PreAbund[i] <<- "(A)"
               }
             }
            #Interior Fraser = always low?
            else if(PreMainDataDF$Cap.Meth[i] == "fixed"){
              PreMainDataDF$PreAbund[i] <<- "(L)"
            }
          }
          
          #Gets rid of all the useless columns
          DFDrops <- c("StockName", "LAC", "UAC", "Cap.Meth", "LAMO", "MAMO", "AAMO", "LEG", "UEG", "MU")
          PreMainDataDF<<- PreMainDataDF[ , !(names(PreMainDataDF) %in% DFDrops)]
          
          
          
          
          MainDataDF <<- merge(MainDataDF, StockDFOBJ, by = "Stock")
          
          #Gets the abundance category
          MainDataDF$PostAbund <<- NA


          for (i in 1:nrow(MainDataDF)){
              if(MainDataDF$Cap.Meth[i] == "None"){
                #do nothing - just makes sure that the loop isn't failing as it checks NAs
              }
             else if(MainDataDF$Cap.Meth[i] == "omu" | MainDataDF$Cap.Meth[i] == "imu"){
               if (MainDataDF$LAC[i] > MainDataDF$OceanCohort[i]){
                 MainDataDF$PostAbund[i] <<- "(L)"
               }
               else if(MainDataDF$LAC[i] < MainDataDF$OceanCohort[i] & MainDataDF$UAC[i] > MainDataDF$OceanCohort[i]){
                 MainDataDF$PostAbund[i] <<- "(M)"
               }
               else{
                 MainDataDF$PostAbund[i] <<- "(A)"
               }
             }
             #Interior Fraser = always low?
             else if(MainDataDF$Cap.Meth[i] == "fixed"){
               MainDataDF$PostAbund[i] <<- "(L)"
             }
           }

          #Gets rid of all the useless columns
          MainDataDF<<- MainDataDF[ , !(names(MainDataDF) %in% DFDrops)]
          
       
          #Ocean Cohort Figures (4.1, 4.2, 4.3)
          incProgress(2/5, detail = "Preparing figures 4.1 to 4.3")
       
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
          
          incProgress(3/5, detail = "Preparing figure 5.1")
          
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
          
          #DF used to set up figure orders
          OrdRow1 <- data.frame(name = "Fig71", Stock = "Lower Fraser", order = 1)
          OrdRow2 <- data.frame(name = "Fig72", Stock = "Interior Fraser", order = 2)
          OrdRow3 <- data.frame(name = "Fig73", Stock = "Georgia Strait ML", order = 3)
          OrdRow4 <- data.frame(name = "Fig74", Stock = "Georgia Strait VI", order = 4)
          OrdRow5 <- data.frame(name = "Fig75", Stock = "Skagit", order = 5)
          OrdRow6 <- data.frame(name = "Fig76", Stock = "Stillaguamish", order = 6)
          OrdRow7 <- data.frame(name = "Fig77", Stock = "Snohomish", order = 7)
          OrdRow8 <- data.frame(name = "Fig78", Stock = "Hood Canal", order = 8)
          OrdRow9 <- data.frame(name = "Fig79", Stock = "US Strait JDF", order = 9)
          OrdRow10 <- data.frame(name = "Fig710", Stock = "Quillayute", order = 10)
          OrdRow11 <- data.frame(name = "Fig711", Stock = "Hoh", order = 11)
          OrdRow12 <- data.frame(name = "Fig712", Stock = "Queets", order = 12)
          OrdRow13 <- data.frame(name = "Fig713", Stock = "Grays Harbor", order = 13)
          
          
          OrderDF <<- rbind(OrdRow1,OrdRow2,OrdRow3,OrdRow4,OrdRow5,OrdRow6,OrdRow7,OrdRow8,OrdRow9,OrdRow10,OrdRow11,OrdRow12,OrdRow13)
          
          incProgress(4/5, detail = "Preparing figures 7.1 to 7.13")
          
          #Figures 7.1-7.13
          for (i in 1:length(StockList)){
            #Switching to local solves issues with assign; you also have to do i <-i and pos =1 in the assign function for this to work
            local ({
              
              i <- i
              #Gets figure name from the order df
              Figname <- as.character(subset(OrderDF, Stock == StockList[i])[1,1])
            
              Fig7DF <- subset(MainDataDF, Stock == StockList[i])
            
              #if in a Canadian stock, do not include 1998-2003, 
              if (StockList[i] == "Lower Fraser" | StockList[i] == "Interior Fraser" | StockList[i] == "Georgia Strait ML" | StockList[i] == "Georgia Strait VI"){
                Fig7DF$Escapement[Fig7DF$RunYear %in% c("1998","1999","2000","2001","2002","2003")] <- NA
                Fig7DF$CAMort[Fig7DF$RunYear %in% c("1998","1999","2000","2001","2002","2003")] <- NA
                Fig7DF$SUSMort[Fig7DF$RunYear %in% c("1998","1999","2000","2001","2002","2003")] <- NA
                Fig7DF$AKMort[Fig7DF$RunYear %in% c("1998","1999","2000","2001","2002","2003")] <- NA
              }
            
              #Get morts in terms of ER
              Fig7DF$USER <- (Fig7DF$SUSMort + Fig7DF$AKMort)/Fig7DF$OceanCohort
              Fig7DF$CAER <- Fig7DF$CAMort/Fig7DF$OceanCohort
            
              #Combine the datasets into a usable format for stacked barplots
              Melted7DF <- melt(Fig7DF, id.vars = "RunYear", measure.vars = c("CAER", "USER"))
            
              #DF with just escapements
              Fig7Esc <- Fig7DF[ , (names(Fig7DF) %in% c("RunYear", "Escapement"))]
            
              #adds in escapements to the DF
              Melted7DF <- merge(Melted7DF, Fig7Esc , by= "RunYear")
            
              #Gets readjustment scale
              maxy <- sort(Fig7Esc$Escapement, decreasing = TRUE)[1] +500
            
              #create variable name for later use
              assign(Figname, ggplot(Melted7DF, aes(x = RunYear, y = value, fill = variable))+
                     geom_bar(stat = "identity", colour = "black") + theme_bw()+
                     theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                           panel.background = element_blank(), axis.line = element_line(colour = "black"))+
                     theme(text = element_text(size=16))+
                     xlab('Catch Year') + ylab('Total Exploitation Rate') +
                     theme(axis.text.x = element_text(angle = 90, hjust = 1))+
                     scale_fill_manual("legend", values = c("USER" = "gray47", "CAER" = "gray87"), labels = c("Canada", "US"))+
                     theme(legend.title=element_blank())+
                     #Second axis must get rescaled according to the scaling below
                     scale_y_continuous(lim = c(0,1),sec.axis = sec_axis(~.*maxy, name = "Escapement"))+
                     #Escapements must get rescaled to match axes
                     geom_line(aes(y = Escapement/maxy), group = 1, size = 1.2)+ggtitle(StockList[i]), envir = .GlobalEnv, pos = 1)
            
              # save file for dropbox
              filePath <- file.path(tempdir(), paste(Figname,".jpg",sep=""))
              ggsave(ggplot(Melted7DF, aes(x = RunYear, y = value, fill = variable))+
                     geom_bar(stat = "identity", colour = "black") + theme_bw()+
                     theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                           panel.background = element_blank(), axis.line = element_line(colour = "black"))+
                     theme(text = element_text(size=16))+
                     xlab('Catch Year') + ylab('Total Exploitation Rate') +
                     theme(axis.text.x = element_text(angle = 90, hjust = 1))+
                     scale_fill_manual("legend", values = c("USER" = "gray47", "CAER" = "gray87"), labels = c("Canada", "US"))+
                     theme(legend.title=element_blank())+
                     #Second axis must get rescaled according to the scaling below
                     scale_y_continuous(lim = c(0,1),sec.axis = sec_axis(~.*maxy, name = "Escapement"))+
                     #Escapements must get rescaled to match axes
                     geom_line(aes(y = Escapement/maxy), group = 1, size = 1.2), file = filePath, width = 12, height = 6)
            
              drop_upload(filePath)
            })
            
          }
          
          incProgress(5/5, detail = "Tables")
          #tables
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
          
          
          
          
          
          
          
          
          # #########Table 6 and table 8

          #Only pre-season data from 2004 onward
          CombinedDF <- subset(MainDataDF, RunYear > 2003)

          #Converts to ERs for table 8; saves Cohort for table 6
          CombinedDF$PostUSER <- (CombinedDF$SUSMort + CombinedDF$AKMort)/CombinedDF$OceanCohort
          CombinedDF$PostCAER <- CombinedDF$CAMort/CombinedDF$OceanCohort
          CombinedDF$PostTotER <- CombinedDF$TotMort/CombinedDF$OceanCohort

          CombinedDF$PostCohort <- CombinedDF$OceanCohort

          #drop useless columns
          CombinedDFDrops <- c("Escapement", "TotMort", "SUSMort", "CAMort", "AKMort", "OceanCohort")
          CombinedDF<- CombinedDF[ , !(names(CombinedDF) %in% CombinedDFDrops)]

          CombinedDF <- merge(CombinedDF, PreMainDataDF, by = c("Stock", "RunYear"))

          #Converts to ERs for table 8; saves Cohort for table 6
          CombinedDF$PreUSER <- (CombinedDF$SUSMort + CombinedDF$AKMort)/CombinedDF$OceanCohort
          CombinedDF$PreCAER <- CombinedDF$CAMort/CombinedDF$OceanCohort
          CombinedDF$PreTotER <- CombinedDF$TotMort/CombinedDF$OceanCohort

          CombinedDF$PreCohort <- CombinedDF$OceanCohort

          #drop useless columns
          CombinedDFDrops <- c("Escapement", "TotMort", "SUSMort", "CAMort", "AKMort", "OceanCohort")
          CombinedDF<- CombinedDF[ , !(names(CombinedDF) %in% CombinedDFDrops)]

          #To get abundance objectives, gets only unique rows for columns 3 to 11
          StockDFOBJ <- unique(StockDF[,c(1,3)])

          StockDFOBJ$Stock <- StockDFOBJ$StockName

          #Add psc stock for table ordering
          CombinedDF <- merge(CombinedDF, StockDFOBJ[ , c("Stock", "PSCStock")], by = "Stock", all.x=TRUE)

          #Sort
          CombinedDF <- CombinedDF[with(CombinedDF, order(PSCStock)), ]

          #Really table 6.1 is three separate tables, one for each grouping (CA, US Inside, US Outside)
          Tab6DF <- CombinedDF

          #This gets the percent difference, rounded to the nearest tenth, with a percentage symbol added
          Tab6DF$Difference <- paste(round((Tab6DF$PreCohort - Tab6DF$PostCohort)/Tab6DF$PreCohort, digits = 3) * 100, "%",sep="")

          #Removes NAs
          Tab6DF[is.na(Tab6DF)] <- ""

          #This rounds cohorts, adds in abundances
          Tab6DF$PostCohort <- paste(round(Tab6DF$PostCohort, digits = 0), Tab6DF$PostAbund, sep=" ")
          Tab6DF$PreCohort <- paste(round(Tab6DF$PreCohort, digits = 0), Tab6DF$PreAbund, sep=" ")

          #Rename column headers
          ColIndex <- which(colnames(Tab6DF)=="PostCohort" )
          colnames(Tab6DF)[ColIndex] <- "Post-Season"
          ColIndex <- which(colnames(Tab6DF)=="PreCohort" )
          colnames(Tab6DF)[ColIndex] <- "Pre-Season"
          ColIndex <- which(colnames(Tab6DF)=="RunYear" )
          colnames(Tab6DF)[ColIndex] <- "Catch Year"


          #Drops columns that arent of interest to the table
          Tab6DFDrops <- c("PostAbund", "PostUSER", "PostCAER", "PostTotER", "PreAbund", "PreUSER", "PreCAER", "PreTotER", "PSCStock")
          Tab6DF<- Tab6DF[ , !(names(Tab6DF) %in% Tab6DFDrops)]

          #Fix column orders
          Tab6DF <- Tab6DF[,c("Stock", "Catch Year", "Pre-Season", "Post-Season", "Difference")]

          #Changes it to three separate tables, this more closely matches the formatting in the report.
          Tab61DF <<- subset(Tab6DF, Stock %in% c("Lower Fraser", "Interior Fraser", "Georgia Strait ML", "Georgia Strait VI"))

          ColIndex <- which(colnames(Tab61DF)=="Stock" )
          colnames(Tab61DF)[ColIndex] <<- "Management Unit"

          Tab61DFhtml <- kable (Tab61DF, row.names = FALSE) %>%
            kable_styling("bordered") %>%
            #group columns by stock
            collapse_rows(columns = 1)%>%
            #add header row
            add_header_above(c(" " = 1, " " = 1, "Cohort Abundance (Status of MU)" = 2, " " = 1), bold = T)
          
          Tab62DF <<- subset(Tab6DF, Stock %in% c("Skagit", "Stillaguamish", "Snohomish", "Hood Canal", "US Strait JDF"))
          
          ColIndex <- which(colnames(Tab62DF)=="Stock" )
          colnames(Tab62DF)[ColIndex] <<- "Management Unit"
          
          Tab62DFhtml <- kable (Tab62DF, row.names = FALSE) %>%
            kable_styling("bordered") %>%
            #group columns by stock
            collapse_rows(columns = 1)%>%
            #add header row
            add_header_above(c(" " = 1, " " = 1, "Cohort Abundance (Status of MU)" = 2, " " = 1), bold = T)
          
          Tab63DF <<- subset(Tab6DF, Stock %in% c("Quillayute", "Hoh", "Queets", "Grays Harbor"))
          
          ColIndex <- which(colnames(Tab63DF)=="Stock" )
          colnames(Tab63DF)[ColIndex] <<- "Management Unit"
          
          Tab63DFhtml <- kable (Tab63DF, row.names = FALSE) %>%
            kable_styling("bordered") %>%
            #group columns by stock
            collapse_rows(columns = 1)%>%
            #add header row
            add_header_above(c(" " = 1, " " = 1, "Cohort Abundance (Status of MU)" = 2, " " = 1), bold = T)

          #upload tables 6.1, 6.2, 6.3
          filePath <- file.path(tempdir(), "Table 6.1 html.txt")
          writeLines(Tab61DFhtml,filePath)
          
          filePath2 <- file.path(tempdir(), "Table 6.2 html.txt")
          writeLines(Tab62DFhtml,filePath2)
          
          filePath3 <- file.path(tempdir(), "Table 6.3 html.txt")
          writeLines(Tab63DFhtml,filePath3)

          drop_upload(filePath)
          drop_upload(filePath2)
          drop_upload(filePath3)
          
          
          
          
          
          
          
          
          #Prepare Table 9
          
          USHatcheryMarkedStocks <- c(4,6,8,10,16,20,22,26,28,32,34,38,40,42,48,50,54,58,66,68,72,74,78,80,84,88,
                                      92,96,100,104,110,114,120,126,130,134,138,142,144,148,152,
                                      156,160,164,166,168,176,178,182,186,190,192,194,198,202)
          USHatcheryUnmarkedStocks <- c(3,5,7,9,15,19,21,25,27,31,33,37,39,41,47,49,53,57,65,67,71,73,77,79,83,87,
                                        91,95,99,103,109,113,119,125,129,133,137,141,143,147,151,
                                        155,159,163,165,167,175,177,181,185,189,191,193,197,201)
          CAHatcheryMarkedStocks <- c(206,210,214,218,222,226,230)
          CAHatcheryUnmarkedStocks <- c(205,209,213,217,221,225,229)
          
          #Subsets Mortality DF to get US Hat Marked in CA fisheries
          Tab9DFUSMarked <- subset(MortTab, StockID %in% USHatcheryMarkedStocks & FisheryID > 166 & FisheryID < 193)
          
          Tab9DFUSMarked <- ddply(Tab9DFUSMarked, "RunYear",  numcolwise(sum))
          
          Tab9DFUSMarked$TotMort <- round(Tab9DFUSMarked$TotMort, digits = 0)
          
          #Gets the column number with mortalities in it, renames it 
          ColIndex <- which(colnames(Tab9DFUSMarked)=="TotMort" )
          colnames(Tab9DFUSMarked)[ColIndex] <- "CA Catch of US Marked"
          
          
          
          #US UM
          
          Tab9DFUSUM <- subset(MortTab, StockID %in% USHatcheryUnmarkedStocks & FisheryID > 166 & FisheryID < 193)
          
          Tab9DFUSUM <- ddply(Tab9DFUSUM, "RunYear",  numcolwise(sum))
          
          Tab9DFUSUM$TotMort <- round(Tab9DFUSUM$TotMort, digits = 0)
          
          ColIndex <- which(colnames(Tab9DFUSUM)=="TotMort" )
          colnames(Tab9DFUSUM)[ColIndex] <- "CA Catch of US Unmarked"
          
          
          
          #CA M
          
          Tab9DFCAMarked <- subset(MortTab, StockID %in% CAHatcheryMarkedStocks & (FisheryID < 167 | FisheryID > 193))
          
          Tab9DFCAMarked <- ddply(Tab9DFCAMarked, "RunYear",  numcolwise(sum))
          
          Tab9DFCAMarked$TotMort <- round(Tab9DFCAMarked$TotMort, digits = 0)
          
          ColIndex <- which(colnames(Tab9DFCAMarked)=="TotMort" )
          colnames(Tab9DFCAMarked)[ColIndex] <- "US Catch of CA Marked"
          
          
          
          #CA UM
          Tab9DFCAUM <- subset(MortTab, StockID %in% CAHatcheryUnmarkedStocks & (FisheryID < 167 | FisheryID > 193))
          
          Tab9DFCAUM <- ddply(Tab9DFCAUM, "RunYear",  numcolwise(sum))
          
          Tab9DFCAUM$TotMort <- round(Tab9DFCAUM$TotMort, digits = 0)
          
          ColIndex <- which(colnames(Tab9DFCAUM)=="TotMort" )
          colnames(Tab9DFCAUM)[ColIndex] <- "US Catch of CA Unmarked"
          
          #Merges DFs to a single table
          Tab9 <- merge(Tab9DFCAMarked, Tab9DFCAUM, by= "RunYear")
          Tab9 <- merge(Tab9, Tab9DFUSMarked, by= "RunYear")
          Tab9 <- merge(Tab9, Tab9DFUSUM, by= "RunYear")
          
          ColIndex <- which(colnames(Tab9)=="RunYear" )
          colnames(Tab9)[ColIndex] <- "Year"
          
          Tab9 <<- Tab9
          
          filePath <- file.path(tempdir(), "Table 9.csv")
          write.csv(Tab9,filePath)
          
          drop_upload(filePath)
          
          #Appendix E
          
          for (i in 1:length(StockList)){
            #Switching to local solves issues with assign; you also have to do i <-i and pos =1 in the assign function for this to work
            local ({
              
              i <- i
              
              #Gets table name from the order df
              Tabname <- paste("TableE",as.character(subset(OrderDF, Stock == StockList[i])[1,3]), sep="")
              
              TabEDF <- subset(MainDataDF, Stock == StockList[i])
              
              TabEDF$Total <- paste(round(TabEDF$TotMort/TabEDF$OceanCohort, digits = 3)*100, "%", sep="")
              TabEDF$USTot <- paste(round((TabEDF$SUSMort+TabEDF$AKMort)/TabEDF$OceanCohort, digits = 3)*100, "%", sep="")
              TabEDF$CanTot <- paste(round(TabEDF$CAMort/TabEDF$OceanCohort, digits = 3)*100, "%", sep="")
              TabEDF$EscRounded <- round(TabEDF$Escapement, digits = 0)
              
              KeepCols <- c("RunYear","Total", "USTot", "CanTot", "EscRounded")
              
              TabEDF <- TabEDF[ , (names(TabEDF) %in% KeepCols)]
              
              ColIndex <- which(colnames(TabEDF)=="RunYear" )
              colnames(TabEDF)[ColIndex] <- "Catch Year"
              ColIndex <- which(colnames(TabEDF)=="USTot" )
              colnames(TabEDF)[ColIndex] <- "U.S. Tot"
              ColIndex <- which(colnames(TabEDF)=="CanTot" )
              colnames(TabEDF)[ColIndex] <- "Can Tot"
              ColIndex <- which(colnames(TabEDF)=="EscRounded" )
              colnames(TabEDF)[ColIndex] <- "Escapement"
              
              #create variable name for later use
              assign(Tabname, TabEDF, envir = .GlobalEnv, pos = 1)
              
              filePath <- file.path(tempdir(), paste(Tabname," ",StockList[i],".csv",sep=""))
              write.csv(TabEDF,filePath)
              drop_upload(filePath)
              
            })
          }
          
          
       })
     })
   })
   
   
   
   
   
   
   #Extra button for F tables, which take up a large amount of server memory
   observe({
      if (input$FTableButton == 0 | input$PasswordAdd != Password)
        return(NULL)
      isolate({
            #Appendix F tables
            #Aggregate fisheries into groupings
            BCNCTRRow <- data.frame(Fishery = "BC No/Cent Troll", FRAMFish = c(171,172,173))
            BCNCNetRow <- data.frame(Fishery = "BC No/Cent Net", FRAMFish = c(178, 179))
            BCNCSptRow <- data.frame(Fishery = "BC No/Cent Sport", FRAMFish = c(187, 188))
            BCWCVITrollRow <- data.frame(Fishery = "BC WCVI Troll", FRAMFish = c(174, 175))
            BCWCVINetRow <- data.frame(Fishery = "BC WCVI Net", FRAMFish = c(180, 181))
            BCWCVISptRow <- data.frame(Fishery = "BC WCVI Sport", FRAMFish = c(190, 193))
            BCJSNetRow <- data.frame(Fishery = "BC JnstStr Net & Trl", FRAMFish = c(182, 170))
            BCJSSptRow <- data.frame(Fishery = "BC JnstStr Sport", FRAMFish = c(186))
            BCGSSptRow <- data.frame(Fishery = "BC GeoStr Spt & Trl", FRAMFish = c(176,191,192))
            BCGSNetRow <- data.frame(Fishery = "BC GeoStr Net", FRAMFish = c(183))
            BCJDFSptRow <- data.frame(Fishery = "BC JDF Sport", FRAMFish = c(189))
            BCJDFNetRow <- data.frame(Fishery = "BC JDF Net & Troll", FRAMFish = c(178, 179))
            BCFraserRow <- data.frame(Fishery = "BC Fraser Net & Spt", FRAMFish = c(169, 184, 167, 168))
            
            SEAKRow <- data.frame(Fishery = "SEAK All", FRAMFish = c(194, 195, 196, 197, 198))
            WAOceanTrlRow <- data.frame(Fishery = "WA Ocean Troll", FRAMFish = c(34, 35, 36, 38, 39, 42, 43, 79))
            WAOceanSportRow <- data.frame(Fishery = "WA Ocean Sport", FRAMFish = c(33, 37, 40, 41, 45, 48, 49))
            SofFRow <- data.frame(Fishery = "S of Falcon All", FRAMFish = c(1,2,3,4,5,6,7,8,9,19,11,12,13,14,15,16,17,18,19,20,21,22))
            USJDFRow <- data.frame(Fishery = "U.S. JDF All", FRAMFish = c(44,80,81,91,92))
            SJINetRow <- data.frame(Fishery = "San Juan Isl Net", FRAMFish = c(87,88,96,97))
            SJISptRow <- data.frame(Fishery = "San Juan Isl Sport", FRAMFish = c(93))
            PSSptRow <- data.frame(Fishery = "PS Sport (8-13)", FRAMFish = c(106,107,115,118,129,136,152))
            PSNetRow <- data.frame(Fishery = "PS Net (8-13)", FRAMFish = c(101,102,109,110,111,112,119,120,121,122,123,124,125,130,
                                                                           131,132,133,137,138,139,140,141,142,143,144,145,146,153,
                                                                           154,155,156,157,158,159,160))
            FWRow <- data.frame(Fishery = "FW Net & Sport", FRAMFish = c(23,24,25,26,27,28,29,30,31,32,46,47,50,51,52,53,54,55,56,57,58,
                                                                         59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,82,83,
                                                                         84,85,86,89,90,94,95,98,99,100,103,104,105,108,113,114,116,117,126,127,128,
                                                                         134,135,147,148,149,150,151,161,162,163,164,165,166))
            
            TabFFishDF <<- rbind(BCNCTRRow, BCNCNetRow, BCNCSptRow, BCWCVITrollRow, BCWCVINetRow,
                                 BCWCVISptRow, BCJSNetRow, BCJSSptRow, BCGSSptRow, BCGSNetRow,
                                 BCJDFSptRow, BCJDFNetRow, BCFraserRow, SEAKRow, WAOceanTrlRow,
                                 WAOceanSportRow,SofFRow, USJDFRow, SJINetRow, SJISptRow,
                                 PSSptRow, PSNetRow, FWRow)
            
            #Column 1 labels
            ColLabs <- c("Fishery","BC No/Cent Troll","BC No/Cent Net","BC No/Cent Sport","BC WCVI Troll","BC WCVI Net",
                         "BC WCVI Sport","BC JnstStr Net & Trl","BC JnstStr Sport","BC GeoStr Spt & Trl","BC GeoStr Net",
                         "BC JDF Sport", "BC JDF Net & Troll", "BC Fraser Net & Spt", "B.C. Sub-Total","SEAK All", "WA Ocean Troll",
                         "WA Ocean Sport", "S of Falcon All","U.S. JDF All","San Juan Isl Net","San Juan Isl Sport","PS Sport (8-13)",
                         "PS Net (8-13)", "FW Net & Sport", "U.S. Sub-Total", "Total ER", "Escapement", "Cohort (Ocean age-3)",
                         "Cohort (Jan age-3)")
            #Set up blank table
            TableF <- data.frame(matrix(, nrow = 30, ncol = 32))
            
            #Add in column 1
            for (i in 1:length(ColLabs)){
              TableF[i,1] <- ColLabs[i]
            }
            
            #by stock/year
            for (i in 1:length(StockList)){
              # Subsets the stock DF to get the stock of interest
              TabFSubStockDF <- subset(StockDF, StockName == StockList[i])
              
              #Fram stock list
              TabFFRAMStks <- unique(TabFSubStockDF$FRAMWildStocks)
              
              # Subsets escapement DF to get the stock of interest
              TabFSubEscDF <- subset(EscTab, StockID %in% TabFFRAMStks)
              
              # Subsets Mortality DF to get the stock of interest
              TabFSubMortDF <- subset(MortTab, StockID %in% TabFFRAMStks)
              
              for(j in 1:length(YearList)){
                #Escapement
                Esc <- sum(subset(TabFSubEscDF, RunYear == YearList[j])$Escapement)
                
                #Ocean Age 3 Cohort
                Ocean3Cohort <- sum(subset(TabFSubEscDF, RunYear == YearList[j])$Escapement) + sum(subset(TabFSubMortDF, RunYear == YearList[j])$TotMort)
                
                #Add in year
                TableF[1,j+1] <- YearList[j]
                
                #   #Subsets mortalities to only be from fisheries and year of interest
                TableF[2,j+1] <- paste(round(sum(subset(TabFSubMortDF, RunYear == YearList[j] & FisheryID %in% unique(subset(TabFFishDF, Fishery == "BC No/Cent Troll")$FRAMFish))$TotMort)/Ocean3Cohort, digits = 4) * 100, "%",sep="")
                TableF[3,j+1] <- paste(round(sum(subset(TabFSubMortDF, RunYear == YearList[j] & FisheryID %in% unique(subset(TabFFishDF, Fishery == "BC No/Cent Net")$FRAMFish))$TotMort)/Ocean3Cohort, digits = 4) * 100, "%",sep="")
                TableF[4,j+1] <- paste(round(sum(subset(TabFSubMortDF, RunYear == YearList[j] & FisheryID %in% unique(subset(TabFFishDF, Fishery == "BC No/Cent Sport")$FRAMFish))$TotMort)/Ocean3Cohort, digits = 4) * 100, "%",sep="")
                TableF[5,j+1] <- paste(round(sum(subset(TabFSubMortDF, RunYear == YearList[j] & FisheryID %in% unique(subset(TabFFishDF, Fishery == "BC WCVI Troll")$FRAMFish))$TotMort)/Ocean3Cohort, digits = 4) * 100, "%",sep="")
                TableF[6,j+1] <- paste(round(sum(subset(TabFSubMortDF, RunYear == YearList[j] & FisheryID %in% unique(subset(TabFFishDF, Fishery == "BC WCVI Net")$FRAMFish))$TotMort)/Ocean3Cohort, digits = 4) * 100, "%",sep="")
                TableF[7,j+1] <- paste(round(sum(subset(TabFSubMortDF, RunYear == YearList[j] & FisheryID %in% unique(subset(TabFFishDF, Fishery == "BC WCVI Sport")$FRAMFish))$TotMort)/Ocean3Cohort, digits = 4) * 100, "%",sep="")
                TableF[8,j+1] <- paste(round(sum(subset(TabFSubMortDF, RunYear == YearList[j] & FisheryID %in% unique(subset(TabFFishDF, Fishery == "BC JnstStr Net & Trl")$FRAMFish))$TotMort)/Ocean3Cohort, digits = 4) * 100, "%",sep="")
                TableF[9,j+1] <- paste(round(sum(subset(TabFSubMortDF, RunYear == YearList[j] & FisheryID %in% unique(subset(TabFFishDF, Fishery == "BC JnstStr Sport")$FRAMFish))$TotMort)/Ocean3Cohort, digits = 4) * 100, "%",sep="")
                TableF[10,j+1] <- paste(round(sum(subset(TabFSubMortDF, RunYear == YearList[j] & FisheryID %in% unique(subset(TabFFishDF, Fishery == "BC GeoStr Spt & Trl")$FRAMFish))$TotMort)/Ocean3Cohort, digits = 4) * 100, "%",sep="")
                TableF[11,j+1] <- paste(round(sum(subset(TabFSubMortDF, RunYear == YearList[j] & FisheryID %in% unique(subset(TabFFishDF, Fishery == "BC GeoStr Net")$FRAMFish))$TotMort)/Ocean3Cohort, digits = 4) * 100, "%",sep="")
                TableF[12,j+1] <- paste(round(sum(subset(TabFSubMortDF, RunYear == YearList[j] & FisheryID %in% unique(subset(TabFFishDF, Fishery == "BC JDF Sport")$FRAMFish))$TotMort)/Ocean3Cohort, digits = 4) * 100, "%",sep="")
                TableF[13,j+1] <- paste(round(sum(subset(TabFSubMortDF, RunYear == YearList[j] & FisheryID %in% unique(subset(TabFFishDF, Fishery == "BC JDF Net & Troll")$FRAMFish))$TotMort)/Ocean3Cohort, digits = 4) * 100, "%",sep="")
                TableF[14,j+1] <- paste(round(sum(subset(TabFSubMortDF, RunYear == YearList[j] & FisheryID %in% unique(subset(TabFFishDF, Fishery == "BC Fraser Net & Spt")$FRAMFish))$TotMort)/Ocean3Cohort, digits = 4) * 100, "%",sep="")
                TableF[16,j+1] <- paste(round(sum(subset(TabFSubMortDF, RunYear == YearList[j] & FisheryID %in% unique(subset(TabFFishDF, Fishery == "SEAK All")$FRAMFish))$TotMort)/Ocean3Cohort, digits = 4) * 100, "%",sep="")
                TableF[17,j+1] <- paste(round(sum(subset(TabFSubMortDF, RunYear == YearList[j] & FisheryID %in% unique(subset(TabFFishDF, Fishery == "WA Ocean Troll")$FRAMFish))$TotMort)/Ocean3Cohort, digits = 4) * 100, "%",sep="")
                TableF[18,j+1] <- paste(round(sum(subset(TabFSubMortDF, RunYear == YearList[j] & FisheryID %in% unique(subset(TabFFishDF, Fishery == "WA Ocean Sport")$FRAMFish))$TotMort)/Ocean3Cohort, digits = 4) * 100, "%",sep="")
                TableF[19,j+1] <- paste(round(sum(subset(TabFSubMortDF, RunYear == YearList[j] & FisheryID %in% unique(subset(TabFFishDF, Fishery == "S of Falcon All")$FRAMFish))$TotMort)/Ocean3Cohort, digits = 4) * 100, "%",sep="")
                TableF[20,j+1] <- paste(round(sum(subset(TabFSubMortDF, RunYear == YearList[j] & FisheryID %in% unique(subset(TabFFishDF, Fishery == "U.S. JDF All")$FRAMFish))$TotMort)/Ocean3Cohort, digits = 4) * 100, "%",sep="")
                TableF[21,j+1] <- paste(round(sum(subset(TabFSubMortDF, RunYear == YearList[j] & FisheryID %in% unique(subset(TabFFishDF, Fishery == "San Juan Isl Net")$FRAMFish))$TotMort)/Ocean3Cohort, digits = 4) * 100, "%",sep="")
                TableF[22,j+1] <- paste(round(sum(subset(TabFSubMortDF, RunYear == YearList[j] & FisheryID %in% unique(subset(TabFFishDF, Fishery == "San Juan Isl Sport")$FRAMFish))$TotMort)/Ocean3Cohort, digits = 4) * 100, "%",sep="")
                TableF[23,j+1] <- paste(round(sum(subset(TabFSubMortDF, RunYear == YearList[j] & FisheryID %in% unique(subset(TabFFishDF, Fishery == "PS Sport (8-13)")$FRAMFish))$TotMort)/Ocean3Cohort, digits = 4) * 100, "%",sep="")
                TableF[24,j+1] <- paste(round(sum(subset(TabFSubMortDF, RunYear == YearList[j] & FisheryID %in% unique(subset(TabFFishDF, Fishery == "PS Net (8-13)")$FRAMFish))$TotMort)/Ocean3Cohort, digits = 4) * 100, "%",sep="")
                TableF[25,j+1] <- paste(round(sum(subset(TabFSubMortDF, RunYear == YearList[j] & FisheryID %in% unique(subset(TabFFishDF, Fishery == "FW Net & Sport")$FRAMFish))$TotMort)/Ocean3Cohort, digits = 4) * 100, "%",sep="")
                
                
                #Get total rows
                TableF[15,j+1] <- paste(round(sum(subset(TabFSubMortDF, RunYear == YearList[j] & FisheryID %in% unique(subset(TabFFishDF, Fishery %in% c("BC No/Cent Troll", "BC No/Cent Net", "BC No/Cent Sport", "BC WCVI Troll", "BC WCVI Net", "BC WCVI Sport", "BC JnstStr Net & Trl", "BC JnstStr Sport", "BC GeoStr Spt & Trl", "BC GeoStr Net", "BC JDF Sport", "BC JDF Net & Troll", "BC Fraser Net & Spt"))$FRAMFish))$TotMort)/Ocean3Cohort, digits = 4) * 100, "%",sep="")
                TableF[26,j+1] <- paste(round(sum(subset(TabFSubMortDF, RunYear == YearList[j] & FisheryID %in% unique(subset(TabFFishDF, Fishery %in% c("SEAK All", "WA Ocean Troll", "WA Ocean Sport", "S of Falcon All", "U.S. JDF All", "San Juan Isl Net", "San Juan Isl Sport", "PS Sport (8-13)", "PS Net (8-13)", "FW Net & Sport"))$FRAMFish))$TotMort)/Ocean3Cohort, digits = 4) * 100, "%",sep="")
                TableF[27,j+1] <- paste(round(sum(subset(TabFSubMortDF, RunYear == YearList[j] & FisheryID %in% unique(subset(TabFFishDF, Fishery %in% c("BC No/Cent Troll", "BC No/Cent Net", "BC No/Cent Sport", "BC WCVI Troll", "BC WCVI Net", "BC WCVI Sport", "BC JnstStr Net & Trl", "BC JnstStr Sport", "BC GeoStr Spt & Trl", "BC GeoStr Net", "BC JDF Sport", "BC JDF Net & Troll", "BC Fraser Net & Spt","SEAK All", "WA Ocean Troll", "WA Ocean Sport", "S of Falcon All", "U.S. JDF All", "San Juan Isl Net", "San Juan Isl Sport", "PS Sport (8-13)", "PS Net (8-13)", "FW Net & Sport"))$FRAMFish))$TotMort)/Ocean3Cohort, digits = 4) * 100, "%",sep="")
                
                #Gets additional rows -escapement, cohort
                TableF[28,j+1] <- round(Esc, digits = 0)
                TableF[29,j+1] <- round(Ocean3Cohort, digits = 0)
                TableF[30,j+1] <- round(Ocean3Cohort * 1.2317, digits = 0)
              }
              #save table F with stock name
              assign(paste("TableF", i, sep=""), TableF, pos = 1)
              
            }

            #Manual table editting/saving
            
            #For each table, it is a good idea to rename the first row.  Because table F is the same
            #data frame used for setting up the output tables, it is easier to do it here rather than the loop above
            
            colnames(TableF1) <- TableF1[1, ]
            TableF1 <- TableF1[-1, ]
            colnames(TableF2) <- TableF2[1, ]
            TableF2 <- TableF2[-1, ]
            colnames(TableF3) <- TableF3[1, ]
            TableF3 <- TableF3[-1, ]
            colnames(TableF4) <- TableF4[1, ]
            TableF4 <- TableF4[-1, ]
            colnames(TableF5) <- TableF5[1, ]
            TableF5 <- TableF5[-1, ]
            colnames(TableF6) <- TableF6[1, ]
            TableF6 <- TableF6[-1, ]
            colnames(TableF7) <- TableF7[1, ]
            TableF7 <- TableF7[-1, ]
            colnames(TableF8) <- TableF8[1, ]
            TableF8 <- TableF8[-1, ]
            colnames(TableF9) <- TableF9[1, ]
            TableF9 <- TableF9[-1, ]
            colnames(TableF10) <- TableF10[1, ]
            TableF10 <- TableF10[-1, ]
            colnames(TableF11) <- TableF11[1, ]
            TableF11 <- TableF11[-1, ]
            colnames(TableF12) <- TableF12[1, ]
            TableF12 <- TableF12[-1, ]
            colnames(TableF13) <- TableF13[1, ]
            TableF13 <- TableF13[-1, ]
            
            #For Canadian stocks, change 1998 to 2003 to blanks
            ColIndex <- which(colnames(TableF10)=="1998")
            TableF10[,c(ColIndex, ColIndex+1, ColIndex+2, ColIndex+3, ColIndex+4, ColIndex+5)] <- "---"
            TableF11[,c(ColIndex, ColIndex+1, ColIndex+2, ColIndex+3, ColIndex+4, ColIndex+5)] <- "---"
            TableF12[,c(ColIndex, ColIndex+1, ColIndex+2, ColIndex+3, ColIndex+4, ColIndex+5)] <- "---"
            TableF13[,c(ColIndex, ColIndex+1, ColIndex+2, ColIndex+3, ColIndex+4, ColIndex+5)] <- "---"
            
            #Save to dropbox; save as a global variable for outputting
            #These ones are saved as Csvs to allow users to edit them prior to use
            #In earlier years, FRAM may not be used to create tables.
            filePath <- file.path(tempdir(), "Table F - Lower Fraser.csv")
            write.csv(TableF10,filePath)
            
            drop_upload(filePath)
            
            TableF10 <<- TableF10
            
            filePath <- file.path(tempdir(), "Table F - Interior Fraser.csv")
            write.csv(TableF11,filePath)
            
            drop_upload(filePath)
            
            TableF11 <<- TableF11
            
            filePath <- file.path(tempdir(), "Table F - St of Geo ML.csv")
            write.csv(TableF12,filePath)
            
            drop_upload(filePath)
            
            TableF12 <<- TableF12
            
            filePath <- file.path(tempdir(), "Table F - St of Geo VI.csv")
            write.csv(TableF13,filePath)
            
            drop_upload(filePath)
            
            TableF13 <<- TableF13
            
            filePath <- file.path(tempdir(), "Table F - Skagit.csv")
            write.csv(TableF1,filePath)
            
            drop_upload(filePath)
            
            TableF1 <<- TableF1
            
            filePath <- file.path(tempdir(), "Table F - Stillaguamish.csv")
            write.csv(TableF2,filePath)
            
            drop_upload(filePath)
            
            TableF2 <<- TableF2
            
            filePath <- file.path(tempdir(), "Table F - Snohomish.csv")
            write.csv(TableF3,filePath)
            
            drop_upload(filePath)
            
            TableF3 <<- TableF3
            
            filePath <- file.path(tempdir(), "Table F - Hood Canal.csv")
            write.csv(TableF4,filePath)
            
            drop_upload(filePath)
            
            TableF4 <<- TableF4
            
            filePath <- file.path(tempdir(), "Table F - US JDF.csv")
            write.csv(TableF5,filePath)
            
            drop_upload(filePath)
            
            TableF5 <<- TableF5
            
            filePath <- file.path(tempdir(), "Table F - Quillayute.csv")
            write.csv(TableF6,filePath)
            
            drop_upload(filePath)
            
            TableF6 <<- TableF6
            
            filePath <- file.path(tempdir(), "Table F - Hoh.csv")
            write.csv(TableF7,filePath)
            
            drop_upload(filePath)
            
            TableF7 <<- TableF7
            
            filePath <- file.path(tempdir(), "Table F - Queets.csv")
            write.csv(TableF8,filePath)
            
            drop_upload(filePath)
            
            TableF8 <<- TableF8
            
            filePath <- file.path(tempdir(), "Table F - Grays Harbor.csv")
            write.csv(TableF9,filePath)
            
            drop_upload(filePath)
            
            TableF9 <<- TableF9
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
        - Data must be processed before F tables can be produced
        
        Thanks for using the tool! 
        
        Please email Derek Dapp at derek.dapp@dfw.wa.gov or
        Angelika Hagen-Breaux at angelika.hagen-breaux@dfw.wa.gov
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
      else if (graphic == "Figure 7.1"){
        # #Load image from dropbox...second axis will be messed up otherwise
        # figurl <- "https://www.dropbox.com/s/kuhyt00y4kr2ivf/Fig71.jpg?raw=1"
        # temploc <- tempfile()
        # download.file(figurl,temploc,mode="wb")
        # jpg = readJPEG(temploc)
        # 
        # plot(c(100,550), c(300,450),asp=1,type='n',xaxs='i',yaxs='i',xaxt='n',yaxt='n',xlab='',ylab='',bty='n')
        # rasterImage(jpg,100,300,450,450)
        
        maxy <- 160000
        Fig71
      }
      else if (graphic == "Figure 7.2"){
        Fig72
      }
      else if (graphic == "Figure 7.3"){
        Fig73
      }
      else if (graphic == "Figure 7.4"){
        Fig74
      }
      else if (graphic == "Figure 7.5"){
        Fig75
      }
      else if (graphic == "Figure 7.6"){
        Fig76
      }
      else if (graphic == "Figure 7.7"){
        Fig77
      }
      else if (graphic == "Figure 7.8"){
        Fig78
      }
      else if (graphic == "Figure 7.9"){
        Fig79
      }
      else if (graphic == "Figure 7.10"){
        Fig710
      }
      else if (graphic == "Figure 7.11"){
        Fig711
      }
      else if (graphic == "Figure 7.12"){
        Fig712
      }
      else if (graphic == "Figure 7.13"){
        Fig713
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
    else if (input$table == "Table 6.1"){
      Tab61DF
    }
    else if (input$table == "Table 6.2"){
      Tab62DF
    }
    else if (input$table == "Table 6.3"){
      Tab63DF
    }
    else if (input$table == "Table 9"){
      Tab9
    }
    else if (input$table == "Table E - Lower Fraser"){
      TableE1
    }
    else if (input$table == "Table E - Interior Fraser"){
      TableE2
    }
    else if (input$table == "Table E - St of Geo ML"){
      TableE3
    }
    else if (input$table == "Table E - St of Geo VI"){
      TableE4
    }
    else if (input$table == "Table E - Skagit"){
      TableE5
    }
    else if (input$table == "Table E - Stillaguamish"){
      TableE6
    }
    else if (input$table == "Table E - Snohomish"){
      TableE7
    }
    else if (input$table == "Table E - Hood Canal"){
      TableE8
    }
    else if (input$table == "Table E - US JDF"){
      TableE9
    }
    else if (input$table == "Table E - Quillayute"){
      TableE10
    }
    else if (input$table == "Table E - Hoh"){
      TableE11
    }
    else if (input$table == "Table E - Queets"){
      TableE12
    }
    else if (input$table == "Table E - Grays Harbor"){
      TableE13
    }
    else if (input$table == "Table F - Lower Fraser"){
      TableF10
    }
    else if (input$table == "Table F - Interior Fraser"){
      TableF11
    }
    else if (input$table == "Table F - St of Geo ML"){
      TableF12
    }
    else if (input$table == "Table F - St of Geo VI"){
      TableF13
    }
    else if (input$table == "Table F - Skagit"){
      TableF1
    }
    else if (input$table == "Table F - Stillaguamish"){
      TableF2
    }
    else if (input$table == "Table F - Snohomish"){
      TableF3
    }
    else if (input$table == "Table F - Hood Canal"){
      TableF4
    }
    else if (input$table == "Table F - US JDF"){
      TableF5
    }
    else if (input$table == "Table F - Quillayute"){
      TableF6
    }
    else if (input$table == "Table F - Hoh"){
      TableF7
    }
    else if (input$table == "Table F - Queets"){
      TableF8
    }
    else if (input$table == "Table F - Grays Harbor"){
      TableF9
    }
  }) 
}
  
)