library(plyr)

FinalRunList <<- c("BASE.Cmd", "CB86.Cmd", "CB87.Cmd", "CB88.Cmd", "CB89.Cmd", "CB90.Cmd", "CB91.Cmd", "CB92.Cmd", "CB93.Cmd", "CB94.Cmd", "CB95.Cmd",
                   "CB96.Cmd", "CB97.Cmd", "BK98 w UF H&W", "BK99 w UF H&W", "BK00 w UF H&W", "BK01 w UF H&W Reload Catches", "BK02 w UF H&W Reload catches",
                   "BK03 w catches and BC cohorts", "BK04 w catches BCcohorts", "BK05 w catches and BC cohorts", "BK06 w catches and BC cohorts", 
                   "BK07 catches and BC cohorts", "BK08.cmd", "BK09 New CNR", "bk10PSCFeb14", "Coho2011Post_PSC 2013", "Coho2012Post_PSC SSNPx2 Q Aug 22 BC MSF corrected",
                   "bk 2013 Feb 11 2015 adjust GB recruits", "bc-BK2014 w TAMM inputs final#2", "bc-bkCoho2015 Final")


#This sets up a stock DF that has all the necessary stock information to be used later
#LAC = Low Abundance Cohort; UAC = Upper Abundance Cohort; LAMO = Low Abundance Management Objective (cap); MAMO = Moderate Abundance Management Objective,
#AAMO = Abundant Abundance Management Objective
SkagitRows <- data.frame(PSCStock = 1, FRAMWildStocks = c(17,18,23,24), StockName = "Skagit",LAC = 22857, UAC = 62500, Cap.Meth = "imu", LAMO = .2, MAMO = .35, AAMO = .6, LEG = NA, UEG = NA)
StillyRows <- data.frame(PSCStock = 2, FRAMWildStocks = c(29,30), StockName = "Stillaguamish",LAC = 9385, UAC = 20000, Cap.Meth = "imu", LAMO = .2, MAMO = .35, AAMO = .5, LEG = NA, UEG = NA)
SnohomishRows <- data.frame(PSCStock = 3, FRAMWildStocks = c(35,36), StockName = "Snohomish",LAC = 51667, UAC = 125000, Cap.Meth = "imu", LAMO = .2, MAMO = .4, AAMO = .6, LEG = NA, UEG = NA)
HoodCanalRows <- data.frame(PSCStock = 4, FRAMWildStocks = c(45,46,55,56,59,60), StockName = "Hood Canal",LAC = 19545, UAC = 41000, Cap.Meth = "imu", LAMO = .2, MAMO = .45, AAMO = .65, LEG = NA, UEG = NA)
JDFRows <- data.frame(PSCStock = 5, FRAMWildStocks = c(115,116,117,118), StockName = "US Strait JDF",LAC = 11679, UAC = 27445, Cap.Meth = "imu", LAMO = .2, MAMO = .4, AAMO = .6, LEG = NA, UEG = NA)

QuilRows <- data.frame(PSCStock = 6, FRAMWildStocks = c(131, 132), StockName = "Quillayute",LAC = 7875, UAC = 10500, Cap.Meth = "omu", LAMO = .2, MAMO = NA, AAMO = NA, LEG = 6300, UEG = 15800)
HohRows <- data.frame(PSCStock = 7, FRAMWildStocks = c(135, 136), StockName = "Hoh",LAC = 2500, UAC = 3333, Cap.Meth = "omu", LAMO = .2, MAMO = NA, AAMO = NA, LEG = 2000, UEG = 5000)
QueetsRows <- data.frame(PSCStock = 8, FRAMWildStocks = c(139,140), StockName = "Queets",LAC = 7250, UAC = 9667, Cap.Meth = "omu", LAMO = .2, MAMO = NA, AAMO = NA, LEG = 5800, UEG = 14500)
GraysHarbRows <- data.frame(PSCStock = 9, FRAMWildStocks = c(149, 150, 153, 154, 157, 158), StockName = "Grays Harbor",LAC = 44250, UAC = 59000, Cap.Meth = "omu", LAMO = NA, MAMO = NA, AAMO = .6, LEG = 35400, UEG = 35400)

LowFRRows <- data.frame(PSCStock = 10, FRAMWildStocks = c(227, 228), StockName = "Lower Fraser",LAC = NA, UAC = NA, Cap.Meth = NA, LAMO = NA, MAMO = NA, AAMO = NA, LEG = NA, UEG = NA)
IntFRRows <- data.frame(PSCStock = 11, FRAMWildStocks = c(231, 232), StockName = "Interior Fraser",LAC = NA, UAC = NA, Cap.Meth = "fixed", LAMO = .2, MAMO = NA, AAMO = NA, LEG = NA, UEG = NA)
GeoStMLRows <- data.frame(PSCStock = 12, FRAMWildStocks = c(207,208), StockName = "Georgia Strait ML",LAC = NA, UAC = NA, Cap.Meth = NA, LAMO = NA, MAMO = NA, AAMO = NA, LEG = NA, UEG = NA)
GeoStVIRows <- data.frame(PSCStock = 13, FRAMWildStocks = c(211, 212), StockName = "Georgia Strait VI",LAC = NA, UAC = NA, Cap.Meth = NA, LAMO = NA, MAMO = NA, AAMO = NA, LEG = NA, UEG = NA)

StockDF <<- rbind(SkagitRows, StillyRows, SnohomishRows, HoodCanalRows, JDFRows, QuilRows, HohRows, QueetsRows, GraysHarbRows, LowFRRows, IntFRRows, GeoStMLRows, GeoStVIRows)

#List of Stocks
StockList <<- as.character(unique(StockDF$StockName))




#Grabs FRAM RunID Table
RunIDTab = read.csv("https://dl.dropboxusercontent.com/s/ntgampj1d8dyp26/RunList.csv?dl=1")

#removes useless columns in the data
Drops <- c("PrimaryKey", "SpeciesName", "RunTitle",
           "RunComments", "CreationDate", "ModifyInputDate", "RunTimeDate")
RunIDTab<- RunIDTab[ , !(names(RunIDTab) %in% Drops)]





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



#This is the Esc DF to which all ESC data is saved
MainEscDF <- data.frame(RunYear = as.character(), Escapement= as.double(), Stock = as.character())
#This is the Mort DF to which all mort data is saved
MainMortDF <- data.frame(RunYear = as.character(), TotMort= as.double(), Stock = as.character())
  
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
  
  MainMortDF <- rbind(MainMortDF, StockMortRows)
  
  
  
}

#Merging data frames
MainDataDF <- merge(MainEscDF, MainMortDF, by= c("Stock","RunYear"))

#Ocean Cohort = Escapement + Mortality
MainDataDF$OceanCohort <- MainDataDF$Escapement + MainDataDF$TotMort

#Ocean Cohort Figures (4.1, 4.2, 4.3)

for (i in 1:3){
  if (i == 1){
    FigStocks <- c("Lower Fraser", "Interior Fraser", "Georgia Strait ML", "Georgia Strait VI")
    
    FigName <- "Figure 4.1.jpg"
    
    #FigDF <- subset(MainDataDF, Stock %in% FigStocks & !(RunYear %in% c("1998","1999","2000","2001","2002","2003")))
    FigDF <- subset(MainDataDF, Stock %in% FigStocks)
    
    FigDF$OceanCohort[FigDF$RunYear %in% c("1998","1999","2000","2001","2002","2003")] <- NA
    
  }
  if (i == 2){
    FigStocks <- c("Skagit", "Stillaguamish", "Snohomish", "Hood Canal", "US Strait JDF")
    
    FigName <- "Figure 4.2.jpg"
    
    FigDF <- subset(MainDataDF, Stock %in% FigStocks)
  }
  if (i == 3){
    FigStocks <- c("Quillayute", "Hoh", "Queets", "Grays Harbor")
    
    FigName <- "Figure 4.3.jpg"
    
    FigDF <- subset(MainDataDF, Stock %in% FigStocks)
  }
  
  FigDF$OceanCohortThous <- FigDF$OceanCohort/1000
  
  Fig <- ggplot(FigDF, aes(x=RunYear, y = OceanCohortThous, group = Stock, linetype = Stock, colour = Stock, size = Stock))+
    geom_line() + theme_bw()+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black")) +
    scale_color_manual(values=c("black", "black", "black", "gray", "gray"))+
    scale_linetype_manual(values=c("solid", "dashed", "dotted", "solid", "dashed"))+
    scale_size_manual(values=c(1.2, 1.2, 1.2, 1.2, 1.2))+
    theme(text = element_text(size=16))+
    xlab('Catch Year') + ylab('Cohort Size (thousands)') +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
}
