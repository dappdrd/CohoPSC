setwd("/Users/dappdrd/Desktop/PSC-FRAM-Admin-master/templates/COTC_Tool")

library(RODBC)
fram.conn <- odbcConnectAccess("./FramVS2-PSC-Coho-Backwards-for 2015.mdb")
cohorttab <- sqlQuery(fram.conn, 'select * from Cohort')

str(cohorttab)

MUList <<- c("Lower Fraser", "Interior Fraser", "Strait of Georgia Mainland", "Strait of Georgia Vancouver Island", "Skagit", "Stillaguamish",
             "Snohomish", "Hood Canal", "US Strait of Juan de Fuca", "Quillayute", "Hoh", "Queets", "Grays Harbor")

LowerFraserRow <- data.frame(name = "Lower Fraser", NatUMStk = 227, NatMStk = 228, HatUMStk = 225, HatMStk = 226)
InteriorFraserRow <- data.frame(name = "Interior Fraser", NatUMStk = 231, NatMStk = 232, HatUMStk = 229, HatMStk = 230)
StGeoRow <- data.frame(name = "Strait of Georgia Mainland", NatUMStk = 207, NatMStk = 208, HatUMStk = 205, HatMStk = 206)
StGeoVIRow <- data.frame(name = "Strait of Georgia Vancouver Island", NatUMStk = 211, NatMStk = 212, HatUMStk = 209, HatMStk = 210)

#Do we need to include Baker/Swin here?
SkagitRow <- data.frame(name = "Skagit", NatUMStk = 17, NatMStk = 18, HatUMStk = 19, HatMStk = 20)

StillyRow <- data.frame(name = "Stillaguamish", NatUMStk = 29, NatMStk = 30, HatUMStk = 31, HatMStk = 32)
#do we need to include tulalip/others?
SnohoRow <- data.frame(name = "Snohomish", NatUMStk = 35, NatMStk = 36, HatUMStk = 37, HatMStk = 38)
#Hood Canal/USJDF/Quillayute/GH/Queets?

HohRow <- data.frame(name = "Hoh", NatUMStk = 135, NatMStk = 136, HatUMStk = 137, HatMStk = 138)


CAFishList <- c()

#includes Columbia River
SUSNonWAFishList <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32)

WAFishList <- c()

AKFishList <- c()