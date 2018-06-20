# Clear workspace
rm(list=ls(all=TRUE))

library(rdrop2)

#Authenticates so that you can access dropbox from Shiny Servers
token <- drop_auth(new_user = TRUE)
saveRDS(token, "C://Users//dappdrd//Desktop//PSC-FRAM-Admin-master//templates//COTC_Tool//droptoken_PSC_CoTC_Dropbox.rds")

token <- readRDS("C://Users//dappdrd//Desktop//PSC-FRAM-Admin-master//templates//COTC_Tool//droptoken_PSC_CoTC_Dropbox.rds")
# Then pass the token to each drop_ function
drop_acc(dtoken = token)
