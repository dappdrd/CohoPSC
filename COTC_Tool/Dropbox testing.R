library(rdrop2)
library(mailR)

#Authenticates so that you can access dropbox from Shiny Servers
token <- drop_auth()
saveRDS(token, "droptoken.rds")
token <- readRDS("droptoken.rds")
drop_acc(dtoken = token)

test <<- data.frame(Test = 1, test2 = 4)
write.csv(test, 'testfile.csv')
drop_upload('testfile.csv')

File <- drop_get('testfile.csv')

fileName <- sprintf("%s_%s.csv", as.integer(Sys.time()), digest::digest(data))
filePath <- file.path(tempdir(), fileName)
write.csv(test, filePath, row.names = FALSE, quote = TRUE)
drop_upload(filePath, dest = "Data")


File <- drop_read_csv('testfile.csv')

filesInfo <- drop_dir("Data")
filePaths <- filesInfo$path
data <- lapply(filePaths, drop_read_csv, stringsAsFactors = FALSE)
data <- do.call(rbind, data)
data

drop_dir()

x <- drop_search("1500918779_76eaed608a41e87527a651c9ae3a4828")

Pathy <- drop_get(x$path, filePath)

data <- drop_get()
send.mail(from = "derek.dapp.dfw@gmail.com",
          to = "derek.dapp@dfw.wa.gov",
          subject = "Coho PSC Report Data",
          body = "Please see the attached files for the PSC report files",
          smtp = list(host.name = "smtp.gmail.com", port = 465, user.name = "derek.dapp.dfw@gmail.com", passwd = "FakePass2", ssl = TRUE),
          authenticate = TRUE,
          send = TRUE,
          attach.files = c("https://dl.dropboxusercontent.com/s/kpc9ipny5hhbvh3/Chinook.jpg", "https://dl.dropboxusercontent.com/s/67cekqliabs52ja/testfile.csv?dl=1"),
          #attach.files = c(drop_read_csv('testfile.csv')),
          # file.names = c("Download log.log", "Upload log.log", "DropBox File.rtf"), # optional parameter
          file.descriptions = c("Random picture", "Random csv"), # optional parameter
          debug = TRUE)



#used for connecting to db
library(RODBC)
library(ImportExport)

db <- drop_get("FramVS2-PSC-Coho-Backwards-for 2015.mdb", overwrite = FALSE)

db <- drop_get("Chinook.jpg", overwrite = TRUE, progress = TRUE)

db <- drop_get("testfile.csv", overwrite = TRUE, progress = TRUE)

test = read.csv("https://dl.dropboxusercontent.com/s/67cekqliabs52ja/testfile.csv?dl=1")

Table <-  access_import("https://www.dropbox.com/s/1vz5ev538tcgcz2/FramVS2-PSC-Coho-Backwards-for%202015.mdb?raw=1",table_names = "TimeStep")

fram.conn <- odbcConnectAccess("https://www.dropbox.com/s/1vz5ev538tcgcz2/FramVS2-PSC-Coho-Backwards-for%202015.mdb?dl=1")
cohorttab <- sqlQuery(fram.conn, 'select * from Cohort')


library(pool)
library(dplyr)

conn <- dbConnect(
  drv = RMySQL::MySQL(),
  dbname = "shinydemo",
  host = "https://www.dropbox.com/s/1vz5ev538tcgcz2/FramVS2-PSC-Coho-Backwards-for%202015.mdb?raw=1")

library(DBI)
conn <- dbConnect(
  drv = RMySQL::MySQL(),
  dbname = "shinydemo",
  host = "https://www.dropbox.com/s/1vz5ev538tcgcz2/")


library(RMySQL)

options(mysql = list(
  "host" = "127.0.0.1",
  "port" = 3306,
  "user" = "root",
  "password" = ""
))

databaseName <- "FramVS2-PSC-Coho-Backwards-for 2015.mdb"
table <- "Cohort"

# Connect to the database
db <- dbConnect(MySQL(), dbname = databaseName, host = options()$mysql$host, 
                  port = options()$mysql$port, user = options()$mysql$user, 
                  password = options()$mysql$password)


temp <- download.file('https://www.dropbox.com/s/1vz5ev538tcgcz2/FramVS2-PSC-Coho-Backwards-for%202015.mdb?raw=1',
              destfile="temp.mdb",
              method="auto")

fram.conn <- odbcConnectAccess('https://www.dropbox.com/s/1vz5ev538tcgcz2/FramVS2-PSC-Coho-Backwards-for%202015.mdb?raw=1')
