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
