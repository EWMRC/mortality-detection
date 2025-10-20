# Write Movebank credentials to a file that is ignored by git, allowing 
# the repository to be shared without compromising credentials

library(move)
login <- movebankLogin(username = "XXXXX", password="XXXXX")
saveRDS(login, file = "movebank_credentials.rds")
