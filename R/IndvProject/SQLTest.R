install.packages("RMariaDB")
library(RMariaDB)

#cmd csv to mysql:
#mysqlimport --columns='head -n 1 trainfordb.csv' --ignore-lines=1 "predic(t)amentdb" trainfordb.csv -u root -p root


localuserpassword <- "root"
PredictamentDB <- dbConnect(RMariaDB::MariaDB(), user='root', password=localuserpassword, dbname='predic(t)amentdb', host='localhost')
dbListTables(PredictamentDB)
query <- "SELECT * FROM train_data;"
df <- dbFetch(dbSendQuery(PredictamentDB, query))
dbDisconnect(PredictamentDB)
