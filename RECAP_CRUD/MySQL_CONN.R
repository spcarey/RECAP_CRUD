source("credentials.R",  local=environment())

library(pool)

dbCredentials <- getCredentials("recap_db")
cat(paste("host:", dbCredentials$hostname, "dbname:", dbCredentials$name))


OpenConnMySQL <- function() {
  print("Connecting to DB ...")
  con_sql <- pool::dbPool (drv = RMySQL::MySQL(), 
                    dbname = dbCredentials$name,
                    host = dbCredentials$hostname,
                    port = dbCredentials$port,
                    username = dbCredentials$username,
                    password = dbCredentials$password
                    )
  
  print(summary(con_sql))
  
  print("Connected to DB")
  OpenConnMySQL = con_sql
}






