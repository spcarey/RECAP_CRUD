

insert_DB <- function(x,y){


 v  <-  x %>% 
    rowwise() %>% 
    transmute(sql_code = paste0("INSERT INTO"," ",y," ","VALUES"," ","(","\'",Date,"\',","\'",Hours,"\'",","," ","\'",Opn_Hrs,"\'",")") )
 
 dbargs <- list(drv = RMySQL::MySQL(), 
                dbname = "recap",
                host =  '127.0.0.1',
                port = 3306,
                username = "root",
                password = "!QAZ2wsx")
 
 recapdb <- do.call(DBI::dbConnect, dbargs)
 
  i = 1

    while (i <= length(v$sql_code)) {
      
        dbGetQuery(recapdb, v$sql_code[i]); i = i + 1
    
    }
 
 DBI::dbDisconnect(recapdb)
   
}





Grumpy_sql <- insert_DB(Grumpy, "Grumpy")
Sleepy_sql <- insert_DB(Sleepy, "Sleepy")






