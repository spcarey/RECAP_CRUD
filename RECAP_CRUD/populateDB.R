library(pool)
library(dplyr)
library(readr)
library(DBI)

dbargs <- list(
   
   drv = RMySQL::MySQL(),
   dbname = "recap",
   host = "localhost",
   port = 3306,
   username = "recapuser",
   password = "!QAZ2wsx"
   
)

Sleepy <- read_csv("RECAP_CRUD/Sleepy_Usage_Report.csv")
Sleepy$Date <- as.Date(Sleepy$Date, format = "%m/%d/%Y") %>% format("%m/%d/%Y") 
Doc <- read_csv("RECAP_CRUD/Doc_Usage_Report.csv")
Doc$Date <- as.Date(Doc$Date, format = "%m/%d/%Y") %>% format("%m/%d/%Y") 
Grumpy <- read_csv("RECAP_CRUD/Grumpy_Usage_Report.csv")
Grumpy$Date <- as.Date(Grumpy$Date, format = "%m/%d/%Y") %>% format("%m/%d/%Y") 
Sneezy <- read_csv("RECAP_CRUD/Sneezy_Usage_Report.csv")
Sneezy$Date <- as.Date(Sneezy$Date, format = "%m/%d/%Y") %>% format("%m/%d/%Y") 
Fleet_Info <- read_csv("RECAP_CRUD/Fleet_Info.csv")


conn <- do.call(DBI::dbConnect, dbargs)
on.exit(DBI::dbDisconnect(conn))

copy_to(
   dest = conn,
   name = "Sleepy",
   df = Sleepy,
   temporary = FALSE,
   overwrite = TRUE
   ) 

dbDisconnect(recapdb)

copy_to(
   dest = conn,
   name = "Grumpy",
   df = Grumpy,
   temporary = FALSE,
   overwrite = TRUE
) 

dbDisconnect(recapdb)

copy_to(
   dest = recapdb,
   name = "Doc",
   df = Doc,
   temporary = FALSE,
   overwrite = TRUE
) 

dbDisconnect(recapdb)

copy_to(
   dest = recapdb,
   name = "Sneezy",
   df = Sneezy,
   temporary = FALSE,
   overwrite = TRUE
) 

dbDisconnect(recapdb)

copy_to(
   dest = recapdb,
   name = "Fleet_Info",
   df = Fleet_Info,
   temporary = FALSE,
   overwrite = TRUE
) 

dbDisconnect(conn)

tail <- "Doc"
date <- "2/2/2019"
hours <-"6" 


sql_code <- paste0("INSERT INTO"," ",tail," ","(Date , Hours)"," ","VALUES"," ","(","\'",date,"\'",",","\'",hours,"\'",")")

cat(sql_code)


query <- sqlInterpolate(recapdb, sql_code )

dbGetQuery(recapdb, sql_code)

recapdb %>%
  tbl("Sleepy") %>% 
  dplyr::collect() %>% tail()




killDbConnections <- function () {
  
  all_cons <- dbListConnections(RMySQL::MySQL())
  
  print(all_cons)
  
  for(con in all_cons)
    +  dbDisconnect(con)
  
  print(paste(length(all_cons), " connections killed."))
  
}

RMySQL::dbDisconnect(RMySQL::dbListConnections(RMySQL::MySQL())[[1]])

RMySQL::dbListConnections(RMySQL::MySQL())

sidebarPanel(selectInput("tail_db", h4(em(p(strong("Please select a tail number."))), style = "color: green" ), 
                         choices = c(Choose="", as.list(fleet.data$Tail_Num)),
                         selected = "Sleepy",
                         selectize = TRUE),
             dateInput("date_db", "Select Date: ", value = Sys.Date(), format = "m/d/yyyy"),
             
             numericInput("hours_db", label = "Enter # of Flight Hours:", value = 0, min = 0),
             actionButton("create_db", "Add to Log" )
), ##/SidebarPanel
mainPanel(
  verbatimTextOutput("SQL_INSERT")
) ##/MainPanel 



recapdb %>% 
  tbl("Sleepy") %>% 
 filter( Date >='1/1/1995' & Date <= '3/6/2019') %>% 
  collect()

datex <- "11/1/1999"

datey <- (recapdb %>% tbl("Sleepy") %>% select("Date") %>% collect() %>% as.list() %>% mdy(date) )

if(datex %in% datey[[1]]){
  print("Truth is found")
} else {
  print("Truth is not found")
}
date

datez <- paste0('^',datex,'$')


str_detect(datey$Date,  datez)

grepl(datez, datey$Date)

datex %in% datey[[1]]

as.character( mdy(datey[[1]]))


OpenConnMySQL <- function() {
  print("Connecting to DB ...")
  con_sql <- DBI::dbConnect (drv = RMariaDB::MariaDB(), 
                             dbname = dbCredentials$name,
                             host = dbCredentials$hostname,
                             port = 3306,
                             username = dbCredentials$username,
                             password = dbCredentials$password
  )
  
  
  
  ibrary("jsonlite")
  
  getVolumeDir <- function(volumeName)
  {
    json <- Sys.getenv("VCAP_SERVICES")
    if (json == '')
    {
      stop("Missing VCAP_SERVICES")
    }
    vcapServices <- fromJSON(json, simplifyVector = FALSE, simplifyDataFrame = FALSE)
    
    for (serviceType in vcapServices)
    {
      for (service in serviceType)
      {
        if (service$name == volumeName) {
          return ( fromJSON(toJSON(service))$volume_mounts$container_dir )
        }
      }
    }
  }
  
  getCredentials <- function(serviceName)
  {
    json <- Sys.getenv("VCAP_SERVICES")
    if (json == '')
    {
      stop("Missing VCAP_SERVICES")
      
    }
    vcapServices <- fromJSON(json, simplifyVector = FALSE, simplifyDataFrame = FALSE)
    
    for (serviceType in vcapServices)
    {
      for (service in serviceType)
      {
        if (service$name == serviceName) {
          return ( fromJSON(toJSON(service))$credentials )
        }
      }
    }
  }
  
  
  traceJsonNode <- function(name, node, printNode = FALSE) {
    
    if (printNode == TRUE)  {
      cat(paste(name, "[", class(node), "]", length(node), "(", node, ")\n"))
    }else {
      cat(paste(name, "[", class(node), "]", length(node), "\n"))
    }
  }
  
  
  
  
  dbCredentials <- getCredentials("recap_db")
  cat(paste(dbCredentials))
  cat(paste("host:", dbCredentials$hostname, "dbname:", dbCredentials$name))