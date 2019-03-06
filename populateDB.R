library(pool)
library(dplyr)
library(readr)

recapdb <- dbPool(
   
   drv = RMySQL::MySQL(),
   dbname = "recap",
   host = "localhost",
   port = 3306,
   username = "recapuser",
   password = "!QAZ2wsx"
   
)

Sleepy <- read_csv("Sleepy_Usage_Report.csv")
Doc <- read_csv("Doc_Usage_Report.csv")
Grupmy <- read_csv("Grumpy_Usage_Report.csv")
Sneezy <- read_csv("Sneezy_Usage_Report.csv")
Fleet_Info <- read_csv("Fleet_Info.csv")


copy_to(
   dest = recapdb,
   name = "Sleepy",
   df = Sleepy,
   temporary = FALSE,
   overwrite = TRUE
   ) 

copy_to(
   dest = recapdb,
   name = "Grumpy",
   df = Grumpy,
   temporary = FALSE,
   overwrite = TRUE
) 

copy_to(
   dest = recapdb,
   name = "Doc",
   df = Doc,
   temporary = FALSE,
   overwrite = TRUE
) 

copy_to(
   dest = recapdb,
   name = "Sneezy",
   df = Sneezy,
   temporary = FALSE,
   overwrite = TRUE
) 

copy_to(
   dest = recapdb,
   name = "Fleet_Info",
   df = Fleet_Info,
   temporary = FALSE,
   overwrite = TRUE
) 

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

