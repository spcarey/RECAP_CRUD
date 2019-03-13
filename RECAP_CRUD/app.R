

library(shiny)
library(shinydashboard)
library(DT)
library(lubridate)
library(dbplyr)
library(DBI)
library(shinyalert)
library(shinyWidgets)
library(dplyr)
library(jsonlite)


source("func.R")


VCAP <- getCredentials("VCAP_SERVICES")



dbargs <- list(drv = RMySQL::MySQL(), 
                         dbname = VCAP$instance_name,
                         host =  VCAP$credentials.hostname,
                         port = 3306,
                         username = VCAP$credentials.username,
                         password = VCAP$credentials.password)

print(dbargs)


#Sleepy <- read.csv("Sleepy_Usage_Report.csv", stringsAsFactors = FALSE)
#Sleepy$Date <- as.Date(Sleepy$Date, format = "%m/%d/%Y") %>% format("%m/%d/%Y") 
#Doc <- read.csv("Doc_Usage_Report.csv", stringsAsFactors = FALSE)
#Doc$Date <- as.Date(Doc$Date, format = "%m/%d/%Y") %>% format("%m/%d/%Y") 
#Grumpy <- read.csv("Grumpy_Usage_Report.csv", stringsAsFactors = FALSE)
#Grumpy$Date <- as.Date(Grumpy$Date, format = "%m/%d/%Y") %>% format("%m/%d/%Y") 
#Sneezy <- read.csv("Sneezy_Usage_Report.csv", stringsAsFactors = FALSE)
#Sneezy$Date <- as.Date(Sneezy$Date, format = "%m/%d/%Y") %>% format("%m/%d/%Y") 
Fleet_Info <- read.csv("Fleet_Info.csv", stringsAsFactors = FALSE)







#--------------------START UI FUNCTION----------------------------#
ui <- dashboardPage(
        dashboardHeader(title = "FLIGHT LOG"),
  dashboardSidebar(
    sidebarMenu( #DEFINE SIDE BAR NAV ICONS
      menuItem("VIEW LOG", tabName = "SELECT", icon = icon("database")),
      menuItem("ADD TO LOG", tabName = "INSERT", icon = icon("plus")),
      menuItem("UPDATE LOG ENTRY", tabName = "UPDATE", icon = icon("user-edit")),
      menuItem("DELETE LOG ENTRY", tabName = "DELETE", icon = icon("backspace"))
    )
  ),  #BEGIN BODY CONTENT
  dashboardBody(
    tabItems(
      # BEGIN SELECT TAB
      tabItem(
        tabName = "SELECT",
        h2("  View Flight Log"),
        useShinyalert(),
        fluidRow(
          # BEGIN FIRST COLUMN in SELECT TAB
          column(width = 4,
                 wellPanel(
                   #WIDGET FOR SELECTING AIRCRAFT
                   selectInput(inputId = "select_tail",
                               "Select Tail Number", 
                               choices = c(Choose="", as.list(Fleet_Info$Tail_Num)),
                               selected = "Sleepy",
                               selectize = TRUE),
                   #WIDGET FOR SELECTING DATES
                   airDatepickerInput(inputId = 'select_date1',
                                  label = 'Select Start Date',
                                  range = FALSE,
                                  value = ('1995-1-1'),
                                  multiple = FALSE
                                  ),
                   airDatepickerInput(inputId = 'select_date2',
                                      label = 'Select End Date',
                                      range = FALSE,
                                      multiple = FALSE,
                                      value = Sys.Date() 
                   ),
                   actionButton(inputId = "select_button", "View Log" )
                   
                   
                 )
            
          ),  #END FIRST COLUMN INSERT TAB
              #BEGIN SECOND COLUMN INSERT TAB
          column( width = 8,
                  
                 dataTableOutput("select_tab")
          )
        )
      ),# END SELECT TAB
        # BEGIN INSERT TAB
      tabItem(
        tabName = "INSERT",
        h2("Create New Log Entry"),
        fluidRow(
          # BEGIN FIRST COLUMN in INSERT TAB
          column(width = 4,
                 wellPanel(
                   #WIDGET FOR SELECTING AIRCRAFT
                   
                   
                
                   selectInput(inputId = "insert_tail",
                               "Select Tail Number", 
                               choices = c(Choose="", as.list(Fleet_Info$Tail_Num)),
                               selected = "Sleepy"
                               ),
                   #WIDGET FOR SELECTING DATES
                   airDatepickerInput(inputId = 'insert_date',
                                      label = 'Select Date of new Record:',
                                      range = FALSE,
                                      multiple = FALSE,
                                      value = Sys.Date()),
                   numericInput(inputId = "insert_hours", label = "Enter Total # of Flight Hours:", value = 0, min = 0),
                   numericInput(inputId = "insert_opnhrs", label = "Of the total flight hours how many were Operational Hours(DEPLOYED):", value = 0, min = 0),
                   actionButton(inputId = "insert_button", "Add to Log" )
                   
                   
                 )
                 
          ),  #END FIRST COLUMN SELECT TAB
              #BEGIN SECOND COLUMN SELECT TAB
          column( width = 8,
                  
                  dataTableOutput("insert_data")
          )
        )
      ), #END INSERT TAB
         #Begin UPDATE TAB
      
      tabItem(
        tabName = "UPDATE",
        h2("Update Previous Log Entry"),
        fluidRow(
          # BEGIN FIRST COLUMN in UPDATE TAB
          column(width = 4,
                 wellPanel(
                   #WIDGET FOR SELECTING AIRCRAFT
                   selectInput(inputId = "update_tail",
                               "Select Tail Number", 
                               choices = c(Choose="", as.list(Fleet_Info$Tail_Num)),
                               selected = "Sleepy",
                               selectize = TRUE),
                   #WIDGET FOR SELECTING DATES
                   airDatepickerInput(inputId = 'update_date',
                                      label = 'Select Record date to be updated:',
                                      range = FALSE,
                                      multiple = FALSE,
                                      value = Sys.Date()),
                   numericInput(inputId = "update_hours", label = "Enter Total # of Flight Hours:", value = 0, min = 0),
                   numericInput(inputId = "update_opnhrs", label =  "Of the total flight hours how many were Operational Hours(DEPLOYED):", value = 0, min = 0),
                   actionButton(inputId = "update_button", "Edit Log" )
                   
                   
                 )
                 
          ),  #END FIRST COLUMN UPDATE TAB
          #BEGIN SECOND COLUMN UPDATE TAB
          column( width = 8,
                  
                  dataTableOutput("update_data")
          )
        )
      ), # END UPDATE TAB
      
      tabItem(
        tabName = "DELETE",
        h2("Delete Log Entry"),
        fluidRow(
          # BEGIN FIRST COLUMN in DELETE TAB
          column(width = 4,
                 wellPanel(
                   #WIDGET FOR SELECTING AIRCRAFT
                   selectInput("delete_tail",
                               "Select Tail Number", 
                               choices = c(Choose="", as.list(Fleet_Info$Tail_Num)),
                               selected = "Sleepy",
                               selectize = TRUE),
                   #WIDGET FOR SELECTING DATES
                   airDatepickerInput(inputId = 'delete_date',
                                      label = 'Select Date of record to be removed:',
                                      range = FALSE,
                                      multiple = FALSE,
                                      value = Sys.Date()),
                   actionButton("delete_button", "Delete Log Entry" )
                   
                   
                 )
                 
          ),  #END FIRST COLUMN DELETE TAB
          #BEGIN SECOND COLUMN DELETE TAB
          column( width = 8,
                  
                  dataTableOutput("delete_data")
          )
        )
      )
    )
  ) # END BODY CONTENT
)


#-----------------START SERVER Function----------------#

server <- function(input, output) { 
  
  
  
    #Reacts the action button on the INSERT tab. Takes values from choices, sends as SQL query
    select_button_click <- eventReactive( input$select_button,  {
      
      recapdb <- do.call(DBI::dbConnect, dbargs)
      on.exit( DBI::dbDisconnect(recapdb))
      
   df <- recapdb %>% 
        tbl(input$select_tail) %>% collect()
        
        
        df %>%
          mutate(Date = mdy(Date)) %>%
          filter( Date >= ymd( input$select_date1 ) & Date <= ymd(input$select_date2))
       
        }) 
    
    #end select button click
    
    #___________________________________
    
    # OUTPUT for table in SELECT Tab
    output$select_tab <- renderDataTable({
      
      select_button_click()
      
      })
    
    
    
    #______________INSERT TAB_______________________________#   
    #Reacts to button click on INSERT Tab
    
    
    observeEvent(input$insert_button, {
      
      recapdb <- do.call(DBI::dbConnect, dbargs)
      on.exit(DBI::dbDisconnect(recapdb))
      
      datey <- (recapdb %>% tbl(input$insert_tail) %>% select("Date") %>% collect() %>% as.list() )
      datey <- mdy(datey[[1]]) %>% as.character()
      
      if(as.character(input$insert_date) %in% datey){
        
        shinyalert("Ooops!!!", "This Date already has a Record", type = "error")
       
        
      } else {
        date <- as.character( format(input$insert_date, "%m/%d/%Y"))
       sql_code <- paste0("INSERT INTO"," ",input$insert_tail," ","VALUES"," ","(","\'",date,"\',","\'",input$insert_hours,"\'",","," ","\'",input$insert_opnhrs,"\'",")")
       #print(sql_code)
       dbGetQuery(recapdb, sql_code)
       output$insert_data <- renderDataTable({
         
         recapdb <- do.call(DBI::dbConnect, dbargs)
         on.exit(DBI::dbDisconnect(recapdb))
         
         recapdb %>% tbl(input$insert_tail) %>% collect() %>% mutate(Date = mdy(Date)) %>% arrange(desc(Date))
         })
       shinyalert("Success!", "New log entry added", type = "success")
       
      
       
      }
      
    })# end observe event for INSERT TAB
    
    #_________________________UPDATE TAB_______________________________# 
    
    #Reacts to button click on UPDATE Tab
    observeEvent(input$update_button, {
      
      recapdb <- do.call(DBI::dbConnect, dbargs)
      on.exit(DBI::dbDisconnect(recapdb))
      
      #get date column, coerce to date list for use in IF statement
      datey <- (recapdb %>% tbl(input$update_tail) %>% select("Date") %>% collect() %>% as.list() )
      
      #coerce date list from above into a character list for use as a condition in the IF statement
      datey <- mdy(datey[[1]]) %>% as.character()
      
      #IF DATE INPUT IS IN DATABASE PROCEED WITH UPDATE, ELSE SHOW ERROR POPUP
      if(as.character(input$update_date) %in% datey){ 
        
        date <- as.character( format(input$update_date, "%m/%d/%Y"))
        
        #Build SQL Statement
        sql_code <- paste0("UPDATE"," ",input$update_tail," ","SET"," ","Hours ="," ","\'", input$update_hours, "\'"," ",",","Opn_Hrs ="," ","\'", input$update_hours, "\'"," ","WHERE Date ="," ","\'",date,"\'",";")
        print(sql_code)
        #send SQL Statement
        dbGetQuery(recapdb, sql_code)
        
        #Create Output object Table that shows updated entry
        output$update_data <- renderDataTable({
          
          recapdb <- do.call(DBI::dbConnect, dbargs)
          on.exit(DBI::dbDisconnect(recapdb))
          
          recapdb %>%  tbl(input$update_tail) %>% filter(Date == date) %>% collect()
                                              })
        #popup notification to show success
        shinyalert("Success!", "Previous Log entry updated", type = "success")
        
        
      } else {
        
        #ERROR POPUP WINDOW
        shinyalert("Ooops!!!", "This Date doesn't have a record", type = "error")
        
       
      }
      
    })# end observe event for UPDATE TAB
    
    
  #______________DELETE TAB_______________________________#
    #Reacts to button click on DELETE Tab
    observeEvent(input$delete_button, {
      
      recapdb <- do.call(DBI::dbConnect, dbargs)
      on.exit(DBI::dbDisconnect(recapdb))
      
      #get date column, coerce to date list for use in IF statement
      datey <- (recapdb %>% tbl(input$delete_tail) %>% select("Date") %>% collect() %>% as.list() )
      
      #coerce date list from above into a character list for use as a condition in the IF statement
      datey <- mdy(datey[[1]]) %>% as.character()
      
      #IF DATE INPUT IS IN DATABASE PROCEED WITH UPDATE, ELSE SHOW ERROR POPUP
      if(as.character(input$delete_date) %in% datey){ 
        
         date <- as.character( format(input$delete_date, "%m/%d/%Y"))
        
        #Build SQL Statement
        sql_code <- paste0("DELETE FROM"," ",input$delete_tail," ","WHERE"," ","Date ="," ","\'", date, "\'",";")
        print(sql_code)
        #send SQL Statement
        dbGetQuery(recapdb, sql_code)
        
        
        
        #Create Output object Table that shows updated entry
        output$delete_data <- renderDataTable({
          
          recapdb <- do.call(DBI::dbConnect, dbargs)
          on.exit(DBI::dbDisconnect(recapdb))
          
          recapdb %>%
            tbl(input$delete_tail) %>%
            collect()
                                             })
        #popup notification to show success
        shinyalert("Success!", "Log entry Removed", type = "success")
        
        
      } else {
        
        #ERROR POPUP WINDOW
        shinyalert("Ooops!!!", "This Date doesn't have a record", type = "error")
        
       
      }
      
    })# end observe event for DELETE TAB
    
    
    
  #___________________END TABS___________________#  
    
   #Disconnects Database pool instance
  #onStop(function() {
    #poolClose(recapdb)
  #})
  
  }
#__________________END SERVER____________________#

shinyApp(ui, server) # Runs app by calling the ui and server functions

