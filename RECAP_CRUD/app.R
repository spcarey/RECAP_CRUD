

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
library(httr)





if (interactive()) {
  # testing url
  options(shiny.port = 8100)
  APP_URL <- "http://localhost:8100/"
} else {
  # deployed URL
  APP_URL <- "https://recapdatabase.apps.np.first-light.io:4443/"
}

source("func.R")

VCAPSSO <- getCredentials_sso("VCAP_SERVICES")




app <- oauth_app("RECAP_DATABASE",
                 key = VCAPSSO$credentials.client_id,
                 secret = VCAPSSO$credentials.client_secret,
                 redirect_uri = APP_URL
)

# Here I'm using a canned endpoint, but you can create with oauth_endpoint()
api <- oauth_endpoint(authorize = "https://sso.login.sys.np.first-light.io/oauth/authorize",
                      access = "https://sso.login.sys.np.first-light.io/oauth/token" )


scope <- "openid"

# Shiny -------------------------------------------------------------------

has_auth_code <- function(params) {
  # params is a list object containing the parsed URL parameters. Return TRUE if
  # based on these parameters, it looks like auth codes are present that we can
  # use to get an access token. If not, it means we need to go through the OAuth
  # flow.
  return(!is.null(params$code))
}




VCAP <- getCredentials("VCAP_SERVICES")



dbargs <- list(drv = RMySQL::MySQL(), 
                         dbname = VCAP$credentials.name,
                         host =  VCAP$credentials.hostname,
                         port = 3306,
                         username = VCAP$credentials.username,
                         password = VCAP$credentials.password)

#dbargs <- list(drv = RMySQL::MySQL(), dbname = "recap", host = "127.0.0.1", port = 3306, username = "root", password = "!QAZ2wsx")

recapdb <- do.call(DBI::dbConnect, dbargs)

 
Fleet_Info <- recapdb %>% tbl("Fleet_Info") %>% collect()

DBI::dbDisconnect(recapdb)


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
#----------------START uiFunc----------------#

uiFunc <- function(req) {
  if (!has_auth_code(parseQueryString(req$QUERY_STRING))) {
    url <- oauth2.0_authorize_url(api, app, scope = scope)
    redirect <- sprintf("location.replace(\"%s\");", url)
    tags$script(HTML(redirect))
  } else {
    ui
  }
}



#-----------------START SERVER Function----------------#

server <- function(input, output, session) { 
  
  
  
    #Reacts the action button on the INSERT tab. Takes values from choices, sends as SQL query
    select_button_click <- eventReactive( input$select_button,  {
      
      recapdb <- do.call(DBI::dbConnect, dbargs)
      on.exit( DBI::dbDisconnect(recapdb))
      
   df <- recapdb %>% 
        tbl("fleet_data") %>%  filter(Tail_Num == input$select_tail) %>% collect()
        
        
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
      
      datey <- (recapdb %>% tbl("fleet_data") %>% filter(Tail_Num == input$insert_tail) %>% select("Date") %>% collect() %>% as.list() )
      datey <- mdy(datey[[1]]) %>% as.character()
      
      if(as.character(input$insert_date) %in% datey){
        
        shinyalert("Ooops!!!", "This Date already has a Record", type = "error")
       
        
      } else {
        date <- as.character( format(input$insert_date, "%m/%d/%Y"))
       sql_code <- paste0("INSERT INTO"," ","fleet_data"," ","VALUES"," ","(","\'",input$insert_tail,"\',","\'",date,"\',","\'",input$insert_hours,"\'",","," ","\'",input$insert_opnhrs,"\'",")")
       #print(sql_code)
       dbGetQuery(recapdb, sql_code)
       output$insert_data <- renderDataTable({
         
         recapdb <- do.call(DBI::dbConnect, dbargs)
         on.exit(DBI::dbDisconnect(recapdb))
         
         recapdb %>% tbl("fleet_data") %>% filter(Tail_Num == input$insert_tail) %>% collect() %>% mutate(Date = mdy(Date)) %>% arrange(desc(Date))
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
      datey <- (recapdb %>% tbl("fleet_data") %>%  filter(Tail_Num == input$update_tail) %>% select("Date") %>% collect() %>% as.list() )
      
      #coerce date list from above into a character list for use as a condition in the IF statement
      datey <- mdy(datey[[1]]) %>% as.character()
      
      #IF DATE INPUT IS IN DATABASE PROCEED WITH UPDATE, ELSE SHOW ERROR POPUP
      if(as.character(input$update_date) %in% datey){ 
        
        date <- as.character( format(input$update_date, "%m/%d/%Y"))
        
        #Build SQL Statement
        sql_code <- paste0("UPDATE"," ","fleet_data"," ","SET"," ","Tail_Num = "," ","\'", input$update_tail, "\'",",","Hours ="," ","\'", input$update_hours, "\'"," ",",","Opn_Hrs ="," ","\'", input$update_hours, "\'"," ","WHERE Date ="," ","\'",date,"\'",";")
        print(sql_code)
        #send SQL Statement
        dbGetQuery(recapdb, sql_code)
        
        #Create Output object Table that shows updated entry
        output$update_data <- renderDataTable({
          
          recapdb <- do.call(DBI::dbConnect, dbargs)
          on.exit(DBI::dbDisconnect(recapdb))
          
          recapdb %>%  tbl("fleet_data") %>% filter(Tail_Num == input$update_tail,
                                                    Date == date) %>% collect()
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
      datey <- (recapdb %>% tbl("fleet_data") %>% filter(Tail_Num == input$delete_tail) %>% select("Date") %>% collect() %>% as.list() )
      
      #coerce date list from above into a character list for use as a condition in the IF statement
      datey <- mdy(datey[[1]]) %>% as.character()
      
      #IF DATE INPUT IS IN DATABASE PROCEED WITH UPDATE, ELSE SHOW ERROR POPUP
      if(as.character(input$delete_date) %in% datey){ 
        
         date <- as.character( format(input$delete_date, "%m/%d/%Y"))
        
        #Build SQL Statement
        sql_code <- paste0("DELETE FROM"," ","fleet_data"," ","WHERE"," ","Tail_Num =","\'", input$delete_tail, "\'"," ","AND"," ","Date ="," ","\'", date, "\'",";")
        print(sql_code)
        #send SQL Statement
        dbGetQuery(recapdb, sql_code)
        
        
        
        #Create Output object Table that shows updated entry
        output$delete_data <- renderDataTable({
          
          recapdb <- do.call(DBI::dbConnect, dbargs)
          on.exit(DBI::dbDisconnect(recapdb))
          
          recapdb %>%
            tbl("fleet_data") %>%  filter(Tail_Num == input$delete_tail) %>%
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

shinyApp(ui = uiFunc , server = server) # Runs app by calling the ui and server functions

