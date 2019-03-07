#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(DT)
library(lubridate)
library(dbplyr)
library(pool)
library(shinyalert)


#DATABASE CONNECTION POOL
# Defines the parameters for connecting to the database 
recapdb <- dbPool(
  
  drv = RMySQL::MySQL(),
  dbname = "recap",
  host = "localhost",
  port = 3306,
  username = "recapuser",
  password = "!QAZ2wsx"
  
)

fleet_info <- recapdb %>%
  tbl("Fleet_Info") %>%
  dplyr::collect() 


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
                   selectInput("select_tail",
                               "Select Tail Number", 
                               choices = c(Choose="", as.list(fleet_info$Tail_Num)),
                               selected = "Sleepy",
                               selectize = TRUE),
                   #WIDGET FOR SELECTING DATES
                   dateRangeInput('select_date',
                                  label = 'Select Date',
                                  start = '1/1/1995', end = Sys.Date(),
                                  format = 'yyyy/m/d'),
                   actionButton("select_button", "View Log" )
                   
                   
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
                   selectInput("insert_tail",
                               "Select Tail Number", 
                               choices = c(Choose="", as.list(fleet_info$Tail_Num)),
                               selected = "Sleepy",
                               selectize = TRUE),
                   #WIDGET FOR SELECTING DATES
                   dateInput("insert_date", "Select Date: ", value = Sys.Date(), format = "m/d/yyyy"),
                   numericInput("hours_insert", label = "Enter # of Flight Hours:", value = 0, min = 0),
                   actionButton("insert_button", "Add to Log" )
                   
                   
                 )
                 
          ),  #END FIRST COLUMN SELECT TAB
              #BEGIN SECOND COLUMN SELECT TAB
          column( width = 8
                  
                  #datatableOutput
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
                   selectInput("update_tail",
                               "Select Tail Number", 
                               choices = c(Choose="", as.list(fleet_info$Tail_Num)),
                               selected = "Sleepy",
                               selectize = TRUE),
                   #WIDGET FOR SELECTING DATES
                   dateInput("update_date", "Select Date: ", value = Sys.Date(), format = "m/d/yyyy"),
                   numericInput("hours_update", label = "Enter # of Flight Hours:", value = 0, min = 0),
                   actionButton("update_button", "Edit Log" )
                   
                   
                 )
                 
          ),  #END FIRST COLUMN UPDATE TAB
          #BEGIN SECOND COLUMN UPDATE TAB
          column( width = 8
                  
                  #datatableOutput
          )
        )
      ), # END UPDATE TAB
      
      tabItem(
        tabName = "DELETE",
        h2("Delete Log Entry"),
        fluidRow(
          # BEGIN FIRST COLUMN in INSERT TAB
          column(width = 4,
                 wellPanel(
                   #WIDGET FOR SELECTING AIRCRAFT
                   selectInput("delete_tail",
                               "Select Tail Number", 
                               choices = c(Choose="", as.list(fleet_info$Tail_Num)),
                               selected = "Sleepy",
                               selectize = TRUE),
                   #WIDGET FOR SELECTING DATES
                   dateInput("delete_date", "Select Date: ", value = Sys.Date(), format = "m/d/yyyy"),
                   actionButton("delete_button", "Add to Log" )
                   
                   
                 )
                 
          ),  #END FIRST COLUMN DELETE TAB
          #BEGIN SECOND COLUMN DELETE TAB
          column( width = 8
                  
                  #datatableOutput
          )
        )
      )
    )
  ) # END BODY CONTENT
)

server <- function(input, output) { 
  
    observeEvent(input$select_button, {
      
      #showNotification("Query Sent")
       # showModal(modalDialog(title = "Important","Important Message"))
      #shinyalert("Ooops!!!", "Something went wrong", type = "error")
    })
  
    #Reacts the action button on the INSERT tab. Takes values from choices, sends as SQL query
    select_button_click <- eventReactive( input$select_button,  {
      
      
   df <- recapdb %>% 
        tbl(input$select_tail) %>% collect()
        
        
        df %>%
          mutate(Date = mdy(Date)) %>%
          filter( Date >= ymd( input$select_date[1]) & Date <= ymd(input$select_date[2]))
        
         
        
        }) 
    
    #end select button click
    
    # OUTPUT for table in SELECT Tab
    output$select_tab <- renderDataTable(
      select_button_click()
      )
  
  # Disconnects Database pool instance
  onStop(function() {
    poolClose(recapdb)
  })
  
  }

shinyApp(ui, server)

