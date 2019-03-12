library(shinydashboard)
library(leaflet)
library(lubridate)
library(billboarder)
library(readr)
library(stringr)



library(shiny)


tweets <- read_csv("https://raw.githubusercontent.com/jdpersona/shiny_deploy/master/dashboard_data_final_v3_test.csv", col_names = TRUE)

tweets <- tweets[, c(2:length(colnames(tweets)))]


tweets$created_at = str_trim(string = substr(tweets$created_at, start = 1, stop=10), side='both')

tweets$created_at = mdy(tweets$created_at)

tweets$created_at = as.Date(tweets$created_at, "dd/mm/yy")



colorByCategory <- colorFactor(palette = c("green", "blue", "red",
                                           "yellow", "brown", "darkorchid1",
                                           "darkseagreen1", "purple", "orange","hotpink", "khaki"),
                               level = c("Personal","Alert: Down Trees", "Alert: Flooding", "Alert: Misc.", 
                                         "Alert: Power Outage", "Alert: Road Closure", "Alert: Structural Damage", 
                                         "News", "Request", "Supply Center", "Update: Power On") )



header <- dashboardHeader(
  title = "Hurricane command center", titleWidth = 400
)

body <- dashboardBody(

  dateRangeInput('date',
                 label = NULL,
                 start = "2012-10-12", end = "2018-10-20",
                 max = "2018-10-20",
                 separator = " - ", format = "dd/mm/yy",
                 startview = "month",language = "en", weekstart = 1
  ),
  
  fluidRow(
    
    
    
    
    column(width = 12,
           box(width = NULL, solidHeader = TRUE,
               
               
               leafletOutput("busmap", height = 400)
         
               
                )
           
           
           
       
   
           
       ),
    
    
    
    fluidRow(
      
      
      column(
        width = 8, 
        
        billboarderOutput('graph', height = 350)
        
        
      ),
      
      
      column(
        width = 4, 
        
        billboarderOutput("predicted_freq", height = 370)
        
        
      )
      
      
      
      

    )

  )
)

dashboardPage(
  header,
  dashboardSidebar(disable = TRUE),
  body
)
