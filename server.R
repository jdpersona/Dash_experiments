
library(shinydashboard)
library(shiny)
library(leaflet)
library(dplyr)
library(highcharter)
library(lubridate)
library(stringr)
library(readr)
library(billboarder)
library(stringr)




# Define server logic required to draw a histogram


server <- function(input, output, session) {
  
  
  
  newtweets <- reactive({
    filter(tweets, between(created_at ,input$date[1], input$date[2]))
   # tweets[tweets$created_at >= input$date[1] & tweets$created_at <= input$date[2], ]
  })
  
  
  output$busmap <- renderLeaflet({
    
    newtweets() %>%
      leaflet() %>%
      addTiles() %>% 
      #addProviderTiles(provider = "OpenStreetMap.BlackAndWhite") %>%  # Change map style
      setView(lat=30.1588, lng=-85.6602, zoom=14) %>%
      addCircleMarkers(lat = ~lat, lng = ~lon, popup = paste(newtweets()$text, "<br>", 
                                                             "lat: ", newtweets()$lat, "<br>",
                                                             "lon: ", newtweets()$lon), radius = 5, 
                       color = ~colorByCategory(predicted_type), 
                       group=~predicted_type#, 
                       #clusterOptions = markerClusterOptions() #Clusters
      ) %>%
      
      
      addLegend(pal = colorByCategory, values = c("Personal", "Alert: Down Trees", "Alert: Flooding", "Alert: Misc.", 
                                                  "Alert: Power Outage", "Alert: Road Closure", "Alert: Structural Damage", 
                                                  "News",  "Request", "Supply Center", "Update: Power On"), title = "Category",position = 'bottomleft', opacity = 0.3) %>%
      addLayersControl(overlayGroups = c("Personal", "Alert: Down Trees", "Alert: Flooding", "Alert: Misc.", 
                                         "Alert: Power Outage", "Alert: Road Closure", "Alert: Structural Damage", 
                                         "News",  "Request", "Supply Center", "Update: Power On")) 
    
    
  })
  
  
  
  output$predicted_freq <- renderBillboarder ({
    #tweets = tweets[tweets$created_at >= input$ate_range[1] & tweets$created_at <= input$date_range[2], ]
    #tweets = tweets[tweets$created_at >= '2012-11-02'  & tweets$created_at <= '2016-10-09', ]
    
  test = newtweets() %>% group_by(predicted_type) %>% count()
    
    billboarder() %>%
      bb_lollipop(data = test,rotated = TRUE,point_color = '#AAD3DF', point_size = 12)
    
    
  }) 
  
  
  
  
  
  output$graph <- renderBillboarder({
    

    test = newtweets() %>% group_by(created_at) %>% count()
   
    billboarder() %>%
      bb_lollipop(data = test,point_color = '#AAD3DF',point_size = 12)
    
    
    
 } )
  
  

  
}





