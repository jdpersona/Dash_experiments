#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shinythemes)
library(shiny)
library(plotly)
library(ggplot2)
library(dplyr)
library(stringr)
library(tidyr)
library(data.table)
library(magrittr)
library(highcharter)


data = read.csv('/Users/john.ekedum@ibm.com/Documents/Data Visualization/Shiny Apps/SAS RESponse/all_data.csv')


# there are 319 dataset
data %>% 
  count()

# there are 10 columns
length(colnames(data))

# convert character date to real dates
data$Begin_Date = as.Date(data$Begin_Date, "%m/%d/%y")
data$End_Date = as.Date(data$End_Date, "%m/%d/%y")

sub_data = data %>% 
  select(Begin_Date, url, form_shown, form_submitted) %>% 
  group_by(Begin_Date, url) %>% summarise (Total_form_shown = sum(form_shown), Total_form_submitted = sum(form_submitted)) %>% 
  arrange(desc(url))

sub_data$Conversion_rate =  round((sub_data$Total_form_submitted/sub_data$Total_form_shown) * 100, digits=0)

sub_data$url = str_trim(sub_data$url)




# Define UI for application that draws a histogram
ui <- fluidPage(
                
                shinythemes::themeSelector(),
  
  # Application title
  titlePanel("SaaS Response",windowTitle = "SaaS Exploration"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      width = 3,
      selectInput(inputId ="choose", 
                  choices = c("Watson Campaign Automation","tealeaf","digitalexperience","digitalanalytics"),    
                  selected = "Watson Campaign Automation",label = "Choose a SaaS Product"),
      br(),
      br(),
      br()
     
    
 
      
    ),
    
    
    
    # Show a plot of the generated distribution
    mainPanel(
      
      tabsetPanel(
        tabPanel("Graphs", highchartOutput("plot",height = 400, width = 1000),
                 br(),
                 highchartOutput("plot2",height = 400, width = 1000),
                 br(),
                 highchartOutput("plot3",height = 400, width = 1000),
                 br(),
                 DT :: dataTableOutput('table')
                 
                 
                 
        ),
        
       tabPanel("Agregated Data", DT :: dataTableOutput('table2') )
                 
      )
    )
    
  )
)



# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$plot <- renderHighchart({
    
    df_WCA = sub_data[sub_data$url == input$choose,] 
    
    highchart() %>% 
      hc_title(text = "Total Form Submitted") %>% 
      hc_add_series_times_values(df_WCA$Begin_Date,df_WCA$Total_form_submitted)
    
  })
  

  output$plot2 <- renderHighchart({
    
    df_WCA = sub_data[sub_data$url == input$choose, ] 
    
    highchart() %>% 
      hc_title(text = "Total Form Shown") %>% 
      hc_add_series_times_values(df_WCA$Begin_Date,df_WCA$Total_form_shown)
  })
  
  
  output$plot3 <- renderHighchart({
    
    df_WCA = sub_data[sub_data$url == input$choose, ] 
    
    highchart() %>% 
      hc_title(text = "Conversion Rate") %>% 
      hc_add_series_times_values(df_WCA$Begin_Date,df_WCA$Conversion_rate)
    
  })
  
  
  mytable <-  reactive({
    df_WCA = sub_data[sub_data$url == input$choose, ]
    
    total_growth = round(df_WCA[df_WCA$Begin_Date == max(df_WCA$Begin_Date),]['Conversion_rate']-df_WCA[df_WCA$Begin_Date == min(df_WCA$Begin_Date),]['Conversion_rate'],0)
    avg_conversion = round(sum(df_WCA$Conversion_rate)/length(df_WCA$Conversion_rate),0)
    avg_form_sub = sum(df_WCA$Total_form_submitted)/length(df_WCA$Total_form_submitted)
    avg_form_shown  = sum(df_WCA$Total_form_shown)/length(df_WCA$Total_form_shown)
    test = data.frame( "Rate" = c(avg_form_shown, avg_form_sub, paste(avg_conversion,"percent"), paste(total_growth$Conversion_rate,"percent")), row.names = c('Average Form Shown', 'Average Form Submitted', 'Average Conversion rates', 'Total Growth') )
    test
  })  
  
  
  
  output$table <- DT :: renderDataTable({           
    
    DT::datatable(mytable(), rownames = TRUE, options = list(pageLength =4))
  }, server=TRUE)
  
  
  output$table2 <- DT :: renderDataTable({           
    
    DT::datatable(sub_data[sub_data$url == input$choose, ], rownames = TRUE, options = list(pageLength =10))
  
    }, server=TRUE)
  

  
}

# Run the application 
shinyApp(ui = ui, server = server)

