#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
library(shinyauthr)
library(shinyjs)
library(ggplot2)


# dataframe that holds usernames, passwords and other user data
user_base <- data.frame(
  user = c("user1", "user2"),
  password = c("pass1", "pass2"), 
  permissions = c("admin", "standard"),
  name = c("User One", "User Two"),
  stringsAsFactors = FALSE
)

data <- data.frame(
  name=letters[1:5],
  value=sample(seq(4,15),5),
  sd=c(1,0.2,3,2,4)
)



ui <- fluidPage(theme = 'cosmo',
  # must turn shinyjs on
  shinyjs::useShinyjs(),
  # add logout button UI 
  div(class = "pull-right", shinyauthr::logoutUI(id = "logout") ),

  # add login panel UI function
  div(class ="log",
  shinyauthr::loginUI(id = "login")
  ),
  # setup table output to show user info after login
  tableOutput("user_table"),
  plotOutput('ggdisplay')
  
)

server <- function(input, output, session) {
  
  # call the logout module with reactive trigger to hide/show
  logout_init <- callModule(shinyauthr::logout, 
                            id = "logout", 
                            active = reactive(credentials()$user_auth))
  
  # call login module supplying data frame, user and password cols
  # and reactive trigger
  credentials <- callModule(shinyauthr::login, 
                            id = "login", 
                            data = user_base,
                            user_col = user,
                            pwd_col = password,
                            log_out = reactive(logout_init()))
  
  # pulls out the user information returned from login module
  user_data <- reactive({credentials()$info})
  
  output$user_table <- renderTable({
    # use req to only render results when credentials()$user_auth is TRUE
    req(credentials()$user_auth)
    user_data()
  })
  
  
 output$ggdisplay <- renderPlot({
  # create dummy data

  req(credentials()$user_auth)
  ggplot(data) +
    geom_bar( aes(x=name, y=value), stat="identity", fill="skyblue", alpha=0.7) +
    geom_errorbar( aes(x=name, ymin=value-sd, ymax=value+sd), width=0.4, colour="orange", alpha=0.9, size=1.3)
  
 })
  
  
}


shinyApp(ui = ui, server = server)
