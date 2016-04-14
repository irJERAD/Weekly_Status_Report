library(shiny)
library(shinydashboard)


header <- dashboardHeader(title = "Weekly Status Reports")
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Team Ratings", tabName = "teamRatings", icon = icon("group")),
    menuItem("Input Your Rating", tabName = "inputRating", icon = icon("edit")),
    menuItem("Raw Data", tabName = "rawData", icon = icon("table"))
  )
)
body <- dashboardBody(
  tabItems(
    tabItem("teamRatings"),
    tabItem("inputRating",
            fluidRow(
              box(title = "Project Status", width = 12,
                  # User input for Project Name, Role, Rating and Date
                  column(width = 4,
                         textInput(inputId = "projectName", label = "Project Name"),
                         selectInput(inputId = "role", label = "Your Role:", 
                                     choices = list("Account Manager" = 1,
                                                    "Project Manager" = 2,
                                                    "Technical Lead" = 3,
                                                    "Quality Assurance" = 4)
                                     ),
                         selectInput(inputId = "rating", label = "Your Rating:", 
                                     choices = list("Green" = 1,
                                                    "Yellow" = 2,
                                                    "Red" = 3)
                                     )
                         ),
                  # Render Color circle image for rating
                  column(width = 2, offset = 2,
                    imageOutput("rating1", height = "auto")
                    )
                  )
              ),
            fluidRow(
              box(title = "Project Summary", width = 12,
                  tags$textarea(id="summary", rows=8, cols="100",
                                placeholder = "This week in our project...")
                  )
            )
            ),
    tabItem("rawData")
    )
  )

ui <- dashboardPage(header, sidebar, body, skin = "blue")

server <- function(input, output) {
  # rating1 sends pre-rendered png
  output$rating1 <- renderImage({
    if (is.null(input$rating))
      return(NULL)
    
    if (input$rating == 1) {
      return(list(
        src = "images/green.png",
        contentType = "image/png",
        alt = "Green"
      ))
    } else if (input$rating == 2) {
      return(list(
        src = "images/yellow.png",
        filetype = "image/png",
        alt = "Yellow"
      ))
    } else if (input$rating == 3) {
      return(list(
        src = "images/red.png",
        filetype = "image/png",
        alt = "Red"
      ))
    }
    
  }, deleteFile = FALSE)
  
}

shinyApp(ui, server)