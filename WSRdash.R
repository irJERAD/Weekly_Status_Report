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
                  column(width = 4,
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
                         )
                  )
            )),
    tabItem("rawData")
  )
)

ui <- dashboardPage(header, sidebar, body, skin = "blue")

server <- function(input, output) { }

shinyApp(ui, server)