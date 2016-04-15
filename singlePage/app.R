# for a multifile system with a global file use below line
#source('global.R', local = TRUE)

library(shiny)
library(shinydashboard)
library(googlesheets)
library(DT)



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
                  # User input for Project Name and Date
                  column(width = 4,
                         textInput(inputId = "projectName", label = "Project Name"),
                         dateInput(inputId = "date", label = "Date:")
                         ),
                  # User input for Role and Rating color
                  column(width = 4,
                         selectInput(inputId = "role", label = "Your Role:", 
                                     choices = list("Account Manager" = "AM",
                                                    "Project Manager" = "PM",
                                                    "Technical Lead" = "TL",
                                                    "Quality Assurance" = "QA")
                                     ),
                         selectInput(inputId = "rating", label = "Your Rating:", 
                                     choices = list("Green" = "green",
                                                    "Yellow" = "yellow",
                                                    "Red" = "red")
                                     )
                         ),
                  # Render Color circle image for rating
                  column(width = 2, offset = 1,
                    imageOutput("rating", height = "auto")
                    )
                  )
              ),
            # Text input for project summary
            fluidRow(
              box(title = "Project Summary", width = 12,
                  tags$textarea(id="summary", rows=8, cols="100",
                                placeholder = "This week in our project..."),
                  actionButton(inputId = "submit", label = "Submit")
                  )
              )
            ),
    tabItem("rawData",
            # show practice weekly status report
            dataTableOutput('WSRtbl')
            )
    )
  )

ui <- dashboardPage(header, sidebar, body, skin = "blue")

server <- function(input, output) {
  
  # rating sends colored circle png based on rating response 
  output$rating <- renderImage({
    # Method for rendering pic without global.R function:
    if (is.null(input$rating))
      return(NULL)

    if (input$rating == "green") {
      return(
        list(
          src = "images/green.png",
          contentType = "image/png",
          alt = "Green"
          )
        )
    } else if (input$rating == "yellow") {
      return(
        list(
          src = "images/yellow.png",
          filetype = "image/png",
          alt = "Yellow"
          )
        )
    } else if (input$rating == "red") {
      return(
        list(
          src = "images/red.png",
          filetype = "image/png",
          alt = "Red"
          )
      )
    }
  }, deleteFile = FALSE)
  
  # grab table from google sheets
  sheet <- gs_title("practiceWSR")
  # consider gs_read(sheet); currently believe csv is faster - untested
  tbl <- gs_read_csv(sheet)
  # render table as server output to be used in the ui
  output$WSRtbl <- renderDataTable({
    tbl
  })

}

shinyApp(ui, server)