# for a multifile system with a global file use below line
#source('global.R', local = TRUE)

library(shiny)
library(shinydashboard)
library(googlesheets)
library(DT)

## Global Functions

removeLeadZero <- function(x) {
  # remove any leading month zero
  y <- gsub("^0", "\\1", x)
  # remove any leading day zero; '/' needs to be replaced
  gsub("/0", "/", y)
  
  ## TODO:
  # Consider making no input and just using today's date
  # since this function should only be used on Sys.Date() input
}

isToday <- function(inputDate) {
  removeLeadZero(format(Sys.Date(), "%m/%d/%Y")) == inputDate
}

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
    tabItem("teamRatings",
            fluidRow(
              tabBox(
                id = "teamSummaries", title = "Team Summaries", width = 12,
                tabPanel(
                  # this method requires browser to be refreshed for newer entries
                  title = "Summaries", textOutput("gSummary")
                ),
                tabPanel(
                  title = "Weekly Status Report OverView", 
                  "These are the teams that have submited their reports"
                )
              )
            )
            ),
    tabItem("inputRating",
            fluidRow(
              box(title = "Project Status", width = 12,
                  # User input for Project Name and Date
                  column(width = 4,
                         textInput(inputId = "projectName", label = "Project Name"),
                         dateInput(inputId = "date", label = "Date:", format = "m-d-yyyy")
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
                  tags$textarea(id="summary", rows=8, cols="110",
                                placeholder = "This week in our project..."),
                  actionButton(inputId = "submit", label = "Submit")
                  )
              )
            ),
    tabItem("rawData",
            # show practice weekly status report
            DT::dataTableOutput("WSRtbl")
            )
    )
  )
ui <- dashboardPage(header, sidebar, body, skin = "blue")

server <- function(input, output) {
  
  # get summary info from googlesheet
  output$gSummary <- renderText({
    # grab table from google sheets
    sheet <- gs_title("practiceWSR")
    # consider gs_read(sheet); currently believe csv is faster - untested
    tbl <- gs_read_csv(sheet)
    
    # grab last summary entry
    tail(tbl$summary, n = 1)
  })
  
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
  
  # Display current State of sheet data
  output$WSRtbl <- renderDataTable({
    # grab table from google sheets
    sheet <- gs_title("practiceWSR")
    # consider gs_read(sheet); currently believe csv is faster - untested
    tbl <- gs_read_csv(sheet)
    # render table as server output to be used in the ui
    tbl
  })

  # Add new row based on user input after pressing submit
  observeEvent(input$submit, {
    
    # cast inpust$date as character or all following input will look for date objects
    date <- as.character(input$date)
    gs_title("practiceWSR") %>% 
      gs_add_row(input = c(date, input$projectName, input$role, 
                           input$rating, input$summary))
    # grab table from google sheets
    sheet <- gs_title("practiceWSR")
    # consider gs_read(sheet); currently believe csv is faster - untested
    tbl <- gs_read_csv(sheet)
    output$WSRtbl <- renderDataTable(tbl)
  })
}

shinyApp(ui, server)