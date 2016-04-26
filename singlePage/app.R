# for a multifile system with a global file use below line
#source('global.R', local = TRUE)

library(shiny)
library(shinydashboard)
library(googlesheets)
library(DT)

## Global

## Global Options
options("googlesheets.webapp.client_secret" = "9CxTmfVIljTHSmjz8IfjoOIx")
options("googlesheets.webapp.redirect_uri" = "https://irjerad.shinyapps.io/Weekly-Status-Report/")
options("googlesheets.webapp.client_id" = "575772486822-gfrlu7mocg3roq58rtgrsp8taq1tn0hd.apps.googleusercontent.com")
## Global Variables

# define current projects

# define roles
roleList <- list("Account Manager", "Project Manager",
                 "Technical Lead", "Quality Assurance")
# define project names
projectNames <- list("HMH", "LAC", "SVB", "Empower", "Ebay", "Geico")
## Global Functions

# name of google sheet being used
table <- "practiceWSR"

saveData <- function(data) {
  # get google sheet
  sheet <- gs_title(table)
  # add new row
  gs_add_row(sheet, input = data)
}

loadData <- function() {
  # get google sheet
  sheet <- gs_title(table)
  # read the data
  gs_read_csv(sheet)
}

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

today <- function(gsTBL) {
  # subset input gsTBL only returning today's inputs
  gsTBL[isToday(gsTBL$timeStamp),]
}


header <- dashboardHeader(title = "Weekly Status Reports",
                          uiOutput("loginButton")
                          )
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
                  title = "Summaries", htmlOutput("gSummary")
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
                         selectInput(inputId = "projectName", label = "Project Name:",
                                     choices = projectNames),
                         dateInput(inputId = "date", label = "Date:", format = "m-d-yyyy")
                         ),
                  # User input for Role and Rating color
                  column(width = 4,
                         selectInput(inputId = "role", label = "Your Role:", 
                                     choices = roleList
                                     ),
                         selectInput(inputId = "rating", label = "Your Rating:", 
                                     choices = list("Green",
                                                    "Yellow",
                                                    "Red")
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

server <- function(input, output, session) {
  
  # get summary info from googlesheet
  output$gSummary <- renderUI({
    # grab table from google sheets
    tbl <- loadData()
    # grab table of today's entries
    todayTBL <- today(tbl)
    
    todayTBL$summary
    
    txt <- paste("<b>Project:</b>", todayTBL$projectName,
                 "<b>Role:</b>", todayTBL$role, 
                 "<b>Rating:</b>", todayTBL$rating, 
                 "<b>Summary:</b>", todayTBL$summary,
                 "<br/>", "<br/>", sep = " ")
    
    HTML(txt)
    
  })
  
  # rating sends colored circle png based on rating response 
  output$rating <- renderImage({
    # Method for rendering pic without global.R function:
    if (is.null(input$rating))
      return(NULL)

    if (input$rating == "Green") {
      return(
        list(
          src = "images/green.png",
          contentType = "image/png",
          alt = "Green"
          )
        )
    } else if (input$rating == "Yellow") {
      return(
        list(
          src = "images/yellow.png",
          filetype = "image/png",
          alt = "Yellow"
          )
        )
    } else if (input$rating == "Red") {
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
    tbl <- loadData()
    # render table as server output to be used in the ui
    tbl
  })

  # Add new row based on user input after pressing submit
  observeEvent(input$submit, {
    
    # cast inpust$date as character or all following input will look for date objects
    date <- as.character(input$date)
    data = c(date, input$projectName, input$role,input$rating, input$summary)
    # update google sheet
    saveData(data)
    
    # # grab table from google sheets
    tbl <- loadData()
    # update raw data view
    output$WSRtbl <- renderDataTable(tbl)
    
    ## TODO place renderUI with abstracted Project Summaries into HTML for auto update
    
  })
  
  ## Get auth code from return URL
  access_token  <- reactive({
    ## gets all the parameters in the URL. The auth code should be one of them.
    pars <- parseQueryString(session$clientData$url_search)
    
    if(length(pars$code) > 0) {
      ## extract the authorization code
      gs_webapp_get_token(auth_code = pars$code)
    } else {
      NULL
    }
  })
  
  ## Make a button to link to Google auth screen
  ## If auth_code is returned then don't show login button
  output$loginButton <- renderUI({
    if(is.null(isolate(access_token()))) {
      actionButton("loginButton",
                   label = a("Authorize App",
                             href = gs_webapp_auth_url()))
    } else {
      return()
    }
  })

}

shinyApp(ui, server)