# for a multifile system with a global file use below line
#source('global.R', local = TRUE)

library(shiny)
library(shinydashboard)
library(googlesheets)
library(DT)
suppressMessages(library(dplyr))

## Global

## Global Options
options("googlesheets.webapp.client_secret" = "9CxTmfVIljTHSmjz8IfjoOIx")
options("googlesheets.webapp.redirect_uri" = "https://irjerad.shinyapps.io/Weekly-Status-Report/")
options("googlesheets.webapp.client_id" = "575772486822-gfrlu7mocg3roq58rtgrsp8taq1tn0hd.apps.googleusercontent.com")
## Global Variables

# define roles
roleList <- list("Account Manager", "Project Manager",
                 "Technical Lead", "Quality Assurance")
# define project names
projectNames <- list("HMH", "LAC", "SVB", "Empower", "Ebay", "Geico", "Weekly Status Report")
## Global Functions

# name of google sheet being used
table <- "practiceWSR"

# a function to append data to the bottom row of google sheet 'sheet'
saveData <- function(data) {
  # get google sheet
  sheet <- gs_title(table)
  # add new row
  gs_add_row(sheet, input = data)
}

# a function to load data from the google sheet 'sheet' and return a csv
loadData <- function() {
  # get google sheet
  sheet <- gs_title(table)
  # read the data
  gs_read_csv(sheet)
}

# removes leading zero from month and day digits in order to match stings with google sheets format
removeLeadZero <- function(x) {
  # remove any leading month zero
  y <- gsub("^0", "\\1", x)
  # remove any leading day zero; '/' needs to be replaced
  gsub("/0", "/", y)
  
  ## TODO:
  # Consider making no input and just using today's date
  # since this function should only be used on Sys.Date() input
}

# checks if inputDate is today; and thus should be used for digest
isToday <- function(inputDate) {
  removeLeadZero(format(Sys.Date(), "%m/%d/%Y")) == inputDate
}

# subset gsTBL to only todays values based on timeStamp value
today <- function(gsTBL) {
  # subset input gsTBL only returning today's inputs
  gsTBL[isToday(gsTBL$timeStamp),]
}

# selects a color based on the rating
ratingPic <- function(rating) {
  if (is.null(rating))
    return(NULL)
  
  if (rating == "Green") {
    return(list(
      src = "images/green.png",
      contentType = "image/png",
      alt = "Green"
    ))
  } else if (rating == "Yellow") {
    return(list(
      src = "images/yellow.png",
      filetype = "image/png",
      alt = "Yellow"
    ))
  } else if (rating == "Red") {
    return(list(
      src = "images/red.png",
      filetype = "image/png",
      alt = "Red"
    ))}
}

##=================== server.R / ui.R functions ==============## 
# rendering function for digest
digest <- function() {
  renderUI({
    # grab table from google sheets
    tbl <- loadData()
    # grab table of today's entries
    todayTBL <- today(tbl)
    
    todayTBL$oneLine
    
    txt <- paste("<b>Project:</b>", todayTBL$projectName,
                 "<b>Role:</b>", todayTBL$role, 
                 "<b>Rating:</b>", todayTBL$rating, 
                 "<b>One Line:</b>", todayTBL$oneLine,
                 "<br/>", "<br/>", sep = " ")
    
    HTML(txt)
  })
}

# create dynamic boxes based on project names. tbl is entire sheet
digestBoxes <- function(tbl) {
  # filter just todays values
  todayTBL <- today(tbl)
  HMH <- todayTBL[todayTBL$projectName == "HMH",]
  if(dim(HMH[HMH$role == "Project Manager",])[1] == 0) {
    box(
      width = 12, title = "HMH",
      "Project Manager has not submitted their Weekly Status Report Yet"
    )
  } else{
    tbl <- HMH[HMH$role == "Project Manager",]
    box(
      width = 12,
      title = tbl$projectName,
      tags$img(src = paste0("half",tbl$rating,".png")),
      tbl$role, tbl$oneLiner
    )
  }
}

# iterate through roles
iterateRoles <- function(df) {
  lapply(roleList, function(x){digestBoxes(x)})
}

# takes google sheet input, extracts todays values, iterates through each project
iterateProjects <- function(tbl) {
  # filter just todays values
  todayTBL <- today(tbl)
  # organize by project
  byProject <- lapply(projectNames, function(x){
    filter(todayTBL, projectName == x)
  })
  # remove empty values for each group
  grouped <- byProject[sapply(byProject, function(y) {dim(y)[1] > 0})]
  lapply(grouped, function(x) digestBoxes(x))
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
                id = "teamDigests", title = "Team Digests", width = 12,
                tabPanel(
                  title = "This Week",
                  digestBoxes(loadData())
                ),
                tabPanel(
                  # this method requires browser to be refreshed for newer entries
                  title = "Digests", htmlOutput("gDigest"),
                  br(), "hello",
                  imageOutput("digestColor", height = "auto")
                ),
                tabPanel(
                  title = "Weekly Status Report OverView", 
                  "These are the teams that have submited their reports",
                  div(
                    # Another way is to use the builder functions; see ?tags.
                    "Here is",
                    tags$span(      # Creates an HTML span.
                      class="foo",  # Any named args become attributes.
                      
                      tags$strong("another"),  # Unnamed args become children of the tag.
                      "way"
                    ),
                    "to do it."
                  )
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
                         imageOutput("ratingImg", height = "auto")
                  ),
                  fluidRow(
                    column(width = 12, offset = 0,
                           textInput(inputId = "oneLiner", width = "100%",
                                     label = "One Liner:", placeholder = "One line that says it all...")
                    )
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
  
  # get digest info from googlesheet
  output$gDigest <- digest()
  
  # rating sends colored circle png based on rating response 
  output$ratingImg <- renderImage({
    # render rating color pic with global.R function
    ratingPic(input$rating)
  }, deleteFile = FALSE)
  
  # attempt for digest color rating ------------------- Can remove now -----
  output$digestColor <- renderImage({
    tbl <- loadData()
    dColor <- tail(tbl, n = 1)
    ratingPic(dColor$rating)
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
    data = c(date, input$projectName, input$role,input$rating,
             input$summary, input$oneLiner, gs_user()$displayName)
    # update google sheet
    saveData(data)
    
    # # grab table from google sheets
    tbl <- loadData()
    # update raw data view
    output$WSRtbl <- renderDataTable(tbl)
    
    ## TODO place renderUI with abstracted Project digest into HTML for auto update
    output$gDigest <- digest()
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