library(shiny)
library(shinydashboard)


shinyUI(fluidPage(
  
  # Application title
  titlePanel("Project Status Color"),
  
  fluidRow(
    
    column(2, wellPanel(
      radioButtons("rating", "Rating:",
                   c("Green", "Yellow", "Red"))
    )),
    
    column(4, wellPanel(
           imageOutput("rating1")
    ))
  )
))
