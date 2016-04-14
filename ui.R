library(shiny)


shinyUI(fluidPage(
  
  # Application title
  titlePanel("Project Status Color"),
  
  fluidRow(
    
    column( 2, wellPanel(
      imageOutput("rating1")
    )),

    column(6, wellPanel(
      radioButtons("rating", "Rating:",
                   choices = c("Green", "Yellow", "Red")),
      dateInput(inputId = "date", label = "Date")
    ))
    
  )
))
