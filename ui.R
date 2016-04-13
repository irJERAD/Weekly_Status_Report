library(shiny)


shinyUI(fluidPage(
  
  # Application title
  titlePanel("Project Status Color"),
  
  fluidRow(
    column(4, wellPanel(
      radioButtons("rating", "Rating:",
                   c("Green", "Yellow", "Red"))
    )),
    column(4,
           imageOutput("image1", height = 300),
           imageOutput("image2")
    )
  )
))
