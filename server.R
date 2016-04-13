#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
  # image2 sends pre-rendered images
  output$image2 <- renderImage({
    if (is.null(input$rating))
      return(NULL)
    
    if (input$rating == "Green") {
      return(list(
        src = "images/green.png",
        contentType = "image/png",
        alt = "Green"
      ))
    } else if (input$rating == "Yellow") {
      return(list(
        src = "images/yellow.png",
        filetype = "image/png",
        alt = "Yellow"
      ))
    } else if (input$rating == "Red") {
      return(list(
        src = "images/red.png",
        filetype = "image/png",
        alt = "Red"
      ))
    }
    
  }, deleteFile = FALSE)
  
})
