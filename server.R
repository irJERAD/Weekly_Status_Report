library(shiny)
library(shinydashboard)


shinyServer(function(input, output) {
   
  # rating1 sends pre-rendered png
  output$rating1 <- renderImage({
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
