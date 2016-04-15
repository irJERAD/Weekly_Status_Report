library(shiny)
library(shinydashboard)

simple <- "This is global"

testing <- function() {
  print(c("Project Title Here"))
}

ratingPic <- function(rating) {
  if (is.null(rating))
  return(NULL)
  
  if (rating == "green") {
    return(list(
      src = "images/green.png",
      contentType = "image/png",
      alt = "Green"
    ))
  } else if (rating == "yellow") {
    return(list(
      src = "images/yellow.png",
      filetype = "image/png",
      alt = "Yellow"
    ))
  } else if (rating == "red") {
    return(list(
      src = "images/red.png",
      filetype = "image/png",
      alt = "Red"
    ))}
}