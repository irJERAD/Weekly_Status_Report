## --- --- temp hold starting under server.R /ui.R functions

pasteDigest <- function(x) {paste(
  tags$img(src = paste0("half", x['rating'], ".png")),
  "<b>Project:</b>", x['projectName'],
  "<b>Role:</b>", x['role'], 
  "<b>Rating:</b>", x['rating'], 
  "<b>One Line:</b>", x['oneLiner'],
  "<br/>", "<br/>", sep = " "
)}
# rendering function for digest
digest <- function() {
  renderUI({
    # grab table from google sheets
    tbl <- loadData()
    # grab table of today's entries
    todayTBL <- today(tbl)
    
    todayTBL$oneLine
    
    txt <- paste(tags$img(src = paste0("half", todayTBL$rating, ".png")),
                 "<b>Project:</b>", todayTBL$projectName,
                 "<b>Role:</b>", todayTBL$role, 
                 "<b>Rating:</b>", todayTBL$rating, 
                 "<b>One Line:</b>", todayTBL$oneLine,
                 "<br/>", "<br/>", sep = " ")
    
    markUp2 <- adply(todayTBL, 1, function(x) {paste(
      tags$img(src = paste0("half", x['rating'], ".png")),
      "<b>Project:</b>", x['projectName'],
      "<b>Role:</b>", x['role'], 
      "<b>Rating:</b>", x['rating'], 
      "<b>One Line:</b>", x['oneLiner'],
      "<br/>", "<br/>", sep = " "
    )}
    )
    markUp <- paste(tags$img(src = paste0("half", todayTBL$rating, ".png")),
                    "<b>Project:</b>", todayTBL$projectName,
                    "<b>Role:</b>", todayTBL$role, 
                    "<b>Rating:</b>", todayTBL$rating, 
                    "<b>One Line:</b>", todayTBL$oneLiner,
                    "<br/>", "<br/>", sep = " "
    )
    
    markUp3 <- adply(todayTBL, 1, function(x) pasteDigest(x))
    
    markUp4 <- ddply(todayTBL, "projectName", .fun = pasteDigest(todayTBL))
    
    HTML(txt)
  })
}