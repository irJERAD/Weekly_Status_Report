## Scratch attempts at fucntions to support dynamics digest boxes that shows:
# Who has submitted their Weekly Status Report along with a One Line digest
# and which people have not.

## Two Functions: Iterate through Teams, then Iterate through Roles
# Iterate by Team:
# First pull Today or This Weeks reports - depending on the display criteria
# Have a function pull out information from this Today-Table into individual teams

# Get Today's info:
todayData <- function() {
  rawData <- loadData()
  todayTBL <- today(rawData)
}

####### TODO: Try ddply() ############
## --- --- Look at ddply() --- --- ##
# --- --- --- "For each subset of a data frame, apply function --- --- #
# --- --- --- --- then combine results into a data frame' --- --- --- --- #

# subset an individual team from Today's data
HMH <- todayTBL[todayTBL$projectName == "HMH",]

# create a subset of all teams with a function that takes the projectNames list
# creates subset of team aProject from today's data
todayTeam <- function(aProject) {
  # Get today's data:
  todayTBL <- todayData()
  # create a subset for each project's today Data by name
  aProject <- todayTBL[todayTBL$projectName == aProject, ]
  aProject
}


# Breakdown tbl into lists of individual projects for todays date
# prepare to be sent to make their live digest boxes:
todayTeamsList <- function() {
  # Create list containing all subsets of individual Projects
  todayList <- lapply(projectNames, function(x) todayTeam(x))
  # list of subsets after removing the empty lists:
  todayList[sapply(todayList, function(x) {dim(x)[1] > 0})]
}
# Iterate by Role:
# Receives a Today-Table of a single team
# This could have any combination or roles who have submitted and not submitted.
# ** It would be nice if all Roles could fit into their own Team Box ** #
# ** It would be nice if we could say which roles have not submitted yet ** #

# function takes a team's today list from the list of today lists and
# output is the box for the team
teamBoxes <- function (aTodayList) {
  # -- ** maybe function ** ----- ##
  # before stripping empty lists from todayList, we could use dimensions
  # to inform the digest that "No one from this team has submitted their report yet
  # --- can't do, bc can't get project name from empty list with dim() = 0 --- #
  
  # if (dim(aTodayList)[1] < 1) {
  #   box(
  #     wdith = 12, title = CANT FIND THIS NAME
  #   )
  # }
  
  # since we can't get a name from an empty list
  # assume we are working with the reduce list of projects that have some or all
  # submission from their roles
  
  box(
    width = 12,
    title = aTodayList$projectName[[1]],
    if (sum(aTodayList$role == "Account Manager")) {
      AM <- aTodayList[aTodayList$role == "Account Manager", ]
      paste(tags$img(src = paste0("half", AM$rating, ".png")),
            AM$role, AM$oneLiner
            )
    } else {
      paste("The Account Manager of",
            aTodayList$projectName[[1]],
            "has not submitted their report yet"
            )
    }
  )
  
}
# Abstact Specific Names for general function:

byRole <- function(specificRole) {
  if(dim(HMH[HMH$role == specificRole,])[1] == 0) {
    box(
      width = 12, title = "HMH",
      paste(specificRole, "has not submitted their Weekly Status Report Yet")
    )
  } else{
    tbl <- HMH[HMH$role == specificRole,]
    box(
      width = 12,
      title = tbl$projectName,
      tags$img(src = paste0("half",tbl$rating,".png")),
      tbl$role, tbl$oneLiner
    )
  }
}