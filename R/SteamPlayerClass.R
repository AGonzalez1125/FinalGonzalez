## Run This Script First.


# Used to access steam via Api request
install.packages("httr")
install.packages("jsonlite")
library(httr)
library(jsonlite)

## This will create the S3 object labeled Steam Player and define a print
## function for it.

# Define the S4 class
setClass(
  "steam_player",
  slots = c(
    steamid = "character",
    player_info = "list",
    game_library = "data.frame"
  )
)

# Constructor for steam_player
steam_player <- function(steamid, player_info = list(), game_library = data.frame()) {
  new("steam_player", steamid = steamid, player_info = player_info, game_library = game_library)
}




