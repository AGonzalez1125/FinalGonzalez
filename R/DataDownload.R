#Programmer: Andre Gonzalez
#
#
# This will install the necessary packages and download the data from steam.



install.packages("devtools")
install.packages("roxygen2")
library(devtools)
library(roxygen2)

# Used to access steam via Api request
install.packages("httr")
install.packages("jsonlite")

library(httr)
library(jsonlite)

# Set your Steam API key, for Security I will not put the API Key in the code.
# You can get your own API key here: https://steamcommunity.com/dev/apikey
api_key <- readLines(file.choose())

# Define the Steam API base URL
steam_url <- "https://api.steampowered.com"

## Call Function to create vector of valid steam IDs
playerSteamID <- steamIDGenerate( api_key, n = 1000)



## Use Steam ID to pull player information
get_recent_games <- function(api_key, steam_id) {
  # Construct the full API request URL
  gameSummaryUrl <- paste0(steam_url, "/IPlayerService/GetRecentlyPlayedGames/v1/?key=", api_key, "&steamid=", steam_id
  )

  # Make the API request
  games_response <- httr::GET(gameSummaryUrl)

  if (httr::status_code(games_response) == 200) {
    # Parse the JSON response
    games_data <- jsonlite::fromJSON(httr::content(games_response, "text", encoding = "UTF-8"))
    print("Parsed JSON:")
    print(games_data)

    # Check if 'games' field exists
    if (!is.null(games_data$response$games)) {
      return(games_data$response$games)
    } else {
      warning("No recently played games found for this Steam ID.")
      return(NULL)
    }
  } else {
    warning("Failed to fetch recently played games. Status code: ", httr::status_code(games_response))
    return(NULL)
  }
}

playerSummary1 <- get_recent_games(api_key, playerSteamID[10])

print(playerSummary)

PlayerGameSummmary <- function(playerSteamID){




}



