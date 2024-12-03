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
playerSteamID <- steamIDGenerate( api_key, n = 100)

## My steam ID for testing functions
testSteamID <- 76561198140788932

## Function to get information about games pulled.
get_app_info <- function(api_key, app_id) {
  base_store_url <- "https://store.steampowered.com/api/appdetails/"
  app_url <- paste0(base_store_url, "?appids=", app_id)

  app_response <- httr::GET(app_url)

  if (httr::status_code(app_response) == 200) {
    app_data <- jsonlite::fromJSON(httr::content(app_response, "text", encoding = "UTF-8"))
    if (!is.null(app_data[[as.character(app_id)]]$data)) {
      return(app_data[[as.character(app_id)]]$data)
    }
  }

  return(NULL) # Return NULL if no data is found
}


## Use Steam ID to pull player information
get_recent_games <- function(api_key, steam_id) {
  # Construct the full API request URL
  gameSummaryUrl <- paste0(steam_url, "/IPlayerService/GetOwnedGames/v1/?key=",
                           api_key, "&steamid=", steam_id , "&include_appinfo=true&include_played_free_games=true")


  # Make the API request
  games_response <- httr::GET(gameSummaryUrl)

  if (httr::status_code(games_response) == 200) {
    # Parse the JSON response
    games_data <- jsonlite::fromJSON(httr::content(games_response, "text", encoding = "UTF-8"))
    print("Parsed JSON:")
    print(games_data)

    # Check if 'games' field exists
    if (!is.null(games_data$response$games)) {
      games_list <- as.list(games_data$response$games)
      return(games_list) ##Return as a list
    } else {
      warning("No recently played games found for this Steam ID.")
      return(NULL)
    }
  } else {
    warning("Failed to fetch recently played games. Status code: ", httr::status_code(games_response))
    return(NULL)
  }
}



##playerSummary1 <- get_recent_games(api_key, playerSteamID[10])

##print(playerSummary1)


## Function to run through the vector of steam IDs generated.
BulkGameHistory <- function(api_key,playerSteamID){

  ## initialize list to store game data for all steam id's
  gamesBulk <- list()

  ## for loop to run through the vector an run the get_recent_games function
  for (steam_id in playerSteamID){
    message("Pulling game data for Steam ID ", steam_id)
    recent_games <- get_recent_games(api_key, steam_id)
    str(recent_games)
    print(recent_games)

    # Process only if recent_games is not NULL
    if (!is.null(recent_games)) {
      for (i in seq_along(recent_games)) {
        # Extract game data
        game <- recent_games[[i]]


        game$steamid <- steam_id

        # Exclude 'content_descriptorids' if it exists
        # This data clouded up objects and will not be used.
        if ("content_descriptorids" %in% names(game)) {
          game$content_descriptorids <- NULL
        }


        # Ensure all required fields are present
        game <- as.list(game)
        required_fields <- c("steamid", "appid", "playtime_forever")
        for (field in required_fields) {
          if (!field %in% names(game)) {
            game[[field]] <- NA
          }
        }

        # Append game data to the list
        gamesBulk <- append(gamesBulk, list(game))
      }
    } else {
      message("No data found for Steam ID: ", steam_id)
    }
  }

  # Create and return a steam_player object
  return(create_steam_player(playerSteamID = playerSteamID, gameData = gamesBulk))
}

all_recent_games <- BulkGameHistory(api_key,testSteamID)
print(all_recent_games)
str(all_recent_games)
