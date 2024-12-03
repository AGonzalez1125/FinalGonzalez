#Programmer: Andre Gonzalez
# This will install the necessary packages and download the data from steam.

## Run This Script Third

## Used to create documentation
install.packages("devtools")
install.packages("roxygen2")
library(devtools)
library(roxygen2)

# Used to access steam via Api request
install.packages("httr")
install.packages("jsonlite")
library(httr)
library(jsonlite)



## Set your Steam API key, for Security I will not put the API Key in the code.
## You can get your own API key here: https://steamcommunity.com/dev/apikey
api_key <- readLines(file.choose())

## Define the Steam API base URL
steam_url <- "https://api.steampowered.com"

## Call Function to create vector of valid steam IDs
steam_ids <- steamIDGenerate( api_key, n = 1000)

## My steam ID for testing functions
testSteamID <- "76561198140788932"


## Function to pull Player Summary
get_player_summary <- function(api_key, steam_id) {
  url <- paste0("https://api.steampowered.com/ISteamUser/GetPlayerSummaries/v2/?key=",
                api_key, "&steamids=", steam_id)
  response <- httr::GET(url)

  if (httr::status_code(response) == 200) {
    data <- jsonlite::fromJSON(httr::content(response, "text", encoding = "UTF-8"))
    print(data) # Debug the entire API response
    if (!is.null(data$response$players) && nrow(data$response$players) > 0) {
      # Extract the player data as a named list
      player_info <- as.list(data$response$players[1, ]) # Extract the first row as a list
      return(player_info)
    } else {
      warning("No player data found for Steam ID: ", steam_id)
      return(list()) # Return an empty list for missing data
    }
  }

  warning("Failed to fetch player summary for Steam ID: ", steam_id)
  return(list()) # Return an empty list for failed API requests
}




## Function to pull Player Game Library
get_game_library <- function(api_key, steam_id) {
  url <- paste0("https://api.steampowered.com/IPlayerService/GetOwnedGames/v1/?key=",
                api_key, "&steamid=", steam_id, "&include_appinfo&include_played_free_games=true")
  response <- httr::GET(url)

  if (httr::status_code(response) == 200) {
    data <- jsonlite::fromJSON(httr::content(response, "text", encoding = "UTF-8"))
    print(data) # Debug the entire API response
    if (!is.null(data$response$games)) {
      return(data$response$games)
    }
  }

  warning("Failed to fetch game library for Steam ID: ", steam_id)
  return(data.frame()) # Return empty data frame for invalid libraries
}

## Function to pull app info
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

## Function to pull more data about games
enrich_game_library <- function(game_library) {
  if (nrow(game_library) > 0) {
    game_library$game_name <- sapply(game_library$appid, function(app_id) {
      app_details <- get_app_info(api_key, app_id)
      if (!is.null(app_details$name)) {
        return(app_details$name)
      } else {
        warning("No game name found for App ID: ", app_id)
        return("Unknown")
      }
    })
  }
  return(game_library)
}



## Function to Call other functions and Create Steam_Player Object
create_steam_player <- function(api_key, steam_id) {
  steam_id <- as.character(steam_id)
  message("Fetching player summary for Steam ID: ", steam_id)

  player_info <- get_player_summary(api_key, steam_id)
  if (length(player_info) == 0 || !is.list(player_info)) {
    warning("No valid player info for Steam ID: ", steam_id)
    player_info <- list(
      personaname = "Unknown",
      profileurl = "Unavailable",
      avatarmedium = "Unavailable"
    )
  }

  message("Fetching game library for Steam ID: ", steam_id)
  game_library <- get_game_library(api_key, steam_id)
  if (nrow(game_library) > 0) {
    game_library <- enrich_game_library(game_library)
  } else {
    warning("Empty game library for Steam ID: ", steam_id)
    game_library <- data.frame(appid = integer(), playtime_forever = numeric(), game_name = character())
  }

  steam_player(steamid = steam_id, player_info = player_info, game_library = game_library)
}


## Function to generate a list of steam_player objects
create_steam_player_list <- function(api_key, steam_ids) {
  valid_players <- list()

  for (id in steam_ids) {
    tryCatch({
      message("Processing Steam ID: ", id)
      player <- create_steam_player(api_key, id)
      valid_players <- append(valid_players, list(player))
    }, error = function(e) {
      warning("Error processing Steam ID: ", id, " - ", conditionMessage(e))
    })
  }

  return(valid_players)
}



## Next Section is to test each functions to assist with debugging.
test_playerSummary <- get_player_summary(api_key,testSteamID)
print(test_playerSummary)

test_gameSummary <- get_game_library(api_key, testSteamID)
print(test_gameSummary)

test_steamPlayer <- create_steam_player(api_key, testSteamID)
print(test_steamPlayer)



## Create Player list to be used for analytics
player_list <- create_steam_player_list(api_key, steam_ids)

