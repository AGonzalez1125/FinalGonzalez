## This file will generate random steam IDs to build out to use in the functions
## to pull data
## This function will also verify if the steam ID is valid using the api connection

# Load required libraries
library(httr)
library(jsonlite)


## Main function to create vector of steam id, starting with 10
steamIDGenerate <- function(api_key,n) {
  ## Steam ID are 17 digit numbers with in a range, define this range
  lower_steam <- 76561197960265728
  upper_steam <- 76561199999999999


  ## Initialize steam vector of valid steam
  verified_steam <- c()


  ## Creating loop to fill vector
  while (length(verified_steam) < n){
  ## Marking here as i want to create a vector of steam ID, this will be used to
  ## create the data frame of information

    random_steam <- sample(seq(lower_steam, upper_steam), n - length(verified_steam), replace = TRUE)

     ## Creating loop to pull steam id
    for (steam_id in random_steam){

      url <- paste0(steam_url, "/ISteamUser/GetPlayerSummaries/v2/?key=", api_key, "&steamids=", steam_id)

      ## Jasonlite request to api
      response <- httr::GET(url)

      if (httr::status_code(response) == 200){
        ## Parse
        data <- jsonlite::fromJSON((httr::content(response, "text", encoding = "UTF-8")))

      ## Check if player exists
      if (length(data$response$players) > 0 ){
          verified_steam <- c(verified_steam, steam_id)
          message("Found Verified Steam ID: ", steam_id)
        }
      }

      ## Break the loop after enough valid steam IDs have been collected
      if (length(verified_steam) >= n) break
    }

  }
  return(verified_steam)
}
