## This will create visualizations based of the data pulled from DataDownload.R
## Run This Script Last.

## Required Packages
install.packages("ggplot2")
library(ggplot2)

# Load cached data
player_list <- readRDS("player_list.rds")

## Function to visualize the most played games across all players
visualize_most_played_games <- function(player_list) {
  library(ggplot2)

  ## Define required columns
  required_columns <- c("appid", "playtime_forever", "game_name")

  ## Combine game libraries from all players
  all_games <- do.call(rbind, lapply(player_list, function(player) {
    if (nrow(player@game_library) > 0) {
      ## Ensure consistent columns
      game_library <- player@game_library
      missing_cols <- setdiff(required_columns, colnames(game_library))
      for (col in missing_cols) {
        game_library[[col]] <- NA # Add missing columns as NA
      }
      game_library <- game_library[, required_columns, drop = FALSE] # Reorder and retain only required columns
      return(game_library)
    } else {
      NULL
    }
  }))

  ## Debugging: Inspect combined data
  print(head(all_games)) # Preview combined game library
  print(colnames(all_games)) # Ensure correct column names

  if (is.null(all_games) || nrow(all_games) == 0) {
    warning("No game data available for visualization.")
    return(NULL)
  }

  ## Exclude games with unknown names
  all_games <- all_games[!(all_games$game_name == "Unknown" | is.na(all_games$game_name)), ]

  if (nrow(all_games) == 0) {
    warning("No valid games available for visualization after cleaning.")
    return(NULL)
  }

  ## Aggregate playtime by appid or game_name if available
  if ("game_name" %in% colnames(all_games)) {
    most_played <- aggregate(playtime_forever ~ game_name, data = all_games, sum)
    most_played <- most_played[order(-most_played$playtime_forever), ]
    x_column <- "game_name"
  } else {
    most_played <- aggregate(playtime_forever ~ appid, data = all_games, sum)
    most_played <- most_played[order(-most_played$playtime_forever), ]
    x_column <- "appid"
  }

  ## Select the top 10 games
  most_played <- head(most_played, 10)

  ## Debugging: Inspect aggregated data
  print(most_played) # Preview the aggregated data

  ## Create the bar chart
  plot <- ggplot(most_played, aes_string(x = x_column, y = "playtime_forever")) +
    geom_bar(stat = "identity", fill = "steelblue") +
    labs(
      title = "Top 10 Most Played Games",
      x = ifelse(x_column == "game_name", "Game Name", "App ID"),
      y = "Total Playtime (Hours)"
    ) +
    theme_minimal() +
    coord_flip()

  ## Testing Function values
  print(head(all_games)) # Preview combined game library
  print(colnames(all_games)) # Ensure correct column names
  print(most_played) # Preview the aggregated data

  ## Return the ggplot object
  return(plot)
}


## Function to get average playtime for top 10 games
visualize_average_playtime <- function(player_list) {
  ## Define required columns
  required_columns <- c("appid", "playtime_forever", "game_name")

  ## Combine game libraries from all players
  all_games <- do.call(rbind, lapply(player_list, function(player) {
    if (nrow(player@game_library) > 0) {
      ## Ensure consistent columns
      game_library <- player@game_library
      missing_cols <- setdiff(required_columns, colnames(game_library))
      for (col in missing_cols) {
        game_library[[col]] <- NA
      }
      game_library <- game_library[, required_columns, drop = FALSE]
      return(game_library)
    } else {
      NULL
    }
  }))

  if (is.null(all_games) || nrow(all_games) == 0) {
    warning("No game data available for visualization.")
    return(NULL)
  }

  ## Exclude games with unknown names
  all_games <- all_games[!(all_games$game_name == "Unknown" | is.na(all_games$game_name)), ]

  ## Aggregate average playtime by game
  avg_playtime <- aggregate(playtime_forever ~ game_name, data = all_games, mean)
  avg_playtime <- avg_playtime[order(-avg_playtime$playtime_forever), ]
  avg_playtime <- head(avg_playtime, 10)

  ## Create the bar chart
  ggplot(avg_playtime, aes(x = reorder(game_name, -playtime_forever), y = playtime_forever)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    labs(
      title = "Average Playtime for Top 10 Games",
      x = "Game Name",
      y = "Average Playtime (Hours)"
    ) +
    theme_minimal() +
    coord_flip()
}

## Function to visualize the top 10 genres (excluding as I could not get it to work)
visualize_top_genres <- function(player_list) {

  ## Define required columns
  required_columns <- c("appid", "playtime_forever", "game_name", "genres")

  ## Combine game libraries from all players
  all_games <- do.call(rbind, lapply(player_list, function(player) {
    if (nrow(player@game_library) > 0) {
      ## Ensure consistent columns
      game_library <- player@game_library
      missing_cols <- setdiff(required_columns, colnames(game_library))
      for (col in missing_cols) {
        game_library[[col]] <- NA
      }
      game_library <- game_library[, required_columns, drop = FALSE]
      return(game_library)
    } else {
      NULL
    }
  }))

  if (is.null(all_games) || nrow(all_games) == 0) {
    warning("No game data available for visualization.")
    return(NULL)
  }

  ## Exclude games with unknown names or missing genres
  all_games <- all_games[!(all_games$game_name == "Unknown" | is.na(all_games$game_name)), ]
  all_games <- all_games[!is.na(all_games$genres), ]

  ## Aggregate genres for top 10 games
  top_games <- aggregate(playtime_forever ~ game_name + genres, data = all_games, sum)
  top_games <- top_games[order(-top_games$playtime_forever), ]
  top_genres <- head(top_games, 10)

  ## Create the bar chart
  ggplot(top_genres, aes(x = reorder(genres, -playtime_forever), y = playtime_forever)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    labs(
      title = "Top Genres in Top 10 Games",
      x = "Genres",
      y = "Total Playtime (Hours)"
    ) +
    theme_minimal() +
    coord_flip()
}


## Visualize the most played games
visualize_most_played_games(player_list)
visualize_average_playtime(player_list)





