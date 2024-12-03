## This will create visualizations based of the data pulled from DataDownload.R

install.packages("ggplot2")
library(ggplot2)

# Function to visualize the most played games across all players
visualize_most_played_games <- function(player_list) {
  library(ggplot2)

  # Combine game libraries from all players
  all_games <- do.call(rbind, lapply(player_list, function(player) {
    if (nrow(player@game_library) > 0) {
      player@game_library
    } else {
      NULL
    }
  }))

  # Aggregate playtime by game
  most_played <- aggregate(playtime_forever ~ appid, data = all_games, sum)

  # Sort and select the top 10 games
  most_played <- most_played[order(-most_played$playtime_forever), ][1:10, ]

  # Plot the data
  ggplot(most_played, aes(x = reorder(as.factor(appid), -playtime_forever), y = playtime_forever)) +
    geom_bar(stat = "identity") +
    labs(title = "Most Played Games", x = "App ID", y = "Total Playtime (Hours)") +
    theme_minimal()
}


# Visualize the most played games
visualize_most_played_games(player_list)
