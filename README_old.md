---
title: "Gonzalez Final Project"
author: "Andre Gonzalez"
date: "`r Sys.Date()`"
output: html_document
---


# Introduction

Video games has always been a part of my life and I was looking away to incorporate it in to my studies. What better way than to analyse data about videogames and the people that play them.  

This Project uses the Steam API to download and analyze player and game data. It uses R to:   
1. Fetch player summaries and game libraries using the Steam API.  
2. Create an `S4` object `steam_player` to organize player and library data.  
3. Generate visualizations for:  
    + The top 10 most played games.  
    + Average playtime for top games.  

## Libraries used in this Project

These are the libraries needed for analysis of the data. These below are samples and are subject to change with the final iteration of the project.



```{r setup, include=FALSE}
# Load necessary libraries
library(ggplot2). 
library(httr). 
library(jsonlite). 
```

##  Function Explanation

Here I will talk about the uses and what the functions do. 

* `steamIDGenerate(api_key, n)`
Generates a vector of n random Steam IDs.

* `get_player_summary(api_key, steam_id)`
Fetches player summary data for a given Steam ID.

* `get_game_library(api_key, steam_id)`
Retrieves the game library for a Steam ID.

* `create_steam_player(api_key, steam_id)`
Creates an S4 object steam_player with:

    + steamid: The Steam ID.
    + player_info: A list of player details.
    + game_library: A data frame of games and playtimes.
    + create_steam_player_list(api_key, steam_ids)
    + Generates a list of steam_player objects for multiple Steam IDs.

* `visualize_most_played_games(player_list)`
Creates a bar chart showing the top 10 most played games.

* `visualize_average_playtime(player_list)`
Generates a bar chart showing the average playtime for the top 10 games.



## Visuals 

`r visualize_most_played_games(player_list)`   
`r visualize_average_playtime(player_list)`






## Assumptions

All Steam IDs generated or provided are valid and publicly accessible.  
The Steam API key used has sufficient permissions to fetch player and game data.  


## Limitations

Data availability is dependent on the privacy settings of Steam users.
Some games may not return complete metadata (e.g., game_name or genres).
API rate limits might affect large-scale data retrieval.
