#Programmer: Andre Gonzalez
#
#
# This will install the necessary packages and download the data from steam.

hello <- function() {
  print("Hello, world!")
}

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
