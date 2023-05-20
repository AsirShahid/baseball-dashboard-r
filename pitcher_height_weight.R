library(shiny)
library(mlbplotR)
library(ggplot2)
library(dplyr)
library(baseballr)
library(scales)
library(rvest)

# Install tidyr and dplyr if you haven't done so already
if (!require(tidyr)) {
  install.packages("tidyr")
}
if (!require(dplyr)) {
  install.packages("dplyr")
}

# Load tidyr and dplyr
library(tidyr)
library(dplyr)

# Create an empty list to store data frames
all_data <- list()

# Define the vector of team names and abbreviations
teams <- c("ari", "atl", "bal", "bos", "chc", "chw", "cin", "cle", "col", "det", "hou", "kc", "laa", "lad", "mia", "mil", "min", "nym", "nyy", "oak", "phi", "pit", "sd", "sea", "sf", "stl", "tb", "tex", "tor", "wsh")

# Loop over each team
for (team in teams) {
  # Construct the URL for the team's webpage
  url <- paste0("https://www.espn.com/mlb/team/roster/_/name/", team)
  
  # Read the webpage
  webpage <- read_html(url)
  
  # Find the table node by its id using xpath or CSS selector
  table_node <- html_node(webpage, xpath = "/html/body/div[1]/div/div/div/main/div[2]/div[5]/div/div[1]/section/div/section/div[2]/div[1]/div[2]/div")
  
  # Parse the table into a data frame
  table_df <- html_table(table_node, fill = TRUE)
  
  # Append the data frame to the list
  all_data[[team]] <- table_df
}

# Combine all the data frames in the list into a single data frame
combined_df <- bind_rows(all_data)


# Then continue with your existing code
combined_df <- combined_df %>%
  mutate(WT = gsub(" lbs", "", WT)) # remove " lbs" from the weight
  

# remove the numbers from the name column
combined_df$Name <- gsub("\\d", "", combined_df$Name)


# create a new data frame with only SPs
sp_df <- subset(combined_df, POS == "SP")

rp_df <- subset(combined_df, POS == "RP")

# View the combined data frame
head(combined_df)

pitcher_stats <- baseballr::fg_pitcher_leaders(2023, 2023, qual="0")

final_df <- merge(pitcher_stats, combined_df, by="Name", all.x = TRUE)

final_df <- final_df[!is.na(final_df$HT) & !is.na(final_df$WT), ]

# Get the names of all columns
column_names <- names(final_df)

# Remove 'height' and 'weight' from our list of column names
column_names <- column_names[!column_names %in% c("HT", "WT")]

# Add 'height' and 'weight' back in the desired positions
column_names <- c(column_names[1:2], "HT", "WT", column_names[3:length(column_names)])

# Apply the new column order to the data frame
final_df <- final_df[, column_names]

