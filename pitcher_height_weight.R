library(shiny)
library(mlbplotR)
library(ggplot2)
library(dplyr)
library(baseballr)
library(scales)
library(rvest)
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

# Then continue with your existing code
combined_df <- combined_df %>%
  mutate(HT = gsub("'", " ", HT),  # replace the ' character with a space
         HT = gsub("\"", "", HT),  # remove the " character
         WT = gsub(" lbs", "", WT)) %>%  # remove " lbs" from the weight
  separate(HT, into = c("feet", "inches"), sep = " ", extra = "drop") %>%
  mutate(feet = as.numeric(feet),
         inches = as.numeric(inches),
         HT_in_inches = feet*12 + inches) %>%
  select(-feet, -inches)  # remove the temporary columns

# Convert the height and weight columns to numeric
combined_df <- combined_df %>%
  mutate(HT_in_inches = as.numeric(HT_in_inches),
         WT = as.numeric(WT))


# remove the numbers from the name column
combined_df$Name <- gsub("\\d", "", combined_df$Name)


# View the combined data frame
head(combined_df)

