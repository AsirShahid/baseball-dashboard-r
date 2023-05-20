library(rvest)
library(dplyr)

# Create an empty list to store data frames
all_data <- list()

# Define the vector of team names and abbreviations
teams <- c("ari", "atl", "bal", "bos", "chc", "chw", "cin", "cle", "col", "det", "hou", "kc", "laa", "lad", "mia", "mil", "min", "nym", "nyy", "oak", "phi", "pit", "sd", "sea", "sf", "stl", "tb", "tex", "tor", "wsh")

# Function to convert height from feet-inches to inches
convert_height <- function(height) {
  height_parts <- strsplit(height, "-")[[1]]
  feet <- as.numeric(height_parts[1])
  inches <- as.numeric(height_parts[2])
  total_inches <- feet * 12 + inches
  return(total_inches)
}

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

# View the combined data frame
head(combined_df)
