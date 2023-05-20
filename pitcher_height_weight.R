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

# Convert height to inches
combined_df$height_inches <- as.numeric(gsub("'", "", unlist(strsplit(as.character(combined_df$HT), split=" "))[c(TRUE, FALSE)])) * 12 +
  as.numeric(gsub("\"", "", unlist(strsplit(as.character(combined_df$HT), split=" "))[c(FALSE, TRUE)]))

# Convert weight to numeric
combined_df$weight_lbs <- as.numeric(combined_df$WT)

# Convert height to meters and weight to kilograms
combined_df$height_m <- combined_df$height_inches * 0.0254
combined_df$weight_kg <- combined_df$weight_lbs * 0.453592

# Calculate BMI
combined_df$BMI <- combined_df$weight_kg / (combined_df$height_m ^ 2)

# View the combined data frame
head(combined_df)

# create a new data frame with only SPs
sp_df <- subset(combined_df, POS == "SP")

rp_df <- subset(combined_df, POS == "RP")

# Get pitcher stats for this season

pitcher_stats <- baseballr::fg_pitcher_leaders(2023, 2023, qual="0")

final_df <- merge(pitcher_stats, combined_df, by="Name", all.x = TRUE)

final_df <- final_df[!is.na(final_df$BMI), ]

# Get the names of all columns
column_names <- names(final_df)

# Remove 'height' and 'weight' from our list of column names
column_names <- column_names[!column_names %in% c("height_inches", "weight_lbs", "BMI")]

# Add 'height' and 'weight' back in the desired positions
column_names <- c(column_names[1:6], "height_inches", "weight_lbs", "BMI", column_names[7:length(column_names)])

# Apply the new column order to the data frame
final_df <- final_df[, column_names]

final_df <- as.data.frame(final_df)


pitcher_stats_2022 = baseballr::fg_pitcher_leaders(2022, 2022, qual="0")

# Add '_2022' to the names of the columns in pitcher_stats_2022, except for 'Name'
names(pitcher_stats_2022) <- ifelse(names(pitcher_stats_2022) != "Name", paste0(names(pitcher_stats_2022), "_2022"), "Name")

# Merge the 2022 stats into final_df by 'Name' or 'playerid'
final_df <- merge(final_df, pitcher_stats_2022, by = "Name", all.x = TRUE)  # Replace "Name" with "playerid" if you want to merge on 'playerid'


# Specify the exact columns you want to keep
exact_columns_to_keep <- c("Name", "height_inches", "weight_lbs", "BMI", "W", "L", "G", "GS", "IP", "Pitches")

# Specify the patterns of columns you want to keep
pattern_columns_to_keep <- c("ERA", "WAR", "FIP")

# Use select() to keep these columns
final_df <- final_df %>%
  select(one_of(exact_columns_to_keep), matches(paste(pattern_columns_to_keep, collapse="|")))

library(tidyr)

# Select relevant columns and reshape data
final_df_long <- final_df %>%
  select(Name, BMI, WAR, WAR_2022) %>%
  pivot_longer(cols = starts_with("WAR"),
               names_to = "Year",
               values_to = "WAR")

# Rename the levels in 'Year' column
final_df_long$Year <- recode(final_df_long$Year, "WAR" = "2023", "WAR_2022" = "2022")

library(ggplot2)

ggplot(final_df_long, aes(x = BMI, y = WAR, color = Year)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "BMI", y = "WAR", color = "Year", title = "BMI vs WAR") +
  theme_minimal()


# Fit linear models for each year
lm_2022 <- lm(WAR ~ BMI, data = subset(final_df_long, Year == "2022"))
lm_2023 <- lm(WAR ~ BMI, data = subset(final_df_long, Year == "2023"))

# Calculate R-squared values
r2_2022 <- summary(lm_2022)$r.squared
r2_2023 <- summary(lm_2023)$r.squared

# Create the plot
p <- ggplot(final_df_long, aes(x = BMI, y = WAR, color = Year)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "BMI", y = "WAR", color = "Year", title = "BMI vs WAR") +
  theme_minimal() +
  annotate("text", x = Inf, y = Inf, label = paste("R-squared (2022) = ", round(r2_2022, 3)), hjust = 1.1, vjust = 2, color = "red") +
  annotate("text", x = Inf, y = Inf, label = paste("R-squared (2023) = ", round(r2_2023, 3)), hjust = 1.1, vjust = 1, color = "green")

print(p)

