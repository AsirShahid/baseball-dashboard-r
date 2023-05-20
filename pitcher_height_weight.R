library(shiny)
library(mlbplotR)
library(ggplot2)
library(dplyr)
library(baseballr)
library(scales)
library(rvest)

# Load rvest package
library(rvest)

# Read the webpage
webpage <- read_html("https://www.espn.com/mlb/team/roster/_/name/nyy")

# Find the table node by its id using xpath
table_node <- html_node(webpage, xpath = "/html/body/div[1]/div/div/div/main/div[2]/div[5]/div/div[1]/section/div/section/div[2]/div[1]/div[2]/div")

# Parse the table into a data frame
table_df <- html_table(table_node, fill = TRUE)

# View the data frame
head(table_df)

