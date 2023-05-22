library(shiny)
library(mlbplotR)
library(ggplot2)
library(dplyr)
library(baseballr)
library(scales)

# Fetch the data first, we'll use the latest year's data for the column names
initial_year <- 2023
df_global <- baseballr::fg_team_pitcher(x = initial_year, y = initial_year)

ui <- fluidPage(
  titlePanel("MLB Pitcher Leaders Comparison"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("stat1", 
                  label = "Choose first statistic for comparison",
                  choices = names(df_global),  # Use column names as choices
                  selected = "ERA"),
      
      selectInput("stat2", 
                  label = "Choose second statistic for comparison",
                  choices = names(df_global),  # Use column names as choices
                  selected = "FIP"),
      
      numericInput("year",
                   label = "Choose year",
                   value = initial_year,
                   min = 2012, 
                   max = initial_year,
                   step = 1)
    ),
    
    mainPanel(
      plotOutput("baseballPlot")
    )
  )
)

server <- function(input, output) {
  output$baseballPlot <- renderPlot({
    df <- baseballr::fg_team_pitcher(x = input$year, y = input$year)
    
    df |> 
      ggplot2::ggplot(aes_string(x = input$stat1, y = input$stat2)) +
      mlbplotR::geom_mlb_logos(aes(team_abbr = Team), width = 0.075, alpha = 0.7) +
      ggplot2::labs(title = paste(input$stat1, " vs ", input$stat2, " in ", input$year, ":"),
                    #                    subtitle = paste("Team Pitching Stats", input$year),
                    caption = "Data: FanGraphs via baseballr") +
      ggplot2::theme_minimal() +
      ggplot2::theme(plot.title = ggplot2::element_text(face = "bold")) #+
    #      ggplot2::scale_x_reverse(breaks = scales::pretty_breaks(), 
    #                               labels = scales::number_format(accuracy = 0.01),
    #                               expand = c(.1, .1)) +
    #      ggplot2::scale_y_reverse(breaks = scales::pretty_breaks(), 
    #                               labels = scales::number_format(accuracy = 0.01),
    #                               expand = c(.1, .1))
  })
}

shinyApp(ui = ui, server = server)