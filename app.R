library(shiny)
library(tidyverse)
library(ggplot2)
library(fmsb)
library(rmarkdown)
source("R/Untitled.R")

data <- read_csv2("data/2022-2023 NBA Player Stats - Playoffs.csv", show_col_types = FALSE) %>%
  as_tibble() %>%
  mutate(across(-c(2, 3, 5), as.numeric)) %>%
  mutate_if(is.character, utf8::utf8_encode)

stat_type <- list(
  "Basic" = c("PTS", "TRB", "AST", "STL", "BLK"),
  "FG (2pt)" = c("FG", "FGA", "FG%", "2P", "2PA", "2P%", "eFG%"),
  "FG (3pt)" = c("FG", "FGA", "FG%", "3P", "3PA", "3P%", "eFG%"),
  "FT" = c("FT", "FTA", "FT%"),
  "Other" = c("G", "GS", "MP", "ORB", "DRB", "PF")
)

ui <- navbarPage(
  title = "2022-2023 NBA Playoff Stats",
  tabPanel(
    "Home",
    fluidPage(
      sidebarLayout(
        sidebarPanel(
          selectInput(
            inputId = "team",
            label = "Team:",
            choices = sort(unique(data$Tm)),
            selected = "GSW"
          ),
          selectInput(
            inputId = "player",
            label = "Player:",
            choices = data$Player,
            selected = "Stephen Curry"
          ),
          selectInput(
            inputId = "stats",
            label = "Stats:",
            choices = c("Basic", "FG (2pt)", "FG (3pt)", "FT", "Other"),
            selected = "FGA"
          )
        ),
        mainPanel(
          plotOutput("radarPlot"),
          tableOutput("table")
        )
      )
    )
  ),
  tabPanel(
    "About",
    fluidPage(
      htmlOutput("rmdContent")
    )
  )
)

server <- function(input, output) {
  players <- reactive({
    data %>%
      filter(Tm == input$team) %>%
      arrange(desc(PTS))
  })

  observeEvent(input$team, {
    updateSelectInput(inputId = "player", choices = players()$Player)
  })

  # Reactive value for player data
  playerData <- reactiveVal()
  tablular <- reactiveVal()

  # Observer to update playerData when team or player changes
  observeEvent(c(input$player, input$stats), {
    graphData <- players() %>%
      filter(Player == input$player) %>%
      select(c("PTS", "TRB", "AST", "STL", "BLK")) %>%
      radar_df(data = data)
    playerData(graphData)
    tableData <- players() %>%
      filter(Player == input$player) %>%
      select(stat_type[[input$stats]])
    tablular(tableData)
  })

  output$radarPlot <- renderPlot({
    radarchart(playerData(),
      pcol = rgb(0.3, 0.3, 1), pfcol = rgb(0.2, 0.2, 0.9, 0.5), plwd = 2,
      cglcol = "grey", cglty = 1, axislabcol = "grey", cglwd = 0.8, vlcex = 1
    )
  })

  output$table <- renderTable(
    tablular()
  )

  output$rmdContent <- renderUI({
    rmdHTML <- render("about.Rmd", output_format = "html_document", quiet = TRUE)
    includeHTML(rmdHTML)
  })
}

shinyApp(ui = ui, server = server)
