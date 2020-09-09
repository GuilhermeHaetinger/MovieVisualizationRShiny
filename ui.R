library(shiny)
library(shinythemes) # Temas para a interface do shiny
library(RCurl)       # Abstração de comandos 'curl' (HTTP requests)
library(dplyr)
library(jsonlite)    # Leitura de JSONs
library(glue)        # Operações em strings parametrizadas (como uma 'F-string')
library(ggplot2)
library(purrr)       # Contém a função 'map', muito útil para aplicar funções em listas
library(plotly)
library(data.table)
library(GGally)
library(network)

ui <- fluidPage(
  theme=shinytheme('superhero'),
  headerPanel("Movie DB Analisys!"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("threshold", label = "threshold",
                  min = 1, max = 8, value = 1, step = 1),
      selectInput("df", "Movie Database", c("Popular Movies", "Top Rated Movies"),
                  selected = "Popular Movies"),
      checkboxInput("sankey", "Sankey", value=TRUE),
        fluidRow(
          column(6,
                 uiOutput("frame")),
          column(5,
                 offset = 1,
                 htmlOutput("description"))
      ),
    ),
    mainPanel(
      plotlyOutput('plot')
     
    )
  )
)
