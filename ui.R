library(shiny)
install.packages("shinythemes")
library(shinythemes) # Temas para a interface do shiny
install.packages("RCurl")
library(RCurl)       # Abstração de comandos 'curl' (HTTP requests)
install.packages("dplyr")

library(dplyr)
install.packages("jsonlite")
library(jsonlite)    # Leitura de JSONs
install.packages("glue")
library(glue)        # Operações em strings parametrizadas (como uma 'F-string')
install.packages("ggplot2")
library(ggplot2)
install.packages("purrr")
library(purrr)       # Contém a função 'map', muito útil para aplicar funções em listas
install.packages("plotly")
library(plotly)
install.packages("data.table")
library(data.table)
install.packages("GGally")
library(GGally)
install.packages("network")
library(network)

ui <- fluidPage(
  theme=shinytheme('superhero'),
  headerPanel("Movie DB Analisys!"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("threshold", label = "threshold",
                  min = 0, max = 10, value = 1, step = 1),
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
