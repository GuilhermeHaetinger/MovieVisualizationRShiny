library(shiny)
install.packages("shinythemes")
library(shinythemes)
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
