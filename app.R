

# Create shiny app
library(shiny)
library(shinythemes)
library(shinyjs)
library(shinycssloaders)


ui <- fluidPage(
  theme = shinytheme("cerulean"),
  useShinyjs(),
  
  # Application title
  titlePanel("Uploading Files"),
  
  # navbarPage layout
 navbarPage(title = NULL,
            tabPanel("Explore",
                     sidebarLayout(
                       sidebarPanel(width = 2),
                       mainPanel(width = 10)
                     )
            ),
 )
)



server <- function(input, output, session) {
  
}

shinyApp(ui = ui, server = server)
  
  
  
  
  
  
  
