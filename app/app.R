# Set Up. ----------------------------------------------------------------------

## Load Shiny libraries. ----
if (!("package:shiny" %in% search())) {
  suppressMessages(library(shiny))
}
if (!("package:shinythemes" %in% search())) {
  suppressMessages(library(shinythemes))
}
if (!("package:shinyjs" %in% search())) {
  suppressMessages(library(shinyjs))
}
if (!("package:shinycssloaders" %in% search())) {
  suppressMessages(library(shinycssloaders))
}

develop <- TRUE

## Load libraries. ----
source("m-load-libraries.R")

## Load data files. ----
source("m-load-prep.R", local = TRUE)

## Application title.----
app_title <- paste("Division of Water Rights",
                   "Water Supply/Demand Visualization Tool")
app_title <- ifelse(develop,
                    paste(app_title,
                          '<font color=\"#FF0000\">--- DEVELOP ---</font>'),
                    app_title) 

# UI. -------------------------------------------------------------------------
ui <- fluidPage(
  useShinyjs(),
  
  # Set theme.
  theme = shinytheme("cerulean"),
  
  
  # Application title
  titlePanel(HTML(app_title)),
  
  # navbarPage layout
  navbarPage(title = NULL,
             
             ## Explore tab panel. ----
             tabPanel("Explore",
                      sidebarLayout(
                        ### Sidebar Panel. ----
                        sidebarPanel(width = 2,
                                     
                                     #### Select HUC-8 watershed. ----
                                     selectInput(inputId = "huc8_selected",
                                                 label = "Select HUC-8 Watershed:",
                                                 choices = sort(names(demand)),
                                                 selected = "Butte",
                                                 multiple = FALSE
                                     ),
                                     
                                     ###### Filter for watersheds with supply information. ----
                                     checkboxInput(inputId = "supply_filter",
                                                   label = "Filter for watersheds with available supply information",
                                                   value = FALSE
                                     ),
                                     
                                     ###### Select demand scenario(s). ----
                                     selectizeInput(inputId = "d_scene_selected",
                                                    label = "Select Up To Two Demand Scenarios:",
                                                    choices = NULL,
                                                    # choices = sort(unique(demand[[input$huc8_selected]]$d_scenario)),
                                                    selected = "Reported Diversions - 2011",
                                                    multiple = TRUE,
                                                    options = list(maxItems = 2)
                                     ),
                        ),
                        mainPanel(width = 10,
                                  # Plot/Data/Watershed map Tabs.
                                  tabsetPanel(type = "pills",
                                              
                                              ##### Plot tabs. ----
                                              tabPanel("By Watershed",
                                                       fluidRow(
                                                         column(width = 12,
                                                                uiOutput("debug_text")
                                                         )
                                                       )
                                              )
                                  )
                        )
                      )
             )
  )
)

# SERVER. ---------------------------------------------------------------------
server <- function(input, output, session) {
  
  # ** DEBUG TEXT ** ----
  
  output$debug_text <- renderUI(HTML(paste0("huc8_selected: ", 
                                            input$huc8_selected, br(),
                                            "d_scene_selected: ",
                                            input$d_scene_selected, br()#,
                                            # "s_scene_selected: ",
                                            # input$s_scene_selected
  )
  )
  )
  
  # #
  # observeEvent(input$huc8_selected, {
  #   if( !is.null(supply[[input$huc8_selected]]) ) {
  #     choices <- sort(unique(supply[[input$huc8_selected]]$s_scenario))
  #     updateSelectizeInput(session,
  #                          inputId = "s_scene_selected",
  #                          choices = supply_choices,
  #                          selected = NULL)
  #   }
  # })
  # 
  ## Update demand scenario choices. ----
  observeEvent(input$huc8_selected, {
    choices <- sort(unique(demand[[input$huc8_selected]]$d_scenario))
    updateSelectizeInput(session,
                         inputId = "d_scene_selected",
                         choices = choices,
                         selected = "Reported Diversions - 2020")
  })
  
}

# APP --------------------------------------------------------------------------

# Run in a dialog within R Studio
runGadget(ui, server, viewer = dialogViewer("Dialog Title", width = 1600, height = 1200))

shinyApp(ui = ui,
         server = server)








