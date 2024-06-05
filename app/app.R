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
                                     
                                     #### huc8_selected: Select HUC-8 watershed. ----
                                     selectInput(inputId = "huc8_selected",
                                                 label = "Select HUC-8 Watershed:",
                                                 choices = NULL,
                                                 selected = NULL,
                                                 multiple = FALSE
                                     ),
                                     
                                     #### Filter for watersheds with supply information. ----
                                     checkboxInput(inputId = "supply_filter",
                                                   label = "Filter for watersheds with available supply information",
                                                   value = FALSE
                                     ),
                                     
                                     #### d_scene_selected: Select demand scenario(s). ----
                                     selectizeInput(inputId = "d_scene_selected",
                                                    label = "Select Up To Two Demand Scenarios:",
                                                    choices = NULL,
                                                    # choices = sort(unique(demand[[input$huc8_selected]]$d_scenario)),
                                                    selected = "Reported Diversions - 2011",
                                                    multiple = TRUE,
                                                    options = list(maxItems = 2)
                                     ),
                                     
                                     #### priority_selected: Select priority year to slice for vsd_plot. ----
                                     selectInput(inputId = "priority_selected",
                                                 label = "Select Demand Priority Year:",
                                                 choices = NULL,
                                                 selected = NULL,
                                                 multiple = FALSE),
                                     
                                     #### s_scene_selected: Select supply scenario(s) for vsd_plot. ----
                                     selectizeInput(inputId = "s_scene_selected",
                                                    label = "Select Up To Three Supply Scenarios:",
                                                    choices = NULL,
                                                    selected = NULL,
                                                    multiple = TRUE,
                                                    options = list(maxItems = 3)
                                     ),
                                     
                                     #### wrt_selected: Select water right types to include in dbwrt_plot. ----
                                     checkboxGroupInput(inputId = "wrt_selected",
                                                        label = "Select Water Right Type(s) to Display:",
                                                        choices = NULL,
                                                        selected = NULL),
                                     
                                     #### no_supply_text: Conditional supply availability text. ----
                                     htmlOutput(outputId = "no_supply_text"),
                                     
                                     #### Copyright. ----
                                     HTML('<center><img src="waterboards_logo_high_res.jpg", height = "70px"><img src="DWR-ENF-Logo-2048.png", height = "70px"></center>'),
                                     HTML(paste("<center>©", year(now()), 
                                                "State Water Resources Control Board</center>"))
                        ),
                        
                        ### Main Panel. ----
                        mainPanel(width = 10,
                                  #### Plot/Data/Watershed map Tabs. ----
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
                                            input$d_scene_selected, br(),
                                            "s_scene_selected: ",
                                            input$s_scene_selected
  )
  )
  )
  
  # OBSERVERS. ----
  
  ## Filter for watersheds that have supply data. ----
  observeEvent(input$supply_filter, {
    if (input$supply_filter) { 
      huc8_choices <- sort(names(demand)[names(demand) %in% names(supply)])
    } else { 
      huc8_choices <- sort(names(demand))
    }
    updateSelectInput(session,
                      inputId = "huc8_selected",
                      choices = huc8_choices,
                      selected = sample(huc8_choices, 1))
  })
  
  ## Update demand scenario choices. ----
  observeEvent(input$huc8_selected, {
    demand_choices <- sort(unique(demand[[input$huc8_selected]]$d_scenario))
    updateSelectizeInput(session,
                         inputId = "d_scene_selected",
                         choices = demand_choices,
                         selected = "Reported Diversions - 2020")
  })
  
  ## Update supply scenario choices. ----
  observeEvent(input$huc8_selected, {
    if( !is.null(supply[[input$huc8_selected]]) ) {
      supply_choices <- sort(unique(supply[[input$huc8_selected]]$s_scenario))
      updateSelectizeInput(session,
                           inputId = "s_scene_selected",
                           choices = supply_choices,
                           selected = NULL)
    }
  })
  
  ## Update priority year choices. ----
  py_choice_list <- reactive({
    sort(na.omit(unique(demand[[input$huc8_selected]]$p_year)), 
         decreasing = TRUE)
  })
  observeEvent(input$huc8_selected, {
    choices <- py_choice_list()[py_choice_list() > min(py_choice_list())]
    updateSelectInput(session, "priority_selected",
                      choices = choices,
                      selected = nth(choices, length(choices) / 2))
  })
  
  ## Update water right type choices. ----
  observeEvent(input$huc8_selected, {
    choices <- sort(unique(demand[[input$huc8_selected]]$wr_type))             
    updateCheckboxGroupInput(session = session, 
                             inputId = "wrt_selected",
                             choices = choices,
                             selected = choices)
  })
  
}

# APP --------------------------------------------------------------------------

# Run in a dialog within R Studio
runGadget(ui, server, viewer = dialogViewer("Dialog Title", width = 1600, height = 1200))

shinyApp(ui = ui,
         server = server)








