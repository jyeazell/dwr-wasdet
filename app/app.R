# Set Up. ----------------------------------------------------------------------

## Load Shiny libraries.
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
debug_flag <- FALSE

# if(develop) {
#   old_wd <- getwd()
#   setwd("./app")
#   
# }


# Debug. #####
if (debug_flag) {
  if (!("package:reactlog" %in% search())) {
    suppressMessages(library(reactlog))
  }
  reactlogReset()
  reactlog_enable()
}

# Initialization. ----

## Load libraries. ----
source("m-load-libraries.R")

# ## Load S3 keys. ----
# source("m-load-s3-keys.R")

## Load data files. ----
source("m-load-prep.R")

## Application title.----
app_title <- paste("Division of Water Rights",
                   "Water Supply/Demand Visualization Tool")

# UI ---------------------------------------------------------------------------

ui <- fluidPage( # Start fluidpage_1
  useShinyjs(),
  
  # Set theme.
  theme = shinytheme("cerulean"),
  
  ## Title bar. ----
  titlePanel(
    ## Toggle for Production <---
    # title = app_title
    title = HTML(paste(app_title,
                       '<font color=\"#FF0000\">--- DEVELOP ---</font>'))
  ),
  
  ## Main Tabs. ----
  navbarPage(title = NULL,
             ### Explore. ----
             tabPanel("Explore",
                      
                      fluidRow(
                        sidebarLayout(
                          
                          ### Sidebar Panel. ----
                          
                          sidebarPanel(width = 2,
                                       
                                       ##### Mandatory inputs. ----
                                       
                                       ###### Select units to display. ----
                                       radioButtons(inputId = "units_selected",
                                                    label = "Select Units:",
                                                    choiceNames = list(
                                                      "Cubic Feet per Second (cfs)",
                                                      "Acre-Feet per Day (AF/d)"
                                                    ),
                                                    choiceValues = list(
                                                      "cfs",
                                                      "afd"
                                                    ),
                                                    selected = "cfs"
                                       ),
                                       
                                       ###### Select HUC-8 watershed. ----
                                       selectInput(inputId = "huc8_selected",
                                                   label = "Select HUC-8 Watershed:",
                                                   choices = NULL,
                                                   selected = NULL,
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
                                                      selected = NULL,
                                                      multiple = TRUE,
                                                      options = list(maxItems = 2)
                                       ),
                                       
                                       ##### Conditional Inputs. ----
                                       
                                       ###### Select supply scenario(s) for vsd_plot. ----
                                       selectizeInput(inputId = "s_scene_selected",
                                                      label = "Select Up To Three Supply Scenarios:",
                                                      choices = NULL,
                                                      selected = NULL,
                                                      multiple = TRUE,
                                                      options = list(maxItems = 3)
                                       ),
                                       
                                       ###### Select priority year to slice for vsd_plot. ----
                                       selectInput(inputId = "priority_selected",
                                                   label = "Select Demand Priority Year:",
                                                   choices = NULL,
                                                   selected = NULL,
                                                   multiple = FALSE),
                                       
                                       ###### Select water right types to include in dbwrt_plot. ----
                                       checkboxGroupInput(inputId = "wrt_selected",
                                                          label = "Select Water Right Type(s) to Display:",
                                                          choices = NULL,
                                                          selected = NULL),
                                       
                                       ###### Conditional supply availability text. ----
                                       htmlOutput(outputId = "no_supply_text"),
                                       
                                       ##### Copyright. ----
                                       HTML('<center><img src="waterboards_logo_high_res.jpg", height = "70px"><img src="DWR-ENF-Logo-2048.png", height = "70px"></center>'),
                                       HTML(paste("<center>©", year(now()), 
                                                  "State Water Resources Control Board</center>"))
                         
                          ),
                          
                          #### Main Panel. ----
                          mainPanel(width = 10,
                                    
                                    # Plot/Data/Watershed map Tabs.
                                    tabsetPanel(type = "pills",
                                                
                                                ##### Plot tabs. ----
                                                tabPanel("By Watershed",
                                                         fluidRow(
                                                           
                                                           # Plot column.
                                                           column(width = 7,
                                                                  tabsetPanel(id = "plot_tabs",
                                                                              selected = "Demand by Water Right Type",
                                                                              type = "pills",
                                                                              
                                                                              ###### Demand by Water right type plot tab. ----
                                                                              tabPanel(title = "Demand by Water Right Type",
                                                                                       id = "dbwrt_tab",
                                                                                       fluidRow(
                                                                                         
                                                                                       )
                                                                              ),
                                                                              
                                                                              ###### Demand by priority plot tab. ----
                                                                              tabPanel(title = "Demand by Priority",
                                                                                       id = "dbp_tab",
                                                                                       fluidRow(
                                                                                         
                                                                                       )
                                                                              ),
                                                                              
                                                                              ###### Supply-Demand plot tab. ----
                                                                              tabPanel(title = "Supply-Demand Scenarios",
                                                                                       id = "vsd_tab",
                                                                                       fluidRow(
                                                                                         
                                                                                       )
                                                                              )
                                                                              
                                                                              
                                                                  )      
                                                           ),
                                                           
                                                           ##### Mini map column. ----
                                                           
                                                           ###### Mini map. ----
                                                           column(width = 5,
                                                                  fluidRow(
                                                                    
                                                                  )
                                                           )
                                                         )
                                                ),
                                                
                                                ##### By Water Right tab. ----
                                                tabPanel("By Water Right",
                                                         fluidRow(
                                                           
                                                         )
                                                ),
                                                
                                                ##### Data tab. ----
                                                tabPanel("Data",
                                                         fluidRow(
                                                           
                                                         )
                                                ),
                                                
                                                ##### California Watershed Map tab. ----
                                                tabPanel("California Watershed Map",
                                                         fluidRow(
                                                           
                                                         )
                                                )
                                    )      
                          )
                        )
                      )
             ), 
             
             ### Dataset Information. ----
             navbarMenu("Dataset Information",
                        icon = icon("table"),
                        
                        #### Demand Scenarios. ----
                        tabPanel("Demand Scenarios",
                                 icon = icon("faucet"),
                                 # includeHTML("./docs/demand-scenarios.html")
                        ),
                        
                        #### Supply Scenarios. ----
                        tabPanel("Supply Scenarios",
                                 icon = icon("water")
                        ),
                        
                        #### Other Data. ----
                        tabPanel("Other Data",
                                 icon = icon("table"),
                                 
                        )
             ),
             
             ### About/Help. ----
             navbarMenu("About/Help",
                        icon = icon("info-circle"),
                        
                        #### About menu. ----
                        tabPanel("About",
                                 icon = icon("info-circle"),
                                 # includeMarkdown("./docs/ABOUT.md")
                        ),
                        
                        #### How to Use the Filters menu. ----
                        tabPanel("How To Use The Filters",
                                 icon = icon("life-ring"),
                                 "How To Use The Filters", br(),
                                 "Content Goes Here"
                        ),
                        
                        #### FAQ menu. ----
                        tabPanel("Frequently Asked Questions",
                                 icon = icon("question"),
                                 # includeHTML(("./docs/faq.html"))
                        ),
                        
                        #### Report Bugs menu. ----
                        tabPanel("Report Bugs/Data Issues",
                                 icon = icon("bug"),
                                 # includeHTML(("./docs/bugs-issues.html"))
                        )
             )
  ),
  selected = "Explore"
) # end fluidPage_1


# SERVER -----------------------------------------------------------------------

server <- function(input, output, session) {
  
  
  
  # ** DEBUG TEXT ** ----
  
  output$debug_text <- renderUI(HTML(paste0("huc8_selected: ", 
                                            input$huc8_selected, br(),
                                            "d_scene_selected: ", 
                                            input$d_scene_selected, br(),
                                            "s_scene_selected: ",
                                            input$s_scene_selected)))
  
  # Setup. ----
  
  # Disable units selector until implemented
  disable(id = "units_selected")
  
  # Helper Functions. ----
  
  # Define plot height function to keep facet panels roughly the same height
  # whether displaying one or two.
  plot_height <- reactive({
    ifelse(length(input$d_scene_selected) == 1, 480,
           ifelse(length(input$d_scene_selected) == 2, 835, "auto"))
  })
  
  # OBSERVERS. ----
  
  ## Control input selectors. ----
  observe({
    if (input$plot_tabs == "Demand by Water Right Type") {
      hideElement(id = "no_supply_text")
      hideElement(id = "s_scene_selected")
      hideElement(id = "priority_selected")
      showElement(id = "wrt_selected")
    }
    if (input$plot_tabs == "Demand by Priority") {
      hideElement(id = "no_supply_text")
      hideElement(id = "s_scene_selected")
      hideElement(id = "priority_selected")
      hideElement(id = "wrt_selected")
    }
    if (input$plot_tabs == "Supply-Demand Scenarios" & 
        is.null(supply[[input$huc8_selected]])) {
      showElement(id = "no_supply_text")
      hideElement(id = "s_scene_selected")
      showElement(id = "priority_selected")
      hideElement(id = "wrt_selected")
    }
    if (input$plot_tabs == "Supply-Demand Scenarios" & 
        !is.null(supply[[input$huc8_selected]])) {
      hideElement(id = "no_supply_text")
      showElement(id = "s_scene_selected")
      showElement(id = "priority_selected")
      hideElement(id = "wrt_selected")
    }
  })
  
} # End Server

# APP --------------------------------------------------------------------------

# Run in a dialog within R Studio
# runGadget(ui, server, viewer = dialogViewer("Dialog Title", width = 1600, height = 1200))

shinyApp(ui = ui,
         server = server)


 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
