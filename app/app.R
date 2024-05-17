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

develop <- FALSE
debug_flag <- FALSE

if(develop) {
  old_wd <- getwd()
  setwd("./app")
  
}


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
# source("m-load-libraries.R")

## Load S3 keys. ----
#source("m-load-s3-keys.R")

# Application title.
app_title <- paste("Division of Water Rights",
                   "Water Supply/Demand Visualization Tool")

#source("app/m-load-prep.R")


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
                          
                          #### Sidebar Panel. ----
                          sidebarPanel(width = 2,
                                       
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
  
  
} # End Server

# APP --------------------------------------------------------------------------

# Run in a dialog within R Studio
runGadget(ui, server, viewer = dialogViewer("Dialog Title", width = 1400, height = 900))
# 
# shinyApp(ui = ui,
#          server = server)

