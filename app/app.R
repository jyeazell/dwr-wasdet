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
    #    title = app_title
    title = HTML(paste(app_title,
                       '<font color=\"#FF0000\">--- DEVELOP ---</font>'))
  ),
  
  ## Main Tabs. ----
  navbarPage(title = NULL,
             selected = "Explore",
             ### Explore. ----
             navbarMenu("Explore",
                        
                        fluidRow(
                          sidebarLayout(
                            
                            #### Sidebar Panel. ----
                            sidebarPanel(width = 2,
                                         
                            ), # End sidebarPanel
                            mainPanel(width = 10,
                                      
                                      # Plot/Data/Watershed map Tabs.
                                      tabsetPanel(type = "pills",
                                                  
                                                  ##### Plots tabs. ----
                                                  tabPanel("Plots",
                                                           fluidRow(
                                                             
                                                             # Plot column.
                                                             column(width = 7, # Plot column.
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
                                                                                
                                                                                
                                                                    ) # End plot_tabs tabsetPanel      
                                                             ), # End plot column.
                                                             
                                                             # Mini map column.
                                                             column(width = 5,
                                                                    fluidRow(
                                                                      
                                                                    ) # End fluidRow
                                                             ) # End column
                                                           ) # End fluidRow
                                                  ), # End Plots tabPanel    
                                                  
                                                  ##### Single Water Right tab. ----
                                                  tabPanel("Single Water Right",
                                                           
                                                  ), # End Single Water Right tabPanel  
                                                  
                                                  ##### Data tab. ----
                                                  tabPanel("Data",
                                                           
                                                  ), # End Data tabPanel
                                                  
                                                  ##### California Watershed Map tab. ----
                                                  tabPanel("California Watershed Map",
                                                           
                                                  ) # End California Watershed Map tabPanel
                                                  
                                      ) # End tabsetPanel      
                            ) # End mainPanel
                          ) # End sidebarLayout
                        ) # End fluidRow
             ), # End Explore navbarMenu
             
             
             ### Dataset Information. ----
             navbarMenu("Dataset Information",
                        icon = icon("table"),
                        
                        
             ), # End Dataset Information navbarMenu
             
             ### About/Help. ----
             navbarMenu("About/Help",
                        icon = icon("info-circle"),
                        
             ) # End About/Help navbarMenu
             
  ) # End navbarPage
) # End fluidPage_1


# SERVER -----------------------------------------------------------------------

server <- function(input, output, session) {
  
  
} # End Server

# APP --------------------------------------------------------------------------

shinyApp(ui = ui,
         server = server)

