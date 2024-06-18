# Set Up. ----------------------------------------------------------------------

## Load Shiny libraries. ----
if (!("package:shiny" %in% search())) {
  suppressMessages(library(shiny))
}

## Load libraries. ----
#source("m-load-libraries.R")
# 
# ## Load data files. ----
# source("m-load-prep.R", local = TRUE)

##  Define variables. ----

# Demand watershed choices.
demand_choices <- c("Aliso-San Onofre",                 "Antelope-Fremont Valleys",        
                    "Applegate",                        "Battle Creek",                    
                    "Big Chico Creek-Sacramento River", "Big-Navarro-Garcia",              
                    "Butte",                            "Butte Creek",                     
                    "Calleguas",                        "Carrizo Creek",                   
                    "Carrizo Plain",                    "Central Coastal",                 
                    "Chetco",                           "Clear Creek-Sacramento River",    
                    "Cottonwood Creek",                  "Cottonwood-Tijuana",              
                    "Cow Creek",                        "Coyote",                          
                    "Coyote-Cuddeback Lakes",           "Crowley Lake",                    
                    "Cuyama",                           "Death Valley-Lower Amargosa",     
                    "East Branch North Fork Feather",   "East Walker",                     
                    "Estrella",                         "Eureka-Saline Valleys",           
                    "Fish Lake-Soda Spring Valleys",    "Fresno River",                    
                    "Goose Lake",                       "Gualala-Salmon")

# Supply watershed choices.
supply_choices <- c("Battle Creek",                     "Big-Navarro-Garcia",
                    "Butte Creek",                      "Cottonwood Creek",       
                    "Cow Creek",                        "Fresno River",          
                    "McCloud",                          "Middle Fork Feather",    
                    "North Fork American",              "Sacramento Headwaters",  
                    "Sacramento Watershed",             "San Joaquin Watershed",   
                    "Scott",                            "Shasta",              
                    "Upper Bear",                       "Upper Cache",          
                    "Upper Calaveras California",       "Upper Cosumnes",       
                    "Upper Merced",                     "Upper Mokelumne",     
                    "Upper Putah",                      "Upper San Joaquin",   
                    "Upper Stanislaus",                 "Upper Stony",          
                    "Upper Tuolumne",                   "Upper Yuba")


# UI. -------------------------------------------------------------------------
ui <- fluidPage(
 
                                     
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
                   
                                  
                                  ###### DEBUG ----
                                  h3("Debug"),
                                  uiOutput("debug_text")
 
  ) # End fluidPage

# SERVER. ---------------------------------------------------------------------
server <- function(input, output, session) {
  
  # ** DEBUG TEXT ** ----
  
  output$debug_text <- renderUI(HTML(paste0("huc8_selected: ", 
                                            input$huc8_selected, br()#,
                                            # "d_scene_selected: ", 
                                            # input$d_scene_selected, br(),
                                            # "s_scene_selected: ",
                                            # input$s_scene_selected
                                            )
                                     )
                                )
  
  # OBSERVERS. ----
  
  ## Filter for watersheds that have supply data. ----
  observeEvent(input$supply_filter, {
    if (input$supply_filter) { 
      huc8_choices <- demand_choices[demand_choices %in% supply_choices]
    } else { 
      huc8_choices <- demand_choices
    }
    updateSelectInput(session,
                      inputId = "huc8_selected",
                      choices = huc8_choices,
                      selected = sample(huc8_choices, 1))
  })
  
  
  # ## Filter for watersheds that have supply data. ----
  # huc8_choices <- reactive({
  #   ifelse(input$supply_filter,
  #          demand_choices[demand_choices %in% supply_choices],
  #          demand_choices)
  # })
  # 
  # observeEvent(input$supply_filter, {
  #   req(input$supply_filter)
  #   updateSelectInput(session,
  #                     inputId = "huc8_selected",
  #                     choices = huc8_choices(),
  #                     selected = sample(demand_choices, 1))
  # })
  # 
}

# APP --------------------------------------------------------------------------

# Run in a dialog within R Studio
runGadget(ui, server, viewer = dialogViewer("Dialog Title", width = 1600, height = 1200))
# 
# shinyApp(ui = ui,
#          server = server)








