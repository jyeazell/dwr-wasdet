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

## Load data files. ----
source("m-load-prep.R", local = TRUE)

## Application title.----
app_title <- paste("Division of Water Rights",
                   "Water Supply/Demand Visualization Tool")
if(develop) app_title <- HTML(paste(app_title,
                                    '<font color=\"#FF0000\">--- DEVELOP ---</font>'))

# UI. -------------------------------------------------------------------------
ui <- fluidPage( # Start fluidPage
  useShinyjs(),
  
  ## Set theme.
  theme = shinytheme("cerulean"),
  
  ## Title bar. ----
  titlePanel(title = app_title),
  
  ## Main Tabs. ----
  navbarPage(title = NULL,
             ### Explore. ----
             tabPanel("Explore",
                      
                      fluidRow(
                        sidebarLayout(
                          
                          #### Sidebar Panel. ----
                          sidebarPanel(width = 2,
                                       
                                       ##### huc8_selected: Select HUC-8 watershed. ----
                                       selectInput(inputId = "huc8_selected",
                                                   label = "Select HUC-8 Watershed:",
                                                   choices = NULL,
                                                   selected = NULL,
                                                   multiple = FALSE
                                       ),
                                       
                                       ##### Conditional supply availability text. ----
                                       htmlOutput(outputId = "no_supply_text"),
                                       
                                       ##### Filter for watersheds with supply information. ----
                                       checkboxInput(inputId = "supply_filter",
                                                     label = "Filter for watersheds with available supply information",
                                                     value = FALSE
                                       ),
                                       
                                       ##### Select demand scenario(s). ----
                                       selectizeInput(inputId = "d_scene_selected",
                                                      label = "Select Up To Two Demand Scenarios:",
                                                      choices = NULL,
                                                      selected = NULL,
                                                      multiple = TRUE,
                                                      options = list(maxItems = 2)
                                       ),
                                       
                                       ##### Select supply scenario(s). ----
                                       selectizeInput(inputId = "s_scene_selected",
                                                      label = "Select Up To Three Supply Scenarios:",
                                                      choices = NULL,
                                                      selected = NULL,
                                                      multiple = TRUE,
                                                      options = list(maxItems = 3)
                                       ),
                                       
                                       ##### Select priority year. ----
                                       selectInput(inputId = "priority_selected",
                                                   label = "Select Demand Priority Year:",
                                                   choices = NULL,
                                                   selected = NULL,
                                                   multiple = FALSE),
                                       
                                       ##### Select water right types. ----
                                       checkboxGroupInput(inputId = "wrt_selected",
                                                          label = "Select Water Right Type(s) to Display:",
                                                          choices = NULL,
                                                          selected = NULL),
                                       
                                       ##### Copyright. ----
                                       HTML('<center><img src="waterboards_logo_high_res.jpg", height = "70px"><img src="DWR-ENF-Logo-2048.png", height = "70px"></center>'),
                                       HTML(paste("<center>©", year(now()), 
                                                  "State Water Resources Control Board</center>")),
                                       
                                       
                                       
                          ), # End sidebarPanel
                          
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
                                                                                         br(),
                                                                                         withSpinner(plotOutput(outputId = "dbwrt_plot"))
                                                                                       )
                                                                              ),
                                                                              
                                                                              ###### Demand by priority plot tab. ----
                                                                              tabPanel(title = "Demand by Priority",
                                                                                       id = "dbp_tab",
                                                                                       fluidRow(
                                                                                         br(),
                                                                                         withSpinner(plotOutput(outputId = "dbp_plot"))
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
                                                                    
                                                                    
                                                                    br(),
                                                                    
                                                                    # ###### DEBUG NOTES. ----
                                                                    uiOutput("debug_text")
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
                          ) # End mainPanel
                          
                        ) # End sidebarLayout
                      ) # End fluidRow
             ), # End tabPanel
             
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




# SERVER. ---------------------------------------------------------------------
server <- function(input, output, session) {
  
  ## DEBUG TEXT ----
  output$debug_text <- renderUI(HTML(paste0(br(), br(),
                                            h3("Debug"), br(),
                                            "huc8_selected: ", 
                                            input$huc8_selected, br(),
                                            "d_scene_selected: ",
                                            input$d_scene_selected, br(),
                                            "s_scene_selected: ",
                                            input$s_scene_selected)
  )
  )
  
  ## Setup. ----
  
  ## Helper Functions. ----
  
  # Define plot height function to keep facet panels roughly the same height
  # whether displaying one or two.
  plot_height <- reactive({
    ifelse(length(input$d_scene_selected) == 1, 480,
           ifelse(length(input$d_scene_selected) == 2, 835, "auto"))
  })
  
  ## OBSERVERS. ----
  
  ### Update input choices by plot tab. ----
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
  
  ### Update available watersheds when input$supply_filter check box changes. ----
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
  
  ### Update demand scenario choices. ----
  observeEvent(input$huc8_selected, {
    req(input$huc8_selected)
    choices <- sort(unique(demand[[input$huc8_selected]]$d_scenario))
    updateSelectizeInput(session, 
                         inputId = "d_scene_selected",
                         choices = choices,
                         selected = "Reported Diversions - 2023")
  })
  
  ### Update supply scenario choices. ----
  observeEvent(input$huc8_selected, {
    req(input$huc8_selected)
    if( !is.null(supply[[input$huc8_selected]]) ) {
      supply_choices <- sort(unique(supply[[input$huc8_selected]]$s_scenario))
    }else {
      supply_choices <- character(0)
    }
    updateSelectizeInput(session,
                         inputId = "s_scene_selected",
                         choices = supply_choices,
                         selected = "")
    
  })
  
  ### Update priority year choices. ----
  py_choice_list <- reactive({
    sort(na.omit(unique(demand[[input$huc8_selected]]$p_year)), 
         decreasing = TRUE)
  })
  observeEvent(input$huc8_selected, {
    req(input$huc8_selected)
    py_choices <- py_choice_list()#[py_choice_list() > min(py_choice_list())]
    updateSelectInput(session, "priority_selected",
                      choices = py_choices,
                      selected = max(py_choices))
  })
  
  ### Update water right type choices. ----
  observeEvent(input$huc8_selected, {
    choices <- unique(demand[[input$huc8_selected]]$wr_type)
    updateCheckboxGroupInput(session = session, 
                             inputId = "wrt_selected",
                             choices = choices,
                             selected = choices)
  })
  
  ## OUTPUTS ----
  
  ### No supply data alert. ----
  output$no_supply_text <- renderText({ 
    paste0('<font color=\"#FF0000\"><p><b>',
           'Supply Data Not Available for This Watershed.',
           '</b><p></font>') 
  })
  
  ### DBWRT - Demand By Water Right Type Plot (dbwrt). ----
  
  #### Build dataset. ----
  # Demand by water right type dataset.
  wrt_plot_data <- reactive({ 
    req(input$huc8_selected)
    demand[[input$huc8_selected]] %>% 
      filter(d_scenario %in% input$d_scene_selected,
             wr_type %in% input$wrt_selected) %>% 
      group_by(d_scenario, 
               wr_type, 
               plot_month = month(plot_date, label = TRUE)) %>%
      summarize(af_monthly = sum(af_monthly, na.rm = TRUE),
                af_daily = sum(af_daily, na.rm = TRUE),
                cfs = sum(cfs, na.rm = TRUE),
                .groups = "drop")
  })
  
  #### Render plot. ----
  output$dbwrt_plot <- renderPlot({
    
    # Validate.
    validate(
      need(input$d_scene_selected, 
           "No data to plot.\nPlease select at least one Demand Scenario."),
      need(input$huc8_selected, 
           "No data to plot.\nPlease select a Watershed."),
      need(nrow(wrt_plot_data()) > 0,
           "No data for selected water right types.")
    )
    
    # Render.
    ggplot(data = wrt_plot_data(),
           aes(x = plot_month,
               y = cfs,
               fill = wr_type)) +
      
      # Demand bars.
      geom_col() + 
      
      # Y axis format.
      scale_y_continuous(labels = comma) +
      
      # Legend.
      scale_fill_manual(name = "Water Right Type:",
                        values = plot_wrt_pal,
                        limits = sort(unique(wrt_plot_data()$wr_type))) +
      
      # labels
      labs(title = "Monthly Demand by Water Right Type",
           y = "Cubic Feet per Second (cfs)") +
      
      # Facet on demand scenario.
      facet_wrap(~ d_scenario, 
                 ncol = 1,
                 scales = "free_x") +
      
      # Theme.
      theme_minimal() +
      theme(
        plot.title = element_text(size = rel(2.0)),
        strip.text.x = element_text(size = rel(2.0)),
        axis.title = element_text(size = rel(1.2)),
        axis.text = element_text(size = rel(1.2)),
        legend.text = element_text(size = rel(1.2)),
        legend.title = element_text(size = rel(1.2)),
        legend.justification = "top",
        legend.box = "horizontal",
        legend.direction = "vertical",
        axis.title.x = element_blank())
    
  }, height = function() plot_height()
  )
  
  ## DBP - Demand by Priority plot. ----
  
  ### Build legend. ----
  
  # Demand by Priority legend.
  priority_plot_pal <- reactive({
    
  })
  
  ### Build dataset. ----
  
  # Demand by priority dataset.
  dbp_plot_data <- reactive({
    demand[[input$huc8_selected]] %>% 
      filter(d_scenario %in% input$d_scene_selected) %>% #,
      #       wr_type %in% input$wrt_selected) %>% 
      group_by(d_scenario, 
               priority, 
               plot_month = month(plot_date, label = TRUE)) %>%
      summarize(af_monthly = sum(af_monthly, na.rm = TRUE),
                af_daily = sum(af_daily, na.rm = TRUE),
                cfs = sum(cfs, na.rm = TRUE),
                .groups = "drop") %>% 
      mutate(priority = ordered(priority, levels = names(priority_pal)))
  })
  
  ### Render plot. ----
  output$dbp_plot <- renderPlot({
    
    # Render.
    ggplot(data = dbp_plot_data(),
           aes(x = plot_month,
               y = cfs,
               fill = priority)) +
      
      # Demand bars.
      geom_col() + 
      
      # Y axis format.
      scale_y_continuous(labels = comma) +
      
      # Legend.
      scale_fill_manual(name = "Priority:",
                        values = priority_pal,
                        limits = sort(unique(dbp_plot_data()$priority))) +
      #    guides(fill = guide_legend(ncol = 2)) +
      
      
      # labels
      labs(title = "Monthly Demand by Priority",
           y = "Cubic Feet per Second (cfs)") +
      
      # Facet on demand scenario.
      facet_wrap(~ d_scenario, 
                 ncol = 1,
                 scales = "free_x") +
      
      # Theme.
      theme_minimal() +
      theme(
        plot.title = element_text(size = rel(2.0)),
        strip.text.x = element_text(size = rel(2.0)),
        axis.title = element_text(size = rel(1.2)),
        axis.text = element_text(size = rel(1.2)),
        legend.text = element_text(size = rel(1.2)),
        legend.title = element_text(size = rel(1.2)),
        legend.justification = "top",
        legend.box = "horizontal",
        legend.direction = "vertical",
        axis.title.x = element_blank())
    
  }, height = function() plot_height()
  )
  
}

# APP --------------------------------------------------------------------------

# Run in a dialog within R Studio
# runGadget(ui, server, viewer = dialogViewer(dialogName = "DWR-WASDET DEVELOP", 
#                                             width = 1600, 
#                                             height = 1200))

shinyApp(ui = ui,
         server = server)









