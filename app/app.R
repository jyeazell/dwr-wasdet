# Set Up. ----------------------------------------------------------------------

## Load Shiny libraries.
if (!("package:shiny" %in% search())) {
  suppressMessages(library(shiny))
}
# if (!("package:shinydashboard" %in% search())) {
#   suppressMessages(library(shinydashboard))
# }
if (!("package:bslib" %in% search())) {
  suppressMessages(library(bslib))
}
if (!("package:shinyjs" %in% search())) {
  suppressMessages(library(shinyjs))
}
if (!("package:shinycssloaders" %in% search())) {
  suppressMessages(library(shinycssloaders))
}

develop <- TRUE
debug_flag <- TRUE

## Debug. #####
if (debug_flag) {
  if (!("package:reactlog" %in% search())) {
    suppressMessages(library(reactlog))
  }
  reactlogReset()
  reactlog_enable()
}

## Initialization. ----

### Load libraries. ----
source("m-load-libraries.R")

### Load data files. ----
source("m-load-prep.R", local = TRUE)

## Functions, ----

### Build plot supply data frame.
build_plot_supply <- function(x, s_scene, d_scene) {
  y <- x %>% 
    filter(s_scenario %in% s_scene) %>%
    mutate(source = "old",
           fill_color = NA,
           plot_group = "supply") %>%
    full_join(.,
              as_tibble(d_scene),
              by = character()) %>%
    select(source,
           d_scenario = value,
           s_scenario,
           plot_date,
           fill_color,
           af_monthly,
           af_daily,
           cfs,
           plot_group,
           plot_category)
  return(y)
}

## Application title.----
app_title <- paste("Division of Water Rights",
                   "Water Supply/Demand Visualization Tool")
if(develop) app_title <- HTML(paste(app_title,
                                    '<font color=\"#FF0000\">--- DEVELOP ---</font>'))

# UI. -------------------------------------------------------------------------
ui <- page_fillable(
  useShinyjs(),
  
  ## CSS tags. ----
  tags$head(
    tags$style(HTML("
      
      .main_menu_theme {
     
    
      
      text-align: left;
      }
                    ")
    )
  ),
  
  theme = bs_theme(version = 5,
                   bootswatch = "cosmo"),
  
  titlePanel("Division of Water Rights Water Supply/Demand Visualization Tool"),
  
  tags$div(class = 'main_menu_theme',
           navset_card_pill( 
             
             ## Explore tab. ----
             tabPanel("Explore",
                      
                      navset_card_pill(
                        selected = "By Watershed",
                        
                        ### Sidebar ----
                        sidebar = sidebar( # Start Explore sidebar.
                          
                          #### Select HUC-8 watershed. ----
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
                          
                          #### Select demand scenario(s). ----
                          selectizeInput(inputId = "d_scene_selected",
                                         label = "Select Up To Two Demand Scenarios:",
                                         choices = NULL,
                                         selected = NULL,
                                         multiple = TRUE,
                                         options = list(maxItems = 2)
                          ),
                          
                          #### Select supply scenario(s) for vsd_plot. ----
                          selectizeInput(inputId = "s_scene_selected",
                                         label = "Select Up To Three Supply Scenarios:",
                                         choices = NULL,
                                         selected = NULL,
                                         multiple = TRUE,
                                         options = list(maxItems = 3)
                          ),
                          
                          #### Select priority year to slice for vsd_plot. ----
                          selectInput(inputId = "priority_selected",
                                      label = "Select Demand Priority Year:",
                                      choices = NULL,
                                      selected = NULL,
                                      multiple = FALSE),
                          
                          #### Select water right types to include in dbwrt_plot. ----
                          checkboxGroupInput(inputId = "wrt_selected",
                                             label = "Select Water Right Type(s) to Display:",
                                             choices = NULL,
                                             selected = NULL),
                          
                          #### Conditional supply availability text. ----
                          htmlOutput(outputId = "no_supply_text"),
                          
                          #### Copyright. ----
                          HTML('<center><img src="waterboards_logo_high_res.jpg", height = "70px"><img src="DWR-ENF-Logo-2048.png", height = "70px"></center>'),
                          HTML(paste("<center>©", year(now()), 
                                     "State Water Resources Control Board</center>"))
                          
                        ), # End Explore sidebar.
                        
                        ### By Watershed tab. ----
                        tabPanel("By Watershed",
                                 
                                 navset_card_pill(id = "plot_tabs",
                                                  selected = "Demand by Water Right Type",
                                                  
                                                  #### Demand by Water Right Type tab. ----
                                                  tabPanel("Demand by Water Right Type", 
                                                           
                                                           layout_column_wrap(
                                                             width = 1/2,
                                                             withSpinner(plotOutput(outputId = "dbwrt_plot")),
                                                             withSpinner(leafletOutput(outputId = "dbwrt_map",
                                                                                       height = "500px"))
                                                           )
                                                  ),
                                                  
                                                  #### Demand by Priority tab. ----
                                                  tabPanel("Demand by Priority",
                                                           
                                                           layout_column_wrap(
                                                             width = 1/2,
                                                             
                                                             withSpinner(plotOutput(outputId = "dbp_plot")),
                                                             
                                                             withSpinner(leafletOutput(outputId = "dbp_map",
                                                                                       height = "500px"))
                                                           )
                                                  ),
                                                  
                                                  #### Supply-Demand Scenarios tab. ----
                                                  tabPanel("Supply-Demand Scenarios",
                                                           
                                                           layout_column_wrap(
                                                             width = 1/2,
                                                             withSpinner(plotOutput(outputId = "vsd_plot")),
                                                             withSpinner(leafletOutput(outputId = "vsd_map",
                                                                                       height = "500px"))
                                                           ))
                                                  
                                 )
                        ),
                        
                        ### By Water Right tab. ----
                        tabPanel("By Water Right", "By Water Right"),
                        
                        ### Data Table tab. ----
                        tabPanel("Data",
                                 h3("Selected Demand Data"),
                                 #### Save data button. ----
                                 actionButton(inputId = "saveBtn",
                                              label = "Save", 
                                              width = "100px"),
                                 DTOutput(outputId = "demand_table")
                                 
                        ),
                        
                        ### California Watershed Map tab. ----
                        tabPanel("California Watershed Map", "California Watershed Map")
                      )
             ), # End Explore tab.
             
             ## Dataset Information tab. ----
             tabPanel(title = "Dataset Information",
                      icon = icon("table"), 
                      
                      navset_card_pill(
                        # selected = "Demand Scenarios",
                        
                        #### Demand Scenarios. ----
                        tabPanel("Demand Scenarios",
                                 icon = icon("faucet"),
                                 includeHTML("./docs/demand-scenarios.html")),
                        
                        #### Supply Scenarios. ----
                        tabPanel("Supply Scenarios",
                                 icon = icon("water"),
                                 "Supply Scenarios", br(),
                                 "Content Goes Here"),
                        
                        #### Other Data. ----
                        tabPanel("Other Data",
                                 icon = icon("table"),
                                 "Other Data", br(),
                                 "Content Goes Here")
                        
                      )
             ), # End Dataset Information tab.
             
             ## About/Help tab. ----
             tabPanel("About/Help", "About/Help"
                      
             ) # End About/Help tab.
           )
  )
)

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
    ifelse(length(input$d_scene_selected) == 1, 400,
           ifelse(length(input$d_scene_selected) == 2, 500, "auto"))
  })
  
  ## REACTIVES. ----
  
  # Filter watershed polygon.
  plot_poly <- reactive({
    req(input$huc8_selected)
    huc8_layer %>% filter(huc8_name %in% input$huc8_selected)
  })
  
  # Filter POD points.
  pod_points <- reactive({
    req(input$huc8_selected)
    pods %>%
      filter(huc8_name %in% input$huc8_selected) %>%
      mutate(vsd_fill_color = if_else(priority == "Statement Demand",
                                      "Statement Demand",
                                      if_else(p_year >= input$priority_selected,
                                              "Junior Post-14", "Post-14")))
  })
  
  # Filter gage stations.
  station_points <- reactive({
    req(input$huc8_selected)
    station_locs %>%
      filter(huc8_name %in% input$huc8_selected)
  })
  
  ### Update POD and Station Labels. ----
  
  # pod_points <- pod_points()
  # station_points <- station_points()
  
  # Create POD label look-up.
  pod_labs <- reactive({
    lapply(seq(nrow(pod_points())), function(i) {
      paste0('Water Right ID: ', st_drop_geometry(pod_points()[i, "wr_id"]), '<br>',
             'Owner: ', st_drop_geometry(pod_points()[i, "owner"]), '<br>',
             'Water Right Type: ', st_drop_geometry(pod_points()[i, "wr_type"]), '<br>',
             'Priority: ' , st_drop_geometry(pod_points()[i, "priority"]), '<br>',
             'Status: ', st_drop_geometry(pod_points()[i, "wr_status"])
      )
    })
  })
  
  station_labs <- reactive({   
    lapply(seq(nrow(station_points())), function(i) {
      paste0('<b>Station: </b>', station_points()[i, "station_id"], '<br>',
             '<b>Name: </b>', station_points()[i, "station_name"], '<br>',
             '<b>Data Provider: </b>', station_points()[i, "data_provider"], '<br>',
             '<b>', station_points()[i, "link"], '</b>')
    })
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
    # req(input$supply_filter)
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
    choices <- sort(unique(demand[[input$huc8_selected]]$d_scenario),
                    decreasing = TRUE)
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
    py_choices <- py_choice_list()[py_choice_list() > min(py_choice_list())]
    updateSelectInput(session, "priority_selected",
                      choices = py_choices,
                      selected = max(py_choice_list()))
  })
  
  ### Update water right type choices. ----
  observeEvent(input$huc8_selected, {
    req(input$huc8_selected)
    choices <- unique(demand[[input$huc8_selected]]$wr_type)
    updateCheckboxGroupInput(session = session,
                             inputId = "wrt_selected",
                             choices = choices,
                             selected = choices)
  })
  
   ## OUTPUTS. ----
  
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
  
  ### Build dataset. ----
  dbp_plot_data <- reactive({
    demand[[input$huc8_selected]] %>%
      filter(d_scenario %in% input$d_scene_selected) %>%
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
    # Validate.
    validate(
      need(input$huc8_selected,
           "No data to plot.\nPlease select a Watershed."),
      need(input$d_scene_selected,
           "No data to plot.\nPlease select at least one Demand Scenario."),
    )
    
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
  
  ## VSD - Supply-Demand Scenario plot. ----
  
  ### Build plot data frame. ----
  
  #### Demand plot data. ----
  vsd_plot_demand <- reactive({
    filter(demand[[input$huc8_selected]],
           d_scenario %in% input$d_scene_selected) %>%
      mutate(fill_color = if_else(priority == "Statement Demand",
                                  "Statement Demand",
                                  if_else(priority == "Statement Demand",
                                          "Statement Demand",
                                          if_else(p_year >= input$priority_selected,
                                                  "Junior Post-14", "Post-14"))),
             fill_color = ordered(fill_color, levels = wa_demand_order)) %>%
      group_by(d_scenario, plot_date, fill_color, plot_category) %>%
      summarise(af_monthly = sum(af_monthly, na.rm = TRUE),
                af_daily = sum(af_daily, na.rm = TRUE),
                cfs = sum(cfs, na.rm = TRUE),
                .groups = "drop") %>%
      mutate(plot_group = "demand",
             s_scenario = NA) %>%
      select(d_scenario,
             s_scenario,
             plot_date,
             fill_color,
             af_monthly,
             af_daily,
             cfs,
             plot_group,
             plot_category) %>%
      # Add boundary points to facilitate barplot vis and correct stacking.
      bind_rows(old = .,
                new = mutate(.,
                             plot_date = ceiling_date(x = plot_date,
                                                      unit = "month") - 1),
                .id = "source") %>%
      arrange(plot_date, source)
  })
  
  #### Supply plot data. ----
  vsd_plot_supply <- reactive({
    if( !is.null(supply[[input$huc8_selected]])) {
      build_plot_supply(supply[[input$huc8_selected]],
                        input$s_scene_selected,
                        input$d_scene_selected)
    } else {
      NA
    }
  })
  
  #### Combine plot data. ----
  vsd_plot_data <- reactive({
    rbind(vsd_plot_demand(),
          #         if(is.data.frame(vsd_plot_supply()) & mean(names(vsd_plot_supply()) == names(vsd_plot_demand())) == 1) vsd_plot_supply())
          if(is.data.frame(vsd_plot_supply())) vsd_plot_supply()) %>%
      mutate(plot_date = update(plot_date, year = 2000))
    
  })
  
  ### Render plot. ----
  output$vsd_plot <- renderPlot({
    
    # Validate.
    validate(
      need(input$d_scene_selected,
           "No data to plot.\nPlease slelect at least one Demand Scenario.")
    )
    
    # Render.
    ggplot(data = vsd_plot_data(),
           aes(x = plot_date,
               y = cfs)) +
      
      # Demand.
      geom_area(data = subset(vsd_plot_data(), plot_group == "demand"),
                position = "stack",
                aes(fill = fill_color)) +
      
      # Supply.
      geom_point(data = subset(vsd_plot_data(), plot_group == "supply"),
                 aes(color = s_scenario,
                     shape = s_scenario),
                 size = 7) +
      geom_line(data = subset(vsd_plot_data(), plot_group == "supply"),
                aes(color = s_scenario),
                linetype = "dashed") +
      
      # X axis format.
      scale_x_date(date_labels = "%b %d",
                   date_minor_breaks = "1 month") +
      
      # Y axis format.
      scale_y_continuous(labels = comma) +
      
      # Demand legend.
      scale_fill_manual(name = "Demand Priority:",
                        values = wa_demand_pal,
                        labels = c(paste(input$priority_selected,
                                         "& Junior Post-14 Demand"),
                                   paste(as.numeric(input$priority_selected) -1,
                                         "& Senior Post-14 Demand"),
                                   "Statement Demand"),
                        limits = sort(unique(vsd_plot_data()$fill_color))) +
      
      # Supply legend.
      scale_shape_manual(name = "Supply Scenario:",
                         values = wa_supply_shapes) +
      scale_color_manual(name = "Supply Scenario:",
                         values = wa_supply_pal) +
      
      # Facet on demand scenario.
      facet_wrap(~ d_scenario,
                 ncol = 1,
                 scales = "free_x") +
      
      # Labels.
      labs(y = "Cubic Feet per Second (cfs)") +
      
      # Theme.
      theme_minimal() +
      theme(
        plot.title = element_text(size = rel(2.0)),
        strip.text.x = element_text(size = rel(2.0)),
        axis.title = element_text(size = rel(1.2)),
        axis.text = element_text(size = rel(1.2)),
        legend.position = "bottom",
        legend.text = element_text(size = rel(1.2)),
        legend.title = element_text(size = rel(1.2)),
        legend.box = "horizontal",
        legend.direction = "vertical",
        panel.spacing = unit(2, "lines"),
        axis.title.x = element_blank()
      )
    
  }, height = function() plot_height()
  )
  
  ## Maps. ----
  
  ### Demand By Water Right Type (dbwrt_map). ----
  output$dbwrt_map <- renderLeaflet({
    
    # Validate.
    validate(
      need(nrow(pod_points()) > 0,
           paste0("No Data Available.\n",
                  "Please select other Watershed(s) or Water Right Type(s)."))
    )
    
    # Render.
    leaflet() %>%
      
      # Add base map.
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(group = "base",
                  data = plot_poly(),
                  weight = 3,
                  col = "blue",
                  fill = TRUE,
                  fillOpacity = 0) %>% 
      addCircleMarkers(group = "content",
                       data = pod_points(),
                       radius = 4,
                       fillOpacity = 0.8,
                       stroke = FALSE,
                       weight = 2,
                       fillColor = ~map_wrt_pal(wr_type),
                       label = lapply(pod_labs(), HTML)) %>%
      addLegend(position = "topright",
                pal = map_wrt_pal,
                values = pod_points()$wr_type,
                title = "Water Right Type",
                opacity = 1)
  })
  
  ### Demand By Priority (dbp_map). ----
  output$dbp_map <- renderLeaflet({
    
    # Validate.
    validate(
      need(nrow(pod_points()) > 0,
           paste0("No Data Available.\n",
                  "Please select other Watershed(s) or Water Right Type(s)."))
    )
    
    # Render.
    leaflet() %>%
      
      # Add base map.
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(group = "base",
                  data = plot_poly(),
                  weight = 3,
                  col = "blue",
                  fill = TRUE,
                  fillOpacity = 0) %>% 
      # POD points.
      addCircleMarkers(group = "content",
                       data = pod_points(),
                       radius = 4,
                       fillOpacity = 0.8,
                       stroke = FALSE,
                       weight = 2,
                       fillColor = ~map_priority_pal(priority),
                       label = lapply(pod_labs(), HTML)) %>%
      
      # Shape legend.
      addControl(html = "See plot legend for color categories.",
                 position = "topright")
  })
  
  ### Supply-Demand (vsd-map). ----
  output$vsd_map <- renderLeaflet({
    
    # Validate.
    validate(
      need(nrow(pod_points()) > 0,
           paste0("No Data Available.\n",
                  "Please select other Watershed(s) or Water Right Type(s)."))
    )
    
    # Render.
    leaflet() %>%
      
      # Add base map.
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(group = "base",
                  data = plot_poly(),
                  weight = 3,
                  col = "blue",
                  fill = TRUE,
                  fillOpacity = 0) %>%
      # POD points.
      addCircleMarkers(group = "content",
                       data = pod_points(),
                       radius = 4,
                       fillOpacity = 0.8,
                       stroke = FALSE,
                       weight = 2,
                       fillColor = ~map_demand_pal(vsd_fill_color),
                       label = lapply(pod_labs(), HTML)) %>%
      
      # Gage station markers.
      addMarkers(group = "content",
                 data = station_points(),
                 lat = ~lat,
                 lng = ~lng,
                 icon = station_icon,
                 popup = lapply(station_labs(), HTML)) %>%
      
      # Color legend.
      addLegend(position = "topright",
                colors = wa_demand_pal[1:3],
                labels = c(paste(input$priority_selected, "& Junior Post-14 Demand"),
                           paste(as.numeric(input$priority_selected) -1, "& Senior Post-14 Demand"),
                           "Statement Demand"),
                title = "Demand Priority",
                opacity = 1) %>%
      
      # Shape legend.
      addControl(html = html_legend,
                 position = "bottomleft")
  })
  
  ## Tables. ----
  
  demand_table <- reactive({
    req(input$huc8_selected)
    demand[[input$huc8_selected]] %>%
      filter(d_scenario %in% input$d_scene_selected,
             wr_type %in% input$wrt_selected) %>%
      mutate(plot_date = as.integer(month(plot_date))) %>%
      rename(month = plot_date) %>%
      select(-p_year)
  })
  
  ## Demand data table.
  output$demand_table <- renderDataTable({
    
    # Validate.
    validate(
      need(nrow(demand_table()) > 0,
           "No Data Available.")
    )
    
    # Render.
    datatable(demand_table(),
              # options = list(
              #   pageLength = 10,
              #   lengthMenu = c(10, 25, 50),
              #   autoWidth = TRUE,
              #   searching = FALSE,
              #   ordering = FALSE,
              #   info = FALSE,
              #   paging = FALSE,
              #   scrollX = TRUE,
              #   scrollY = "400px",
              #   fixedColumns = TRUE,
              #   fixedColumns = list(leftColumns = 1)
              filter = "top",
              rownames = FALSE)
  })
  
  ### Save datafile. ----
  observeEvent(input$saveBtn,{
    cat("\n\n* Saving table in directory: ", getwd())
    file_name <- 'test.csv'
    curr_dt_time <- as.character(Sys.time())
    curr_dt_time <- gsub(":", "_", curr_dt_time  )
    
    write.csv(demand_table(), paste0(file_name,"_last_save_",curr_dt_time,".csv"),
              row.names = FALSE
    )
    
  })
  
  
} # End server.

# APP --------------------------------------------------------------------------

# Run in a dialog within R Studio
# runGadget(ui, server, viewer = dialogViewer(dialogName = "DWR-WASDET DEVELOP",
#                                             width = 1600,
#                                             height = 1200))

shinyApp(ui = ui,
         server = server)











