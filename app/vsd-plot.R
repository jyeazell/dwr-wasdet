
### Load libraries. ----
source("m-load-libraries.R")


develop <- TRUE

input.huc8_selected <- "Battle Creek"
input.d_scene_selected <- "Reported Diversions - 2022"
input.s_scene_selected <- "Historic: Mean Unimpaired Flow at COTC1, Below Normal Year"
input.priority_selected <- 1931


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

### Build plot data frame. ----

#### Demand plot data. ----
vsd_plot_demand <- 
  filter(demand[[input.huc8_selected]], 
         d_scenario %in% input.d_scene_selected) %>%
    mutate(fill_color = if_else(priority == "Statement Demand",
                                "Statement Demand",
                                if_else(priority == "Statement Demand",
                                        "Statement Demand",
                                        if_else(p_year >= input.priority_selected,
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

#### Supply plot data. ----
vsd_plot_supply <-
  if( !is.null(supply[[input.huc8_selected]])) {
    build_plot_supply(supply[[input.huc8_selected]], 
                      input.s_scene_selected, 
                      input.d_scene_selected)
  } else {
    NA
  }


#### Combine plot data. ----
vsd_plot_data <- reactive({
  rbind(vsd_plot_demand(), 
        #         if(is.data.frame(vsd_plot_supply()) & mean(names(vsd_plot_supply()) == names(vsd_plot_demand())) == 1) vsd_plot_supply())
        if(is.data.frame(vsd_plot_supply())) vsd_plot_supply())
  
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
  
}, height = function() plot_height())
