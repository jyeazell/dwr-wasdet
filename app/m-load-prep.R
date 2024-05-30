## Build plot supply data frame.
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

# Load data from AWS S3 bucket. ----

## Load Water Right Info. ----
#if(develop) {
  load("data/wasdet-wrinfo.RData")
#} else {
#  s3load(object = "wasdet-wrinfo.RData",
#         bucket = "dwr-shiny-apps")
#}

## Load Demand Data. ----
#if(develop) {
  load("data/wasdet-demands.RData")
#} else {
#  s3load(object = "wasdet-demands.RData", 
#         bucket = "dwr-shiny-apps")
##}

## Load Supply data. ----
#if(develop) {
  load("data/wasdet-supplies.RData")
#} else {
#  s3load(object = "wasdet-supplies.RData",
#         bucket = "dwr-shiny-apps")
#}

# Load local data. ----

## Gage station location information. ----
station_locs <- read_csv("./common/station-locations.csv")

# Define color and shape aesthetics. ----

## Demand.
wa_demand_order <- ordered(c("Junior Post-14",
                             "Post-14",
                             "Statement Demand",
                             "Environmental Demand"))
wa_demand_pal <- c(wes_palettes$GrandBudapest1[c(2, 1)], "#BEBEBE", "#000000")
names(wa_demand_pal) <- wa_demand_order
map_demand_pal <- colorFactor(palette = wa_demand_pal,
                              levels = names(wa_demand_pal))

# Water right type.
plot_wrt_pal <- c(wes_palette("Darjeeling1"),
                  wes_palette("Darjeeling2"))[2:10]
names(plot_wrt_pal) <- sort(unique(wr_info$wr_type))
map_wrt_pal <- colorFactor(palette = plot_wrt_pal,
                           domain = wr_info$wr_type)

# Priority.
priority_order <- c(c(year(now()):1914),
                    "Statement Demand", "Environmental Demand")
priority_pal <- c(viridis_pal()(length(c(year(now()):1914))),
                  "#BEBEBE", "#000000")
names(priority_pal) <- priority_order
map_priority_pal <- colorFactor(palette = priority_pal,
                                levels = names(priority_pal))

# Historical and Forecast supply.
wa_supply_pal <- colorRampPalette(wes_palette("Rushmore")[3:4])(3)
wa_supply_shapes <- c(15, 16, 17)

# Current-year supply.
cy_supply_pal <- "blue"

# VSD Plot theme.
vsd_plot_theme <-  theme(
  legend.box = "vertical",
  legend.direction = "horizontal",
  axis.title.x = element_blank()
)

# Mini map icons.
station_icon <- icons(iconUrl = "./www/x-diamond-fill.svg")

# Icon legend for vsd plot.
html_legend <-
  "<img src='circle-fill.svg' style='width:8px;height:8px;'> Point of Diversion<br/>
<img src='x-diamond-fill.svg'> Gage Station"
