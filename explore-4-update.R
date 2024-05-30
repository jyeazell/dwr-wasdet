# Load libraries. ----
if (!("package:aws.s3" %in% search())) {         # aws.s3
  suppressMessages(library(aws.s3))
}
if (!("package:wesanderson" %in% search())) {    # wesanderson
  suppressMessages(library(wesanderson))
}
if (!("package:leaflet" %in% search())) {        # leaflet
  suppressMessages(library(leaflet))
}
if (!("package:lubridate" %in% search())) {      # lubridate
  suppressMessages(library(lubridate))
}
if (!("package:readr" %in% search())) {          # readr
  suppressMessages(library(readr))
}
if (!("package:scales" %in% search())) {         # scales
  suppressMessages(library(scales))
}

loadData <- function() {
  
  ## Load S3 keys. ----
  source("load-s3-keys.R")
  
  
  
  ## Load Water Right Info. ----
  load("./output/wasdet-wrinfo.RData")
  
  ## Load Demand data. ----
  load("./output/wasdet-demands.RData")
  
  ## Load Supply data. ----
  aws.s3::s3load(object = "wasdet-supplies.RData",
                 bucket = "dwr-shiny-apps")
  
  # Load local data. ----
  
  ## Gage station location information. ----
  station_locs <- readr::read_csv("./app/common/station-locations.csv")
}

loadData()


# Define color and shape aesthetics. ----

# Demand.
wa_demand_order <- ordered(c("Junior Post-14",
                             "Post-14",
                             "Statement Demand",
                             "Environmental Demand"))
wa_demand_pal <- c(wesanderson::wes_palettes$GrandBudapest1[c(2, 1)], "#BEBEBE", "#000000")
names(wa_demand_pal) <- wa_demand_order
map_demand_pal <- leaflet::colorFactor(palette = wa_demand_pal,
                                       levels = names(wa_demand_pal))

# Water right type.
plot_wrt_pal <- c(wesanderson::wes_palette("Darjeeling1"),
                  wesanderson::wes_palette("Darjeeling2"))[2:10]
names(plot_wrt_pal) <- sort(unique(wr_info$wr_type))
map_wrt_pal <- leaflet::colorFactor(palette = plot_wrt_pal,
                                    domain = wr_info$wr_type)

# Priority.
priority_order <- c(c(year(now()):1914),
                    "Statement Demand", "Environmental Demand")
priority_pal <- c(scales::viridis_pal()(length(c(year(now()):1914))),
                  "#BEBEBE", "#000000")
names(priority_pal) <- priority_order
map_priority_pal <- colorFactor(palette = priority_pal,
                                levels = names(priority_pal))

# Historical and Forecast supply.
wa_supply_pal <- colorRampPalette(wes_palette("Rushmore")[3:4])(3)
wa_supply_shapes <- c(15, 16, 17)

# Current-year supply.
cy_supply_pal <- "blue"
