library(tidyr)
library(dplyr)
library(readr)
library(janitor)
library(lubridate)
library(purrr)
library(future)
library(furrr)
library(aws.s3)

# Initialization. ----

## Switches. ----
write_data <- TRUE
save_to_aws <- TRUE
download_divs <- TRUE
save_test_set <- TRUE

# set up parallel R sessions.
plan(multisession)

# Load functions.
if(download_divs) source("f_getReportedDivs_FF.R")

# Define variables.

# priority_order <- c(c(plot_year:1914),  
#                     "Statement Demand", 
#                     "Environmental Demand")

# Load wr_info data.
load("./output/wasdet-wrinfo.RData")

# Download diversion data from WRUDS link.
if(download_divs) {
  divs_fname <- getReporteDivsCSV()
} else {
  div_data_files <- file.info(list.files("./wruds_downloads/", 
                                         full.names = T))
  divs_fname <- rownames(div_data_files)[which.max(div_data_files$mtime)]
}

# Load diversion data, filter for reporting years 2009 on.
diversions_raw <- read_csv(divs_fname) %>% clean_names()

# Set plot year.
plot_year <- max(diversions_raw$year)

# Fix water year reporting bug so that data is in correct time-series format. ----
# Repair data.
fixData <- function(x) {
  
  # WY 2022 data:
  # Extract Oct, Nov, and Dec 2022 rows and rewind to 2021.
  t1 <- x %>% 
    filter(year == 2022,
           month %in% c(10:12)) %>% 
    mutate(year = 2021)
  
  # Cut offending Oct, Nov, Dec rows for 2021 and 2022.
  x <- x %>% 
    filter(!(year %in% c(2021, 2022) & month %in% c(10:12)))
  
  # Bind corrected Oct, Nov, Dec 2021 rows.
  x <- bind_rows(x, t1)
  
  # WY 2023 data:
  # Extract Oct, Nov, and Dec 2023 rows and rewind to 2022.
  t2 <- x %>% 
    filter(year == 2023,
           month %in% c(10:12)) %>% 
    mutate(year = 2022)
  
  # Cut offending Oct, Nov, Dec rows for 2022 and 2023.  
  x <- x %>% 
    filter(!(year %in% c(2022, 2023) & month %in% c(10:12)))
  
  # Bind corrected Oct, Nov, Dec 2022 rows.
  x <- bind_rows(x, t2) %>% 
    arrange(appl_id, year, month)
  
  # Return result.
  return(x)
}

diversions_repaired <- fixData(diversions_raw)

diversions <- diversions_repaired %>% 
  select(d_scenario = year,
         wr_id = appl_id,
         rept_month = month,
         af_monthly = amount,
         everything(),
         -water_right_id) %>% 
  filter(d_scenario >= 2009 & d_scenario <= (plot_year))

# Re-code scenario name to be more descriptive, add plot_category.
diversions <- diversions %>% 
  mutate(d_scenario = paste0("Reported Diversions - ", d_scenario),
         plot_category = "demand")

# join wr_info.
diversions <- diversions %>% 
  left_join(., select(wr_info,
                      huc8_name,
                      wr_id, 
                      owner,
                      wr_type,
                      wr_status,
                      priority,
                      demand_wt), 
            by = "wr_id")

# Split diversions into list of tibbles by scenario.
demand <- split(x = diversions, 
                f = diversions$huc8_name)
demand <- future_map(.x = demand,
                     .f = ~ select(., -huc8_name))

# Scale diversion amounts by HUC-8 weighting factor.
demand <- future_map(.x = demand,
                     .f = ~mutate(., af_monthly = af_monthly * demand_wt))

# Aggregate diversions by water right id. 
agg_dem_wrid <- function(x) {
  x <- x %>% 
    rowwise() %>% 
    group_by(d_scenario, wr_id, rept_month) %>% 
    mutate(af_monthly = sum(af_monthly, na.rm = TRUE) * demand_wt) %>% 
    select(-diversion_type) %>% 
    distinct() %>% 
    ungroup()
}
demand <- future_map(.x = demand,
                     .f = agg_dem_wrid,
                     .progress = TRUE)

## Demands are AF/month. Plots will be on a daily time step. Calculate 
## month-averaged daily AF and cfs. Map to date time series with plot_year.

# Create vector containing series of dates for project year.
dates_to_map <- tibble(plot_date = seq(as.Date(paste0(plot_year, 
                                                      "-01-01")),
                                       as.Date(paste0(plot_year, 
                                                      "-12-31")),
                                       by = "month"),
                       rept_month = as.numeric(month(plot_date)))

make_daily_demands <- function(x) {
  x <- x %>% 
    right_join(., dates_to_map, by = "rept_month") %>%
    mutate(af_daily = af_monthly / as.numeric(days_in_month(plot_date)),
           cfs = af_daily * 0.504166667) %>% 
    select(d_scenario,
           wr_id,
           owner,
           wr_type,
           wr_status,
           priority,
           plot_category,
           plot_date,
           af_monthly,
           af_daily,
           cfs) %>% 
    arrange(d_scenario,
            plot_date, 
            priority)
}
demand <- future_map(.x = demand,
                     .f = make_daily_demands,
                     .progress = TRUE)

# Make p_year column. Introduces NAs by coercion, but that's ok.
make_numeric_priority <- function(x) {
  x <- x %>% 
    # Check that it doesn't match any non-number
    #  numbers_only <- function(x) !grepl("\\D", x)
    mutate(p_year = ifelse(!grepl("\\D", .$priority), 
                           suppressWarnings(as.numeric(priority)),
                           NA))
}
demand <- map(.x = demand,
              .f = make_numeric_priority)

# Kill parallel R sessions. 
plan(sequential)

## Save data files locally and to S3 bucket. ----

# Set data create date.
demand_create_date <- Sys.Date()

# Save to S3 for Shiny app to pick up.
if(save_to_aws) {
  outfile_loc <- "./output/wasdet-demands.RData"
  save(demand,
       demand_create_date,
       file = outfile_loc)
  put_object(file = outfile_loc,
             object = "wasdet-demands.RData",
             bucket = "dwr-shiny-apps",
             multipart = TRUE)
}

if (write_data) {
  # Save demand dataset locally.
  file_loc <- "./output/wasdet-demands.RData"
  save(demand,
       demand_create_date,
       file = file_loc)
  }

if (save_test_set) {
  # Save demand test set for shorter load times.
  # demand_test_set <- demand[grepl("Upper", names(demand))]
  demand_test_set <- demand[sample(1:length(demand), 20)]   
  test_data_loc <- "./output/wasdet-demands-test-set.RData"
  save(demand_test_set,
       demand_create_date,
       file = test_data_loc)
  put_object(file = test_data_loc,
             object = "wasdet-demands-test-set.RData",
             bucket = "dwr-shiny-apps",
             multipart = TRUE)
}




















