# Load libraries. ----

if (!("package:aws.s3" %in% search())) { # aws.s3
  suppressMessages(library(aws.s3))
}




# Load data from AWS S3 bucket. ----

## Load S3 keys. ----
source("load-s3-keys.R")

## Load Water Right Info. ----
s3load(object = "wasdet-wrinfo.RData",
       bucket = "dwr-shiny-apps")
