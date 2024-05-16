# Load libraries. ----
if (!("package:aws.s3" %in% search())) {
  suppressMessages(library(aws.s3))
}
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
if (!("package:htmltools" %in% search())) {
  suppressMessages(library(htmltools))
}
if (!("package:ggplot2" %in% search())) {
  suppressMessages(library(ggplot2))
}
if (!("package:leaflet" %in% search())) {
  suppressMessages(library(leaflet))
}
if (!("package:sf" %in% search())) {
  suppressMessages(library(sf))
}
if (!("package:wesanderson" %in% search())) {
  suppressMessages(library(wesanderson))
}
if (!("package:tidyr" %in% search())) {
  suppressMessages(library(tidyr))
}
if (!("package:dplyr" %in% search())) {
  suppressMessages(library(dplyr))
}
# if (!("package:spdplyr" %in% search())) {
#   suppressMessages(library(spdplyr))
# }
if (!("package:readr" %in% search())) {
  suppressMessages(library(readr))
}
if (!("package:lubridate" %in% search())) {
  suppressMessages(library(lubridate))
}
if (!("package:scales" %in% search())) {
  suppressMessages(library(scales))
}
if (!("package:DT" %in% search())) {
  suppressMessages(library(DT))
}
