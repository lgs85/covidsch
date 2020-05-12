library(devtools)
library(ringbp)

load("data-raw/am_list.RData")

haslemere <- format_network(am_list[[1]])
usethis::use_data(haslemere, internal = TRUE)
