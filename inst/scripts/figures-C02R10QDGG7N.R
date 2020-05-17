library(tidyverse)
library(cowplot)
library(covidsch)

inf <- read_rds("data-raw/infection_rates.rds")

case_plot(inf)
