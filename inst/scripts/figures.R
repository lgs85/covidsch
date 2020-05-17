library(tidyverse)
library(cowplot)
library(covidsch)

inf <- read_rds("data-raw/infection_rates.rds")

labs <- unique(inf$intervention)
a <- case_plot(filter(inf,intervention == labs[1]))
legend <- get_legend(a)
a <- a+theme(legend.position = "none")+
  ggtitle(labs[1])
b <- case_plot(filter(inf,intervention == labs[2]))+
  theme(legend.position = "none")+
  ggtitle(labs[2])
c <- case_plot(filter(inf,intervention == labs[3]))+
  theme(legend.position = "none")+
  ggtitle(labs[3])

plot_grid(a,b,c,legend,nrow = 2)

ggsave("inst/plots/infection_rates.pdf")
