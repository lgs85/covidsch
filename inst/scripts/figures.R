library(tidyverse)
library(cowplot)
library(covidsch)



# Network plot ------------------------------------------------------------

load("data-raw/school_data_2.Rdata")

plot_sch_network2 <- purrr::partial(plot_sch_network,
                                    am = am2.sch,
                                    df = name.class,
                                    groups = name.class$classa,
                                    num.initial.cases = 1,
                                    prop.ascertain = 0,
                                    R = 6.5,
                                    asym.adult = 0.4, asym.child = 0.9,asym.adult.inf = 0.5,
                                    sym.child.inf = 0.1, asym.child.inf = 0.05,
                                    presymrate = 0.4,
                                    delay_shape = 1,
                                    delay_scale = 1.4,
                                    num.initial.cases = 1,
                                    isolation = FALSE,
                                    quarantine = FALSE,
                                    tracing = FALSE,
                                    secondary = FALSE,
                                    outside = 0,
                                    sensitivity = "high",
                                    testing = "none",
                                    s = 444)




png("inst/plots/sch_network.png",
    height = 16,
    width = 16,
    units = "in",
    res = 150

    )

layout(matrix(1:4,2,2,byrow = T))
par(mar = c(0,0,1,0))
plot_sch_network2(day = 1)
text(0.2,2,"Day 0",cex = 2)

plot_sch_network2(day = 5)
text(0.2,2,"Day 5",cex = 2)

plot_sch_network2(day = 10)
text(0.2,2,"Day 10",cex = 2)

plot_sch_network2(day = 30)
text(0.2,2,"Day 30",cex = 2)

dev.off()



# 90% asym children -------------------------------------------------------

inf <- read_rds("data-raw/infection_rates_asym90.rds")

labs <- unique(inf$intervention)
figa <- case_plot(filter(inf,intervention == labs[1]))+
  theme(legend.position = "top")+
  ylim(c(0,250))
legend <- get_legend(figa)
figa <- figa+theme(legend.position = "none")+
  ggtitle(labs[1])
figb <- case_plot(filter(inf,intervention == labs[2]))+
  theme(legend.position = "none")+
  ggtitle(labs[2])+
  ylim(c(0,250))
figc <- case_plot(filter(inf,intervention == labs[3]))+
  theme(legend.position = "none")+
  ggtitle(labs[3])+
  ylim(c(0,250))

linegraphs <- plot_grid(figa,figb,figc,nrow = 1)

plot_sch_network2 <- purrr::partial(plot_sch_network,
                                    am = am2.sch,
                                    day = 10,
                                    df = name.class,
                                    groups = name.class$classa,
                                    num.initial.cases = 1,
                                    prop.ascertain = 0,
                                    R = 6.5,
                                    asym.adult = 0.4, asym.child = 0.9,asym.adult.inf = 0.5,
                                    presymrate = 0.4,
                                    delay_shape = 1,
                                    delay_scale = 1.4,
                                    num.initial.cases = 1,
                                    isolation = FALSE,
                                    quarantine = FALSE,
                                    tracing = FALSE,
                                    secondary = FALSE,
                                    outside = 0,
                                    sensitivity = "high",
                                    testing = "none",
                                    s = 444)


figd <- function(){
  par(mar = c(0,0,0,0))
plot_sch_network2(sym.child.inf = 1, asym.child.inf = 0.5)
}

fige <- function(){
  par(mar = c(0,0,0,0))
  plot_sch_network2(sym.child.inf = 0.5, asym.child.inf = 0.25)
}

figf <- function(){
  par(mar = c(0,0,0,0))
  plot_sch_network2(sym.child.inf = 0.1, asym.child.inf = 0.05)
}

nets <- plot_grid(figd,fige,figf,nrow = 1)


png("inst/plots/infection_rates90.png",
    height = 18,
    width = 24,
    units = "in",
    res = 150)
plot_grid(legend,linegraphs,nets,nrow = 3,rel_heights = c(0.1,0.8,1))
dev.off()

# 40% asym children -------------------------------------------------------

inf <- read_rds("data-raw/infection_rates_asym90.rds")

labs <- unique(inf$intervention)
figa <- case_plot(filter(inf,intervention == labs[1]))+
  theme(legend.position = "top")+
  ylim(c(0,250))
legend <- get_legend(figa)
figa <- figa+theme(legend.position = "none")+
  ggtitle(labs[1])
figb <- case_plot(filter(inf,intervention == labs[2]))+
  theme(legend.position = "none")+
  ggtitle(labs[2])+
  ylim(c(0,250))
figc <- case_plot(filter(inf,intervention == labs[3]))+
  theme(legend.position = "none")+
  ggtitle(labs[3])+
  ylim(c(0,250))

linegraphs <- plot_grid(figa,figb,figc,nrow = 1)

plot_sch_network2 <- purrr::partial(plot_sch_network,
                                    am = am2.sch,
                                    day = 10,
                                    df = name.class,
                                    groups = name.class$classa,
                                    num.initial.cases = 1,
                                    prop.ascertain = 0,
                                    R = 6.5,
                                    asym.adult = 0.4, asym.child = 0.4,asym.adult.inf = 0.5,
                                    presymrate = 0.4,
                                    delay_shape = 1,
                                    delay_scale = 1.4,
                                    num.initial.cases = 1,
                                    isolation = FALSE,
                                    quarantine = FALSE,
                                    tracing = FALSE,
                                    secondary = FALSE,
                                    outside = 0,
                                    sensitivity = "high",
                                    testing = "none",
                                    s = 444)


figd <- function(){
  par(mar = c(0,0,0,0))
  plot_sch_network2(sym.child.inf = 1, asym.child.inf = 0.5)
}

fige <- function(){
  par(mar = c(0,0,0,0))
  plot_sch_network2(sym.child.inf = 0.5, asym.child.inf = 0.25)
}

figf <- function(){
  par(mar = c(0,0,0,0))
  plot_sch_network2(sym.child.inf = 0.1, asym.child.inf = 0.05)
}

nets <- plot_grid(figd,fige,figf,nrow = 1)


png("inst/plots/infection_rates40.png",
    height = 18,
    width = 24,
    units = "in",
    res = 150)
plot_grid(legend,linegraphs,nets,nrow = 3,rel_heights = c(0.1,0.8,1))
dev.off()













