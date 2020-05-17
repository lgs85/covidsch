rm(list=ls())

library(tidyverse)
library(covidsch)
library(cowplot)

load("data-raw/school_data_2.RData")

net.sch <- format_network(am2.sch,idvec = name.class$id)

case_data <- outbreak_setup(net = net.sch,
                            df = name.class,
                            num.initial.cases = 1,
                            incfn = dist_setup(dist_shape = 2.322737,dist_scale = 6.492272),
                            asym.adult = 0.4,
                            asym.child = 0.8,
                            delayfn = dist_setup(dist_shape = 1,dist_scale = 1.4),
                            isolation = FALSE)



for(i in 1:70)
{
  case_data <- outbreak_step(case_data = case_data,
                             day = i,
                             net = net.sch,
                             incfn = dist_setup(dist_shape = 2.322737,dist_scale = 6.492272),
                             delayfn = dist_setup(dist_shape = 1,dist_scale = 1.4),
                             prop.ascertain = 0,
                             R = 1,
                             presymrate = 0.4,
                             quarantine = FALSE,
                             isolation = FALSE,
                             tracing = FALSE,
                             secondary = FALSE,
                             asym.adult = 0.4,
                             asym.child = 0.8,
                             asym.adult.inf = 0.5,
                             sym.child.inf = 0.5,
                             asym.child.inf = 0.1,
                             outside = 0,
                             sensitivity = "high",
                             testing = "none")
}

save(case_data,file = "data-raw/sch_cont_ex.Rdata")



nreps <- 50

scenario_sim2 <- purrr::partial(scenario_sim,
                               n.sim = nreps, net = net.sch, df = name.class,prop.ascertain = 0,
                               cap_max_days = 30, R = 6.5, presymrate = 0.4,
                               delay_shape = 1, delay_scale = 1.4, num.initial.cases = 1,
                               scenario = "nothing",outside = 0.001, sensitivity = "high",
                               testing = "none", output = "daily", cap_max_tests = NULL)

out1 <- scenario_sim2(asym.adult = 0.4, asym.child = 0.9,asym.adult.inf = 0.5,
                      sym.child.inf = 1, asym.child.inf = 0.5)
out2 <- scenario_sim2(asym.adult = 0.4, asym.child = 0.9,asym.adult.inf = 0.5,
                      sym.child.inf = 0.5, asym.child.inf = 0.25)
out3 <- scenario_sim2(asym.adult = 0.4, asym.child = 0.9,asym.adult.inf = 0.5,
                      sym.child.inf = 0.1, asym.child.inf = 0.05)

a <- case_plot(out1)
legend <- get_legend(a)
a <- a+theme(legend.position = "none")+
  ggtitle("Children as infectious as adults")
b <- case_plot(out2)+theme(legend.position = "none")+
  ggtitle("Children half as infectious as adults")
c <- case_plot(out3)+theme(legend.position = "none")+
  ggtitle("Children 10% as infectious as adults")


plot_grid(a,b,c,legend,nrow = 2)
