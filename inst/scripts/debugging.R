library(tidyverse)

case_data <- outbreak_setup(net = haslemere,
                            df = school_data,
                            num.initial.cases = 5,
                            incfn = dist_setup(dist_shape = 2.322737,dist_scale = 6.492272),
                            asym.adult = 0.4,
                            asym.child = 0.8,
                            delayfn = dist_setup(dist_shape = 1,dist_scale = 1.4),
                            isolation = FALSE)



for(i in 1:70)
{
  case_data <- outbreak_step(case_data = case_data,
                             day = i,
                             net = haslemere,
                             incfn = dist_setup(dist_shape = 2.322737,dist_scale = 6.492272),
                             delayfn = dist_setup(dist_shape = 1,dist_scale = 1.4),
                             prop.ascertain = 0,
                             R = 1,
                             presymrate = 0.4,
                             quarantine = FALSE,
                             isolation = FALSE,
                             tracing = FALSE,
                             secondary = FALSE,
                             asym.child = 0.8,
                             asym.adult = 0.4,
                             asym.adult.inf = 0.5,
                             sym.child.inf = 0.5,
                             asym.child.inf = 0.1,
                             outside = 0.001,
                             sensitivity = "high",
                             testing = "none")
}
sum(!is.na(case_data$exposure))
sum(case_data$exposure < min(case_data$isolated_time),na.rm= T)
