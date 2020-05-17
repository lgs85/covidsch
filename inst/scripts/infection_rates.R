###########################################
#SIMULATE CHILD INFECTION RATES
###########################################

library(covidsch)
library(dplyr)
library(purrr)

# Set number of replicates ------------------------------------------------

nreps <- 50


## Set up multicore if using see ?future::plan for details
## Use the workers argument to control the number of cores used.
future::plan("multiprocess")


# Set up partial function -------------------------------------------------

scenario_sim2 <- partial(scenario_sim, net = haslemere, n.sim = nreps, num.initial.cases = 1,prop.asym=0.4,
                         prop.ascertain = 0.8, cap_max_days = 69,
                         delay_shape = 1, delay_scale = 1.4,R = 6.5,presymrate = 0.4,
                         outside = 0.001, sensitivity = "high", testing = "none",cap_max_tests = Inf)

# Simulate scenarios ------------------------------------------------------

scenario_sim2 <- purrr::partial(scenario_sim,
                                n.sim = nreps, net = net.sch, df = name.class,prop.ascertain = 0,
                                cap_max_days = 30, R = 6.5, presymrate = 0.4,
                                delay_shape = 1, delay_scale = 1.4, num.initial.cases = 1,
                                scenario = "nothing",outside = 0.001, sensitivity = "high",
                                testing = "none", output = "daily", cap_max_tests = NULL)

res1 <- scenario_sim2(asym.adult = 0.4, asym.child = 0.9,asym.adult.inf = 0.5,
                      sym.child.inf = 1, asym.child.inf = 0.5)
res2 <- scenario_sim2(asym.adult = 0.4, asym.child = 0.9,asym.adult.inf = 0.5,
                      sym.child.inf = 0.5, asym.child.inf = 0.25)
res3 <- scenario_sim2(asym.adult = 0.4, asym.child = 0.9,asym.adult.inf = 0.5,
                      sym.child.inf = 0.1, asym.child.inf = 0.05)



# Bind together results and save output -----------------------------------

res <- bind_rows(res1,res2,res3) %>%
  mutate(intervention = rep(c("Children as infectious as adults",
                              "Children as half infectious as adults",
                              "Children 10% as infectious as adults"),
                                each = nrow(res1)))


saveRDS(res, file = "data-raw/infection_rates.rds")
