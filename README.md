# Assessment of contact tracing, social distancing, and real-world social tracking data for control of COVID-19 

WORKING CODE WHICH MAY NOT RUN YET

This repository contains code for simulating COVID-19 dynamics in a range of scenarios across a real-world social network. The epidemic model is based conceptually on a branching-process model of contact-tracing and COVID-19, which can be accessed here:

https://github.com/cmmid/ringbp



## Abstract
Currently, non-pharmaceutical interventions are the only option for controlling the spread of SARS-CoV-2. As well as wider social distancing measures, case isolation combined with detailed tracing of contacts could contribute to the ongoing control of COVID-19 outbreaks, in line with the World Health Organisation’s “test, trace, isolate” recommendation. However, extensive contact tracing may require a large proportion of the population to be quarantined, and it remains unclear how to best combine contact tracing with other social distancing strategies. Further, real-world social tracking data is limited, and we do not understand how real-world dynamic networks could influence the success of targeted control strategies. We examined the effectiveness of control strategies for SARS-CoV-2 across the ‘Haslemere network’, a real-world social network generated from three full days of continuous and simultaneous mobile-phone app-based tracking of 468 individuals within a set area as part of a recent BBC television documentary. We show that relevant social contact events can be robustly determined from this data, and that social contact data generally predicts exposure to contagion processes. By applying an empirically-parameterised epidemic model to the real-world social network, we found that case isolation and quarantining of primary and secondary contacts substantially reduced the size of simulated outbreaks, with secondary contact tracing reducing outbreak size to a greater extent than primary contact tracing. However, secondary contact tracing required up to 80% of the population to be quarantined during outbreak peaks for effective control. Testing and releasing non-infectious individuals reduced the numbers of quarantined individuals without large increases in outbreak size, but very high testing rates were required for this to be effective. Finally, we show that a combination of social distancing with contact tracing and testing may enable epidemic control while reducing the number of quarantined individuals, and without requiring unfeasibly high testing rates. In such scenarios when rates of social contact are reduced, primary contact tracing may be more efficient than secondary contact tracing, as it achieves similar levels of outbreak control but requires fewer individuals to be quarantined and tested. Our approach highlights the utility of real-world networks when considering epidemic spread, and provides new insight into how contact tracing and social distancing strategies can be combined to control COVID-19 outbreaks.

## Usage

### Set up

Set your working directory to the parent directory of the project folder. Install the analysis and all dependencies with: 

```r
devtools::install("covidhm", dependencies = TRUE) #or whatever your folder name is
```

### Run a single scenario

Run a single scenario for a 10 simulations. Use `?scenario_sim` for an explanation of parameters.

```r
library(covidhm)
library(ggplot2)

res <- scenario_sim(net = haslemere, n.sim = 10, num.initial.cases = 5,prop.asym=0.4,
                             prop.ascertain = 0.4, cap_max_days = 70,
                             delay_shape = 1, delay_scale = 1.4, R = 6.5, presymrate = 0.4, scenario = "nothing",
                             sensitivity = "high", testing = "none", outside = 0.001)

# Plot of raw cumulative cases
ggplot(data=res, aes(x=week, y=cumcases,col = sim)) +
geom_line(show.legend = FALSE, alpha=0.6, aes(group = sim)) +
scale_y_continuous(name="Weekly number of cases") +
  theme_bw()

```

### Run the full analysis

Run the analyses in the terminal with the following commands:

```bash

Rscript inst/scripts/scenarios.R
Rscript inst/scripts/network.R
Rscript inst/scripts/distancing.R
Rscript inst/scripts/distancing2.R
Rscript inst/scripts/outside.R
Rscript inst/scripts/sensitivity_testing.R

```

### Generate figures

Render figures with the following:

```bash
Rscript inst/scripts/figures.R

```
