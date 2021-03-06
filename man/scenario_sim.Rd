% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/scenario_sim.R
\name{scenario_sim}
\alias{scenario_sim}
\title{Run a specified number of simulations with identical parameters}
\usage{
scenario_sim(
  n.sim = 1,
  net,
  df,
  prop.ascertain,
  cap_max_days,
  R,
  presymrate,
  delay_shape,
  delay_scale,
  num.initial.cases,
  asym.adult,
  asym.child,
  asym.adult.inf,
  sym.child.inf,
  asym.child.inf,
  scenario,
  outside,
  sensitivity = "high",
  testing = "none",
  output = "daily",
  cap_max_tests = NULL
)
}
\arguments{
\item{n.sim}{number of simulations to run}

\item{net}{network from which to sample cases}

\item{prop.ascertain}{Probability that cases are ascertained by contact tracing}

\item{cap_max_days}{Maximum number of days to run process for}

\item{R}{scaling factor for infection probability}

\item{presymrate}{rate of presymptomatic transmission (must be 0 <= x <= 1)}

\item{delay_shape}{shape of distribution for delay between symptom onset and isolation}

\item{delay_scale}{scale of distribution for delay between symptom onset and isolation}

\item{num.initial.cases}{Initial number of cases in each initial cluster (must be 0 <= x <= 1)}

\item{scenario}{character - must be either ("nothing","isolation","primary_quarantine", or "secondary_quarantine")}

\item{outside}{infection rate from outside the network (must be 0 <= x <= 1)}

\item{sensitivity}{character - must be "high" or "low"}

\item{testing}{character - must be "realistic", "random" or "none"}

\item{cap_max_tests}{integer - max number of daily tests. Only use if testing != "none"}

\item{prop.asym}{proportion of asymptomatic cases (must be 0 <= x <= 1)}
}
\value{
data.frame of weekly cases, isolations, quarantines and tests for each simulation
}
\description{
Run a specified number of simulations with identical parameters
}
\examples{
\dontrun{
res <- scenario_sim(n.sim = 5,
net = haslemere,
prop.ascertain = 0.8,
cap_max_days = 69,
R = 6.5,
presymrate = 0.4,
delay_shape = 1,
delay_scale = 1.4,
num.initial.cases = 1,
prop.asym = 0.4,
scenario = "nothing",
outside = 0.001,
sensitivity = "high",
testing = "none")
}

}
\author{
Joel Hellewell

Lewis Spurgin
}
