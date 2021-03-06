#' Run a specified number of simulations with identical parameters
#' @author Joel Hellewell
#' @author Lewis Spurgin
#' @param n.sim number of simulations to run
#' @param net network from which to sample cases
#' @param num.initial.cases Initial number of cases in each initial cluster (must be 0 <= x <= 1)
#' @param prop.ascertain Probability that cases are ascertained by contact tracing
#' @param cap_max_days Maximum number of days to run process for
#' @param delay_shape shape of distribution for delay between symptom onset and isolation
#' @param delay_scale scale of distribution for delay between symptom onset and isolation
#' @param scenario character - must be either ("nothing","isolation","primary_quarantine", or "secondary_quarantine")
#' @param R scaling factor for infection probability
#' @param presymrate rate of presymptomatic transmission (must be 0 <= x <= 1)
#' @param prop.asym proportion of asymptomatic cases (must be 0 <= x <= 1)
#' @param outside infection rate from outside the network (must be 0 <= x <= 1)
#' @param sensitivity character - must be "high" or "low"
#' @param testing character - must be "realistic", "random" or "none"
#' @param cap_max_tests integer - max number of daily tests. Only use if testing != "none"
#'
#' @importFrom purrr safely
#' @importFrom dplyr bind_rows mutate
#' @return data.frame of weekly cases, isolations, quarantines and tests for each simulation
#' @export
#'
#' @examples
#' \dontrun{
#' res <- scenario_sim(n.sim = 5,
#' net = haslemere,
#' prop.ascertain = 0.8,
#' cap_max_days = 69,
#' R = 6.5,
#' presymrate = 0.4,
#' delay_shape = 1,
#' delay_scale = 1.4,
#' num.initial.cases = 1,
#' prop.asym = 0.4,
#' scenario = "nothing",
#' outside = 0.001,
#' sensitivity = "high",
#' testing = "none")
#' }
#'

scenario_sim <- function(n.sim = 1, net, df, prop.ascertain, cap_max_days, R, presymrate,
                         delay_shape, delay_scale, num.initial.cases,asym.adult,
                         asym.child,asym.adult.inf,sym.child.inf, asym.child.inf, scenario,
                         outside, sensitivity = "high", testing = "none", output = "daily", cap_max_tests = NULL) {


  # Check input parameters --------------------------------------------------

  if(!sensitivity %in% c("high","low")) stop("sensitivity needs to be 'high' or 'low'")
  if(!testing %in% c("realistic","random", "none")) stop("testing needs to be 'realistic', 'random' or 'none'")
  if(floor(prop.ascertain) != 0) stop("prop.ascertain must between 0 and 1")
  if(floor(presymrate) != 0) stop("presymrate must between 0 and 1")
  if(floor(outside) != 0) stop("outside must between 0 and 1")


  # Set up scenarios --------------------------------------------------------

  if(scenario == "nothing") {
    isolation <- FALSE
    tracing <- FALSE
    quarantine <- FALSE
    secondary <- FALSE
  } else {
    if(scenario == "isolation") {
      isolation <- TRUE
      tracing <- FALSE
      quarantine <- FALSE
      secondary <- FALSE
    } else {
      if(scenario == "primary_quarantine") {
        isolation <- TRUE
        tracing <- TRUE
        quarantine <- TRUE
        secondary <- FALSE
      } else {
        if(scenario == "secondary_quarantine") {
          isolation <- TRUE
          tracing <- TRUE
          quarantine <- TRUE
          secondary <- TRUE
        } else {
          stop('Scenario must be either "nothing","isolation","primary_quarantine", or "secondary_quarantine"')
        }
      }
    }
  }



  # Run n.sim number of model runs and put them all together in a big data.frame
  res <- purrr::map(.x = 1:n.sim, ~ outbreak_model(num.initial.cases = num.initial.cases,
                                                   net = net,
                                                   df = df,
                                                   prop.ascertain = prop.ascertain,
                                                   cap_max_days = cap_max_days,
                                                   delay_shape = delay_shape,
                                                   delay_scale = delay_scale,
                                                   R = R,
                                                   presymrate = presymrate,
                                                   asym.adult = asym.adult,
                                                   asym.child = asym.child,
                                                   asym.adult.inf = asym.adult.inf,
                                                   sym.child.inf = sym.child.inf,
                                                   asym.child.inf = asym.child.inf,
                                                   quarantine = quarantine,
                                                   secondary = secondary,
                                                   tracing = tracing,
                                                   isolation = isolation,
                                                   outside = outside,
                                                   sensitivity = sensitivity,
                                                   testing = testing,
                                                   output = output,
                                                   cap_max_tests = cap_max_tests))


  # bind output together and add simulation index
  if(output == "weekly") {
    res <- dplyr::bind_rows(res) %>%
    dplyr::mutate(sim = rep(1:n.sim, rep(floor(cap_max_days / 7) + 1, n.sim)))
  } else {
    if(output == "daily")
    {
      res <- dplyr::bind_rows(res) %>%
        dplyr::mutate(sim = rep(1:n.sim, each = cap_max_days))
    }
  }

  return(res)
}
