#' Run a single instance of the branching process model
#' @author Joel Hellewell
#' @author Lewis Spurgin
#' @inheritParams outbreak_step
#' @param num.initial.cases integer number of initial cases
#' @param delay_shape numeric shape parameter of delay distribution
#' @param delay_scale numeric scale parameter of delay distribution
#' @param weekly logical - return summarised weekly cases or the raw case data
#' @param s seed - optional for reproducing the same output. Useful for `plot_network()`
#'
#' @return data.frame of cases by week, cumulative cases, and weekly isolations, quarantines and tests
#' @export
#'
#' @importFrom magrittr %>% %<>%
#' @importFrom dplyr filter mutate group_by summarise arrange
#' @importFrom tibble tibble
#' @importFrom tidyr replace_na
#'
#' @examples
#'
#' \dontrun{
#' outbreak_model(net = haslemere, num.initial.cases = 1, prop.ascertain = 0.8, cap_max_days = 69, R = 6.5, presymrate = 0.4,
#' delay_shape = 1,delay_scale = 1.4,prop.asym = 0.4, quarantine = TRUE, isolation = TRUE, tracing = TRUE, secondary = TRUE,
#' outside = 0.001, sensitivity = "high", testing = "none"}
#'
#'
outbreak_model <- function(net,
                           df,
                           num.initial.cases,
                           prop.ascertain,
                           cap_max_days,
                           R , presymrate, delay_shape,
                           delay_scale,
                           asym.adult,
                           asym.child,
                           asym.adult.inf,
                           sym.child.inf,
                           asym.child.inf,
                           quarantine, isolation,
                           tracing, secondary,
                           outside, sensitivity = "high",
                           testing = "none", cap_max_tests = NULL,
                           output = "weekly", s = NULL) {

  # Set up functions to sample from distributions
  # incubation period sampling function
  incfn <- dist_setup(dist_shape = 2.322737,
                      dist_scale = 6.492272)
  # incfn <- dist_setup(dist_shape = 3.303525,dist_scale = 6.68849) # incubation function for ECDC run
  # onset to isolation delay sampling function
  delayfn <- dist_setup(delay_shape,
                        delay_scale)


  # Set initial values for loop indices
  total.cases <- num.initial.cases
  latest.onset <- 0
  extinct <- FALSE
  popsize <- nrow(net)
  cday <- 1
  daily_isolated <- 0 #none isolated on day 0
  daily_quarantined <- 0
  daily_tests <- 0




  # Initial setup
  if(exists("s")){set.seed(s)}
  case_data <- outbreak_setup(net = net,
                              df = df,
                              num.initial.cases = num.initial.cases,
                              incfn = incfn,
                              asym.adult = asym.adult,
                              asym.child = asym.child,
                              delayfn = delayfn,
                              isolation = isolation)



  # Model loop
  if(exists("s")){set.seed(s)}
  while (cday < cap_max_days & total.cases < popsize & !extinct) {
    case_data <- outbreak_step(case_data = case_data,
                               day = cday,
                               net = net,
                               incfn = incfn,
                               delayfn = delayfn,
                               prop.ascertain = prop.ascertain,
                               R = R,
                               presymrate = presymrate,
                               quarantine = quarantine,
                               isolation = isolation,
                               tracing = tracing,
                               secondary = secondary,
                               asym.child = asym.child,
                               asym.adult = asym.adult,
                               asym.adult.inf = asym.adult.inf,
                               sym.child.inf = sym.child.inf,
                               asym.child.inf = asym.child.inf,
                               outside = outside,
                               sensitivity = sensitivity,
                               testing = testing,
                               cap_max_tests = cap_max_tests)


    total.cases <- sum(!is.na(case_data$exposure))
    extinct <- all(case_data$isolated,na.rm = TRUE)
    daily_isolated <- c(daily_isolated,sum(case_data$isolated))
    daily_quarantined <- c(daily_quarantined,sum(case_data$quarantined))
    daily_tests <- c(daily_tests,sum(floor(case_data$test_time) == cday))
    cday <- cday + 1

  }

  daily_cases <- tibble(
    day = c(1:cap_max_days)
  ) %>%
    left_join(case_data %>%
                filter(!is.na(exposure)) %>%
                mutate(day = floor(exposure)) %>%
                group_by(day) %>%
                summarise(daily_infections = n()),
              by = "day") %>%
    left_join(case_data %>%
                filter(!asym,
                       !is.na(onset)) %>%
                mutate(day = floor(onset)) %>%
                group_by(day) %>%
                summarise(daily_cases = n()),
              by = "day") %>%
    left_join(case_data %>%
                filter(isolated_time < Inf) %>%
                mutate(day = floor(isolated_time)) %>%
                group_by(day) %>%
                summarise(daily_isolations = n()),
              by = "day") %>%
    filter(day %in% c(1:cap_max_days)) %>%
    mutate(daily_quarantined = daily_quarantined,
           daily_tests = daily_tests) %>%
    replace(is.na(.), 0) %>%
    mutate(cuminf = cumsum(daily_infections),
           cumcases = cumsum(daily_cases)) %>%
    filter(day %in% c(1:cap_max_days))

  # Prepare output, group into weeks
  weekly_isolation <- c()
  weekly_quarantine <- c()
  weekly_tests <- c()

  for(i in seq(1,cap_max_days+1,7)){
    weekly_isolation <- c(weekly_isolation,
                          mean(daily_isolated[i:(i+6)],na.rm = TRUE))

    weekly_quarantine <- c(weekly_quarantine,
                           mean(daily_quarantined[i:(i+6)],na.rm = TRUE))

    weekly_tests <- c(weekly_tests,
                      sum(daily_tests[i:(i+6)],na.rm = TRUE))
  }

  weekly_cases <- tibble(week = unique(floor((1:cap_max_days)/7)),
                         weekly_isolation,
                         weekly_tests,
                         weekly_quarantine) %>%
    left_join(case_data %>%
                dplyr::mutate(week = floor(onset/7)) %>%
                dplyr::group_by(week) %>%
                dplyr::summarise(weekly_cases = n()),
              by = "week") %>%
    mutate(weekly_cases = tidyr::replace_na(weekly_cases,0),
           weekly_isolation = tidyr::replace_na(weekly_isolation,0))


  # order and sum up
  weekly_cases %<>%
    dplyr::arrange(week) %>%
    dplyr::mutate(cumcases = cumsum(weekly_cases),
                  cumiso = cumsum(weekly_isolation))

  # return
  if(output == "weekly") {
    return(weekly_cases)
  } else {
    if(output == "raw")
    {
      return(case_data)
    } else{
      if(output == "daily"){
        return(daily_cases)
      } else{
        stop("output must be 'daily', 'weekly' or 'raw'")
      }
    }
  }

}
