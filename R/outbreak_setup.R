#' Set up initial cases for network model
#' @author Lewis Spurgin
#' @author Joel Hellewell
#'
#' @param net network in pairwise list format. Object `haslemere` is built into the package.
#' @param num.initial.cases integer number of initial cases
#' @param incfn function that samples from incubation period Weibull distribution; generated using dist_setup
#' @param delayfn function that samples from the onset-to-hospitalisation delay Weibull distribution; generated using dist_setup
#' @param prop.asym numeric proportion of cases that are sublinical (between 0 and 1)
#' @param isolation logical - do you want indiviuals to self-isolate on infection?
#'
#' @return data.frame of cases in outbreak so far
#' @export
#' @importFrom tibble tibble
#'
#' @examples
#'
#'\dontrun{
#' # incubation period sampling function
#' incfn <- dist_setup(dist_shape = 2.322737,dist_scale = 6.492272)
#' # delay distribution sampling function
#' delayfn <- dist_setup(dist_shape = 1,dist_scale = 1.4)
#' outbreak_setup(net, num.initial.cases = 5,incfn,delayfn,prop.asym=0, isolation = TRUE)
#'}
#'
#'
outbreak_setup <- function(net, df, num.initial.cases, incfn, delayfn, asym.child, asym.adult, isolation) {


# DEBUGGING ---------------------------------------------------------------

# net <- am.sch
# df <- school_data
# num.initial.cases <- 5
# incfn <- dist_setup(dist_shape = 2.322737,dist_scale = 6.492272)
# delayfn <- dist_setup(dist_shape = 1,dist_scale = 1.4)
# asym.child <- 0.8
# asym.adult <- 0.4
# isolation <- TRUE


  # Set up table of population
  popsize <- length(unique(c(net$Var1,net$Var2)))
  case_data <- tibble(exposure = NA, # Exposure time of 0 for all initial cases
                      asym = NA,
                      caseid = df$id, # set case id
                      age = df$age,
                      infector = NA,
                      onset = NA,
                      isolated_time = Inf,
                      quarantine_time = Inf,
                      test_time = Inf,
                      release_time = NA,
                      recovery_time = NA,
                      status = "S",
                      isolated = FALSE,
                      quarantined = FALSE)

  case_data$asym[case_data$age == "adult"] <-
    rbernoulli(n = length(case_data$asym[case_data$age == "adult"]),
               p = asym.adult)

  case_data$asym[case_data$age == "child"] <-
    rbernoulli(n = length(case_data$asym[case_data$age == "child"]),
               p = asym.child)

  # Set up initial cases
  initial_cases <- sample(1:popsize,num.initial.cases)
  case_data$exposure[initial_cases] <- 0
  case_data$onset[initial_cases] <- incfn(num.initial.cases)
  case_data$recovery_time[initial_cases] <- case_data$onset[initial_cases] + 7
  case_data$status[initial_cases] <- "I"

  if(isolation){
    #Isolation times for symptomatic cases: onset + delay
    sym_cases <- initial_cases[!case_data$asym[initial_cases]]
    case_data$isolated_time[sym_cases] <- case_data$onset[sym_cases] +
      delayfn(length(sym_cases))
    case_data$release_time[sym_cases] <- case_data$isolated_time[sym_cases] + 100
  }


  # return
  return(case_data)
}
