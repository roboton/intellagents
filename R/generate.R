#' Generate random identifiers
#'
#' @param n number of identifiers to generate
#' @param max_len max character length of each identifier
#'
#' @return character vector of identifiers
#' @export
#'
#' @examples
#' generate_ids(n = 10, max_len = 8)
generate_ids <- function(n, max_len = 5) {
  sapply(1:n, function(x) paste(sample(c(letters[1:6], 0:9), max_len),
                                collapse = ""))
}

#' Generate a case load
#'
#' @param num_cases number of cases to generate
#' @param time_period period of time that the cases will span
#' @param start_dt earliest case date time
#' @param end_dt latest case date time
#' @param max_len maximum character length of identifiers
#'
#' @return data frame with date_time, case_id, subject_id and characteristic columns
#' @export
#'
#' @examples
#' generate_cases(n = 100)
generate_cases <- function(num_cases,
                           time_period = as.difftime(365, units = "days"),
                           start_dt = Sys.time() - time_period,
                           end_dt = Sys.time(),
                           max_len = 5) {
  case_ids <- generate_ids(n = num_cases)
  case_chars <- data.frame(
    case_char = LETTERS[strtoi(case_ids, 16L) %% max_len + 1])
  subject_ids <- sample(generate_ids(n = num_cases), replace = TRUE)
  subject_chars <- data.frame(
    subject_char = LETTERS[strtoi(subject_ids, 16L) %% max_len + max_len + 1])
  date_times <- sort(sample(seq(Sys.time() - time_period,
                                Sys.time(), by = "sec"),
                           num_cases, replace = FALSE))
  return(cbind(data.frame(date_time = date_times, case_id = case_ids,
                          subject_id = subject_ids), case_chars, subject_chars))
}

#' Generate agents with known performance
#'
#' @param num_agents number of agents to generate
#' @param outcomes character vector of outcomes this agent produces
#' @param num_periods number of different periods of varyiing performance
#' @param min_val minimum performance value (expressed as a probability)
#' @param max_val maximum performance value (expressed as a probability)
#' @param max_len maximum character length of identifiers
#'
#' @return a matrix of performance scores for each agent/time period
#' @export
#' @importFrom stats runif
#'
#' @examples
#' generate_agents(num_agents = 10, outcomes = c("outcome1", "outcome2"))
generate_agents <- function(num_agents, outcomes, num_periods = 1,
                            min_val = 0.25, max_val = 0.75, max_len = 5) {
  agent_ids <- generate_ids(n = num_agents)

  thetas <- sapply(agent_ids, function(i) {
    sapply(outcomes, function(j) { runif(num_periods, min_val, max_val) })},
    simplify = FALSE)

  if (num_periods == 1) {
    thetas <- sapply(thetas, function(x) { t(as.matrix(x)) }, simplify = FALSE)
  }
  return(thetas)
}
