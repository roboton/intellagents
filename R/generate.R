# generate max_len character hex strings
generate_ids <- function(n, max_len = 5) {
  sapply(1:n, function(x) paste(sample(c(letters[1:6], 0:9), max_len),
                                collapse = ""))
}

# generate a case load
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

# generate agents
generate_agents <- function(num_agents, outcomes, num_periods = 1,
                            max_len = 5, min_val = 0.25, max_val = 0.75) {
  agent_ids <- generate_ids(n = num_agents)

  thetas <- sapply(agent_ids, function(i) {
    sapply(outcomes, function(j) { runif(num_periods, min_val, max_val) })},
    simplify = FALSE)

  if (num_periods == 1) {
    thetas <- sapply(thetas, function(x) { t(as.matrix(x)) }, simplify = FALSE)
  }
  return(thetas)
}
