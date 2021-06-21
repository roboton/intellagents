simulate <- function(cases, agents, outcomes = colnames(agents[[1]]),
                     outcome_weights = rep(1, length(outcomes)),
                     norm_type = "O",
                     methods = c("thompson", "random", "oracle"),
                     fixed_agent_list = NULL,
                     max_cases = 1,
                     max_period = as.difftime(1, units = "days"),
                     memory = as.difftime(365, units = "days")) {

  if ("fixed" %in% methods & is.null(fixed_agent_list)) {
    warning("Removing 'fixed'. No fixed_agent_list provided.")
    methods <- methods[methods != "fixed"]
  }

  # add outcomes to cases
  aug_cases <- sapply(methods, function(x) {
    cbind(cases, data.frame(method = rep(x, nrow(cases)),
                            agent_id = rep(NA, nrow(cases)),
                            as.list(sapply(outcomes, function(y) NA)),
                            agent_pool = NA)) },
    simplify = FALSE)
  for (method in methods) {
    for (i in 1:nrow(cases)) {
      # drop future cases
      case_history <- aug_cases[[method]][-c((i + 1):nrow(cases)),]
      # get current case
      cur_case <- aug_cases[[method]][i,]
      # filter down to cases within memory
      case_history <- case_history[case_history$date_time >=
                                     cur_case$date_time - memory,]
      # get agents exceeding caseload limit
      exclude_agent_ids <- names(which(table(case_history$agent_id[
        case_history$date_time >= cur_case$date_time - max_period]) >= max_cases))

      # dependent on method type
      if (method == "fixed") {
        cur_case$agent_id <- fixed_agent_list[i]
      } else {
        cur_case$agent_id <- draw_agents(case_history, method,
                                         agents, exclude_agent_ids,
                                         outcomes, outcome_weights, norm_type)
      }
      cur_case[,outcomes] <- draw_agent_outcomes(agents[[cur_case$agent_id]])
      cur_case$agent_pool <- list(I(list(setdiff(names(agents),
                                                 exclude_agent_ids))))
      aug_cases[[method]][i,] <- cur_case
    }
  }

  return(do.call(rbind, aug_cases))
}
