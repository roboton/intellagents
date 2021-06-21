# score multidimensional outcomes
score_outcomes <- function(outcomes, outcome_weights = rep(1, length(outcomes)),
                           norm_type = "I") {
  norm(as.matrix(outcomes * outcome_weights), type = norm_type)
}

# score a set of agents
score_agents <- function(agents, outcome_weights = 1, norm_type = "O") {
  sapply(agents, function(agent) { apply(agent, 1, score_outcomes,
                                         outcome_weights = outcome_weights,
                                         norm_type = norm_type)})
}

get_max_agent <- function(agents, outcome_weights = 1, norm_type = "O") {
  agent_scores <- score_agents(agents, outcome_weights, norm_type)
  if (is.matrix(agent_scores)) {
    return(names(agents)[apply(agent_scores, 1, which.max)])
  }
  return(names(agents)[which.max(agent_scores)])
}

draw_agents <- function(case_history, method = c("thompson"),
                        agents, exclude_agent_ids = NULL,
                        outcomes, outcome_weights = 1, norm_type = "O") {

  # get available agents
  available_agents <- agents[!names(agents) %in% exclude_agent_ids]

  if (length(available_agents) == 0) {
    available_agents <- agents
  }

  # thompson sampling
  if (method == "thompson") {
    agents_outcomes <- rbind(
      # this initializes the beta distribution with one success and failure
      cbind(data.frame(agent_id = names(agents),
                       sapply(outcomes,
                              function(x) c(rep(FALSE, length(agents)),
                                            rep(TRUE, length(agents)))))),
      case_history[,c("agent_id", outcomes)])

    agents_outcomes <- agents_outcomes[
      agents_outcomes$agent_id %in% names(available_agents),]

    draws <- aggregate(. ~ agent_id, data = agents_outcomes, FUN = function(x) {
      rbeta(1,
            sum(x == TRUE, na.rm = TRUE),
            sum(x == FALSE, na.rm = TRUE)) })
    return(draws$agent_id[which.max(apply(as.matrix(draws[,-1]),
                                          1, score_outcomes,
                                          outcome_weights = outcome_weights,
                                          norm_type = norm_type))])
  # random choice
  } else if (method == "random") {
    return(sample(names(available_agents), 1))
  # best (available) arm
  } else if (method == "oracle") {
    return(get_max_agent(available_agents, outcome_weights, norm_type))
  }
}

draw_agent_outcomes <- function(agent, period = 1) {
  sapply(agent[period,], function(x) rbinom(1, 1, x))
}
