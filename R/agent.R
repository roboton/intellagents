#' Score a vector of outcomes
#'
#' @param outcomes numeric vector of outcomes to score
#' @param outcome_weights weights to apply to each outcome
#' @param norm_type type of norm. Same as _type_ in \link[base]{norm}
#'
#' @return a numeric score for the vector of outcomes
#' @export
#'
#' @examples
#' score_outcomes(c(3, 4))
score_outcomes <- function(outcomes, outcome_weights = rep(1, length(outcomes)),
                           norm_type = "O") {
  norm(as.matrix(outcomes * outcome_weights), type = norm_type)
}

#' Score a set of agents based on their known performance
#'
#' @param agents matrix of agents and outcome scores
#' @param outcome_weights weights to apply to each outcome
#' @param norm_type type of norm. Same as _type_ in \link[base]{norm}
#'
#' @return vector of numeric scores for each agent
#' @export
#'
#' @examples
#' score_agents(generate_agents(num_agents = 2, outcomes = c("o1", "o2")))
score_agents <- function(agents, outcome_weights = 1, norm_type = "O") {
  sapply(agents, function(agent) { apply(agent, 1, score_outcomes,
                                         outcome_weights = outcome_weights,
                                         norm_type = norm_type)})
}

#' Get the agent with the maximum score
#'
#' @param agents matrix of agents and outcome scores
#' @param outcome_weights weights to apply to each outcome
#' @param norm_type type of norm. Same as _type_ in \link[base]{norm}
#'
#' @return agent name (character) of agent with max score
#' @export
#'
#' @examples
#' get_max_agent(generate_agents(num_agents = 2, outcomes = c("o1", "o2")))
get_max_agent <- function(agents, outcome_weights = 1, norm_type = "O") {
  agent_scores <- score_agents(agents, outcome_weights, norm_type)
  if (is.matrix(agent_scores)) {
    return(names(agents)[apply(agent_scores, 1, which.max)])
  }
  return(names(agents)[which.max(agent_scores)])
}

#' Select an agent for a given history of outcomes
#'
#' @param case_history history of cases and outcomes
#' @param method method of drawing agent ("random", "oracle", or "thompson")
#' @param agents agent matrix
#' @param exclude_agent_ids which agent_ids to exclude from selection
#' @param outcomes character vector of outcomes to consider
#' @param outcome_weights weights to apply to each outcome
#' @param norm_type type of norm. Same as _type_ in \link[base]{norm}
#'
#' @return name of agent selected
#' @export
#' @importFrom stats aggregate rbeta
draw_agents <- function(case_history, method = "thompson",
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

#' Draw outcome for a given agent
#'
#' @param agent agent and their outcome scores
#' @param period agent time period to draw from
#'
#' @return vector of 0 or 1 indicating success or failure for each agent outcome
#' @export
#' @importFrom stats rbinom
#'
#' @examples
#' draw_agent_outcomes(generate_agents(num_agents = 1,
#'                                     outcomes = c("o1", "o2"))[[1]])
draw_agent_outcomes <- function(agent, period = 1) {
  sapply(agent[period,], function(x) rbinom(1, 1, x))
}
