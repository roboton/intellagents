#' Plot animated agent performance
#'
#' @param simulated_cases cases after simulation
#' @param type "density" or "scatter" plot
#' @param outcomes outcomes to plot
#' @param methods character vector of methods of simulation to plot
#' @param period animate every "period" cases
#' @param x_label label for x-axis
#' @param y_label label for y-axes
#' @param ... options passed to \link[ggplot2]{theme}
#'
#' @return animated plotly plot
#' @export
#' @import dplyr
#' @import ggplot2
#' @rawNamespace import(plotly, except = last_plot)
#' @import tidyr
#' @importFrom rlang .data
#' @importFrom stats dbeta
plot_agent_perf <- function(simulated_cases, type = "density",
                            outcomes = NULL, methods = NULL,
                            period = 10, x_label = NULL, y_label = NULL, ...) {

  if (is.null(outcomes)) {
    outcomes <- names(simulated_cases)[
      (which(names(simulated_cases) == "agent_id") + 1):ncol(simulated_cases)]
  }
  if (is.null(methods)) {
    methods <- unique(simulated_cases$method)
  }

  plot_dat <- simulated_cases %>%
    pivot_longer(all_of({{ outcomes }}), names_to = "outcome",
                 values_to = "success") %>%
    filter(.data$outcome %in% {{ outcomes }} &
             .data$method %in% {{ methods }}) %>%
    select(.data$date_time, .data$agent_id, .data$method, .data$outcome,
           .data$success) %>%
    mutate(success = as.logical(.data$success), fail = !.data$success) %>%
    pivot_wider(names_from = .data$agent_id,
                values_from = c(.data$success, .data$fail),
                values_fill = 0) %>%
    group_by(.data$method, .data$outcome) %>%
    mutate(across(-c(.data$date_time), cumsum)) %>%
    ungroup() %>%
    pivot_longer(-c(.data$date_time, .data$method, .data$outcome),
                 names_sep = "_", names_to = c(".value", "agent_id")) %>%
    arrange(.data$agent_id) %>%
    mutate(agent_id = as.factor(.data$agent_id)) %>%
    filter(round(as.numeric(.data$date_time) -
                   min(as.numeric(.data$date_time))) %%
             {{ period }} == 0) %>%
    mutate(date_time = as.character(.data$date_time))

  if (type == "density") {
    x_seq <- seq(0.01, 0.99, by = 0.01)
    plot_dat <- plot_dat %>%
      rowwise() %>%
      mutate(x = list({{ x_seq }}),
             y = list(dbeta({{ x_seq }}, .data$success, .data$fail))) %>%
      select(.data$date_time, .data$method, .data$outcome, .data$agent_id,
             .data$x, .data$y) %>%
      unnest(c(.data$x, .data$y))

    p <- plot_dat %>%
      ggplot(aes(.data$x, .data$y,
                 color = .data$agent_id, frame = .data$date_time)) +
      geom_line() +
      facet_grid(.data$outcome ~ .data$method, scales = "free_y")
  } else if (type == "scatter") {
    if (length(outcomes) != 2) {
      stop("Plot type 'scatter' only relevant to two-dimesional outcomes.")
    }
    outcome1_name <- outcomes[1]
    plot_dat <- plot_dat %>%
      mutate(mean = beta_mean(.data$success, .data$fail),
             var = beta_var(.data$success, .data$fail),
             dev = exp(lbeta_dev(.data$success, .data$fail))) %>%
      select(.data$date_time, .data$method, .data$agent_id, .data$outcome,
             .data$mean, .data$var, .data$dev) %>%
      mutate(outcome = if_else(.data$outcome == {{ outcome1_name }},
                               "outcome1", "outcome2")) %>%
      pivot_wider(names_from = .data$outcome,
                  values_from = c(.data$mean, .data$dev, .data$var)) %>%
      rowwise() %>%
      mutate(ellipse = list(get_ellipse_points(
        x0 = .data$mean_outcome1 , y0 = .data$mean_outcome2,
        a = .data$dev_outcome1 * 2, b = .data$dev_outcome2 * 2))) %>%
      unnest(.data$ellipse)
    p <- plot_dat %>%
      ggplot(aes(.data$x, .data$y, color = .data$agent_id,
                 frame = .data$date_time)) +
      geom_path() +
      facet_wrap(~ .data$method) +
      xlim(0.01, 0.99)
  } else {
    stop(paste("invalid plot type:", type))
  }

  # TODO: this is a hack to drop duplicated legend entries that are shared
  # between the first 'scatter' plot and the second 'density' plot.
  keep_axes <- "x"
  return(fix_legend(ggplotly(p + theme_minimal() + theme(...) +
                               xlab(x_label) + ylab(y_label)), keep_axes))
}

#' Plot animated agent assignment and outcomes
#'
#' @param simulated_cases cases after simulation
#' @param outcomes outcomes to plot
#' @param methods character vector of methods of simulation to plot
#' @param agent_scores vector of agents and their scores
#' @param period animate every "period" cases
#' @param x_label label for x-axis
#' @param y_label label for y-axes
#' @param color_palette to be used to differentiate colors
#' @param ... options passed to \link[ggplot2]{theme}
#'
#' @return animated plotly plot
#' @export
#' @import dplyr
#' @importFrom stringr str_glue
#' @import ggplot2
#' @rawNamespace import(plotly, except = last_plot)
#' @import tidyr
plot_counts <- function(simulated_cases, outcomes = NULL, methods = NULL,
                        agent_scores = NULL, period = 10,
                        x_label = NULL, y_label = NULL,
                        color_palette = "Dark2", ...) {
  # outcomes
  if (is.null(outcomes)) {
    outcomes <- names(simulated_cases)[
      (which(names(simulated_cases) == "agent_id") + 1):ncol(simulated_cases)]
  }
  if (is.null(methods)) {
    methods <- unique(simulated_cases$method)
  }
  # agents
  if (!is.null(agent_scores)) {
    simulated_cases <- simulated_cases %>%
      left_join(tibble(agent_id = names({{ agent_scores }}),
                       score = str_glue("({round(agent_scores, 2)})")),
                                        by = "agent_id") %>%
      unite("agent_id", .data$agent_id, .data$score, sep = " ")
  }

  count_dat <- bind_rows(
    simulated_cases %>%
      select(-.data$agent_pool) %>%
      select(.data$date_time, .data$method, all_of({{ outcomes }})) %>%
      group_by(.data$method) %>%
      arrange(.data$date_time) %>%
      mutate(across(all_of({{ outcomes }}), cumsum)) %>%
      pivot_longer(-c(.data$method, .data$date_time), names_to = "name",
                   values_to = "count") %>%
      mutate(type = "outcomes"),
    simulated_cases %>%
      select(-.data$agent_pool) %>%
      select(.data$method, .data$date_time, .data$agent_id) %>%
      mutate(count = 1) %>%
      pivot_wider(names_from = .data$agent_id, values_from = .data$count,
                  values_fill = 0) %>%
      group_by(.data$method) %>%
      arrange(.data$date_time) %>%
      mutate(across(-.data$date_time, cumsum)) %>%
      pivot_longer(-c(.data$date_time, .data$method), names_to = "name",
                   values_to = "count") %>%
      mutate(type = "agents")) %>%
    filter(round(as.numeric(.data$date_time) -
                   min(as.numeric(.data$date_time))) %% {{ period }} == 0) %>%
    mutate(date_time = as.character(.data$date_time))

  p <- count_dat %>%
    ggplot(aes(.data$name, .data$count, color = .data$method,
               fill = .data$method, frame = .data$date_time)) +
    geom_col(position = "identity", alpha = 0.2) +
    facet_wrap(~ type, scales = "free_x") +
    xlab(x_label) + ylab(y_label) +
    theme_minimal() +
    scale_color_brewer(palette = color_palette) +
    scale_fill_brewer(palette = color_palette) +
    theme(...)

  return(fix_bar_dodge(p, length(methods)))
}

# helper functions below
fix_legend <- function(gp, keep_axes = c("x")) {
  for (i in seq_along(gp$x$data)) {
    if (!gp$x$data[[i]]$xaxis %in% keep_axes) {
      gp$x$data[[i]]$showlegend <- FALSE
    }
  }
  return(gp)
}

fix_bar_dodge <- function(p, num_methods) {
  gp <- ggplotly(p) %>% layout(barmode = "dodge")
  for (i in seq_along(gp$x$data)) {
    gp$x$data[[i]]$width <- 1 / (num_methods + 1)
  }
  return(fix_legend(gp))
}

get_ellipse_points <- function(x0 = 0, y0 = 0, a = 1, b = 1, res = 0.05) {
  tibble(x = a * cos(seq(0, 2*pi, by = res)) + x0,
         y = b * sin(seq(0, 2*pi, by = res)) + y0)
}

beta_mean <- function(a, b) {
  return(a / (a + b))
}

beta_var <- function(a, b) {
  return((a * b) / ((a + b)^2 + (a + b + 1)))
}

beta_dev <- function(a, b) {
  return((2 * (a ^ a) * (b ^ b)) / (beta(a, b) * (a + b)^(a + b + 1)))
}

lbeta_dev <- function(a, b) {
  return(log(2) + a * log(a) + b * log(b) - lbeta(a, b) -
           ((a + b + 1) * log(a + b)))
}
