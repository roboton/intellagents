
<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- badges: start -->
<!-- badges: end -->

[repository](https://github.com/roboton/intellagents)
[website](https://roboton.github.io/intellagents/)
[demo](https://roboton.github.io/intellagents/intellagents_demo.html)

# Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("roboton/intellagents")
```

# Background

There are many applied scenarios where a set of agents take on an
incoming set of cases and apply their discretion to the case to take or
recommend a course of action. While there usually is a set of rules
governing this discretion, it is often far from an objective,
deterministic process.

The goal of the policymaker in this setting is to give more cases to
agents who perform better on a set of pre-defined outcomes of interest
to the policymaker. However, the policymaker does not know which agents
perform better than others so needs to balance exploration (learning
agent performance) vs exploitation (giving cases to the best performing
agent). In reinforcement learning, this is known as the [multi-armed
bandit](https://en.wikipedia.org/wiki/Multi-armed_bandit).

This balance between exploration and exploitation can be described as
the difference between a randomized trial vs an uninformed prior. In a
randomized trial, we maximize exploration so that each agent in this
case gets an equal number of cases. This would maximize power to
differentiate performance between one agent versus another. This gives
us the most information for which agents are best but, in the process,
we’ve allocated a considerable amount of the cases to poorly performing
agents which comes at a loss to welfare. If we wanted to focus on
exploitation over exploration, we would simply form beliefs around agent
performance and assign cases to the best performing agents. This is
likely the most common in the real world and is heavily dependent on the
policymakers’ ability to adhere to the relevant outcomes and form
empirically-valid beliefs around agent performance.

One solution to the multi-armed bandit problem is probability matching
(aka Thompson Sampling or Bayesian Bandits) where the selection of agent
should be in proportion to the probability that this is the best agent
to select and is the approach taken in this notebook. In a real-world
setting with actual human agents we take into account a number of
considerations that depart from your usual multi-armed bandit
probability matching set up:

-   An agent can only handle so many cases in a certain time period.
-   An agent may change in performance over time.
-   Outcomes can be multidimensional with different importance weights
    assigned to different outcomes.
-   Agents need to be compensated for being assigned more cases (reward
    per case assigned).
-   Agents may vary in their performance across different types of cases
    or contexts of the case.

Since each arm in this case is a human agent, we will refer to this
setting as a multi-agent bandit. The solutions provided below addresses
the first four considerations above while the fifth is work-in-progress.

# Walkthrough

The two libraries above define a probability matching solution to the
multi-agent bandit problem given the aforementioned considerations.
Below we set a number of parameters to adjust the problem to these
considerations:

``` r
# load the intellagents library
library(intellagents)
#> 
#> Attaching package: 'intellagents'
#> The following object is masked from 'package:stats':
#> 
#>     simulate
library(tidyverse)
#> ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.0 ──
#> ✓ ggplot2 3.3.4     ✓ purrr   0.3.4
#> ✓ tibble  3.1.0     ✓ dplyr   1.0.5
#> ✓ tidyr   1.1.3     ✓ stringr 1.4.0
#> ✓ readr   1.4.0     ✓ forcats 0.5.1
#> ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
#> x dplyr::filter() masks stats::filter()
#> x dplyr::lag()    masks stats::lag()
# set a seed for the random number generator for consistent results
rng_seed <- 143
set.seed(rng_seed)

## agent 

# how long to look back in forming performance beliefs for an agent
memory <- as.difftime(30, units = "days")

# time period to consider for a maximum number of assigned cases
max_period <- as.difftime(1, units = "days")

# number of cases an agent can handle per max_period
max_cases <- 5

# number of total agents assigned to the caseload
num_agents <- 5

## cases
# number of cases in the caseload (simulation)
num_cases <- 8000

# total time period of the cases
time_period <- as.difftime(365 * 3, units = "days")

## outcomes
# names of relevant outcomes
outcomes <- c("referrals", "no_adverse_events")

# weights on above outcomes for scoring
outcome_weights <- c(1, 2)
```

## Generating agents

Next we generate a set of agents with associated (randomly generated)
performances:

``` r
agents <- generate_agents(num_agents = num_agents, outcomes = outcomes)
agent_scores <- score_agents(agents = agents, outcome_weights = outcome_weights)
max_agent <- get_max_agent(agents = agents, outcome_weights = outcome_weights)

as_tibble(sapply(agents, cbind)) %>%
  mutate(outcome = outcomes) %>%
  select(outcome, everything()) %>%
  bind_rows(data.frame(agent_scores) %>% rownames_to_column("agent_id") %>%
              pivot_wider(names_from = agent_id, values_from = agent_scores) %>%
              mutate(outcome = "score"))
#> # A tibble: 3 x 6
#>   outcome           `1fd9a` `5fa12` b45fc d139b eb604
#>   <chr>               <dbl>   <dbl> <dbl> <dbl> <dbl>
#> 1 referrals           0.409   0.312 0.264 0.667 0.723
#> 2 no_adverse_events   0.574   0.337 0.344 0.727 0.744
#> 3 score               1.56    0.985 0.951 2.12  2.21
```

There are 5 agents with their corresponding performance for each outcome
and the weighted score. In this case eb604 is the best performing agent.

This is not known to the policymaker (and likely the agents themselves)
and is learned through the simulation.

## Generating cases

We generate a set of cases that these agents will be assigned with
additional case and subject characteristics features to address the
future capability to have performance vary by these characteristics.

``` r
cases <- generate_cases(num_cases, time_period = time_period)
cases %>% mutate(across(where(is.character), as.factor)) %>% summary()
#>    date_time                      case_id       subject_id   case_char
#>  Min.   :2018-06-22 18:48:49   02ad7  :   2   1032a  :   7   A:1577   
#>  1st Qu.:2019-03-19 22:09:26   0b6f9  :   2   246ec  :   6   B:1612   
#>  Median :2019-12-11 06:09:00   0e29f  :   2   96feb  :   6   C:1582   
#>  Mean   :2019-12-16 22:54:00   21da3  :   2   db3ca  :   6   D:1616   
#>  3rd Qu.:2020-09-20 08:14:18   258b3  :   2   dfac0  :   6   E:1613   
#>  Max.   :2021-06-21 14:59:54   2b39e  :   2   0621b  :   5            
#>                                (Other):7988   (Other):7964            
#>  subject_char
#>  F:1705      
#>  G:1524      
#>  H:1545      
#>  I:1638      
#>  J:1588      
#>              
#> 
```

## Simulation

We then run the simulation across three different scenarios: (1)
`thompson` reflecting our probability matching strategy, (2) `random`
which randomly assigns the cases to each agent (randomized trial), and
(3) `oracle` which knows each agents’ performance and simply chooses the
best one, all within the maximum caseload constraints.

``` r
# TODO: Consider multiple, parallel simulations
system.time({
  simulated_cases <- simulate(cases = cases,
                              agents = agents,
                              outcomes = outcomes,
                              outcome_weights = outcome_weights,
                              memory = memory,
                              max_cases = max_cases,
                              max_period = max_period,
                              methods = c("thompson", "random", "oracle"))
})
#>    user  system elapsed 
#>  69.026   0.155  69.261
```

## Results

Below is a table summarizing the number of times each agent is selected
in each of the three simulation scenarios and the number of outcomes
that are realized. The `thompson` and `oracle` scenario are selecting
optimally for more of each outcome so naturally allocate more selections
towards higher scoring agents, resulting in improved outcomes compared
to the `random` scenario. The `oracle` scenario is meant to simulate the
best possible case assignment one could achieve so if either other the
other scenarios are close, it is considered optimal (on average).

``` r
simulated_cases %>% group_by(method, agent_id) %>%
  summarise(across(c(referrals, no_adverse_events), sum), selected = n(),
            .groups = "drop") %>%
  bind_rows(
    simulated_cases %>% mutate(agent_id = "all") %>%
      group_by(method, agent_id) %>%
      summarise(across(c(referrals, no_adverse_events), sum), selected = n(),
                .groups = "drop"))
#> # A tibble: 18 x 5
#>    method   agent_id referrals no_adverse_events selected
#>    <chr>    <chr>        <int>             <int>    <int>
#>  1 oracle   1fd9a          289               384      681
#>  2 oracle   5fa12           12                13       41
#>  3 oracle   b45fc            0                 1        3
#>  4 oracle   d139b         1887              2047     2809
#>  5 oracle   eb604         3261              3294     4466
#>  6 random   1fd9a          684               915     1622
#>  7 random   5fa12          484               529     1604
#>  8 random   b45fc          406               508     1567
#>  9 random   d139b         1031              1122     1570
#> 10 random   eb604         1208              1185     1637
#> 11 thompson 1fd9a          251               308      568
#> 12 thompson 5fa12           61                62      200
#> 13 thompson b45fc           68                86      236
#> 14 thompson d139b         2043              2245     3085
#> 15 thompson eb604         2802              2940     3911
#> 16 oracle   all           5449              5739     8000
#> 17 random   all           3813              4259     8000
#> 18 thompson all           5225              5641     8000
```

We can animate these results across time to see how they accumulate to
the table above:

``` r
time_periods <- 30 
period <- round(n_distinct(simulated_cases$date_time) / time_periods)
margin <- 0.03

agent_perf_scat <- plot_agent_perf(simulated_cases, type = "scatter",
                                   outcomes = outcomes, period = period,
                                   legend.title = element_blank())
agent_perf_dens <- plot_agent_perf(simulated_cases, type = "density",
                                   outcomes = outcomes, period = period,
                                   legend.title = element_blank())
count_plots <- plot_counts(simulated_cases, agent_scores = agent_scores,
                           outcomes = outcomes, period = period,
                           legend.title = element_blank())

plot_list <- list(agent_perf_scat, agent_perf_dens, count_plots)
all_plots <- plotly::subplot(plot_list, nrows = length(plot_list), margin = margin)

# save git the headache of checking in another large html file
if (!file.exists("intellagents_demo.html")) {
  htmlwidgets::saveWidget(all_plots, file = "intellagents_demo.html")
}
# plotly does not render in for github_document (.md) output target
# all_plots
```

Resulting visualization can be found
[here](https://roboton.github.io/intellagents/intellagents_demo.html)
with an explanation below:

The first row of plots plots the beliefs and 2x mean deviation from the
simulated estimates of each agents’ performance across the two outcomes.
As time passes and more cases are assigned to an agent, there is a more
confident estimate of the agents’ performance resulting in a tighter
deviation.

The second row of plots is similar but plots each outcome separately in
its own row with each column being one of the three scenarios. This is a
representation of our priors for each of the agents for each outcome as
it evolves over time. Only `thompson` using this prior to choose which
agent to assign next.

The third row of plots are the accumulated case assignments (left) and
outcomes (right), colored by the scenario, across time. Assigning more
cases to the better performing agents results in higher accumulated
counts of the desired outcomes.
