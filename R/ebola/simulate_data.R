# Simulate a small ebola virus outbreak in North Eastern DRC using data on the 9th outbreak data

# Simulate transmission chains
# Simulate contact network

# Packages ---------------------------------------------------------------
library(ape)
library(epichains)
library(epicontacts)
library(tidyverse)


# source -----------------------------------------------------------------

source("R/set_paths.R")

# Paths ------------------------------------------------------------------
#get the MVH data from the outbreak
path_to_data <- "~/GitHub/EVD-COD9/data/clean/chains/mvh_epicontact.rds"

output <- here::here("data", "ebola")

# Import COD9 data -------------------------------------------------------

# import mvh epicontact (real transmssion tree)
mvh <- readRDS(path_to_data)

# Retrieve offspring distribution ----------------------------------------
# plot offspring distirbution to use in epichains
off <- epicontacts::get_degree(mvh, type = "out")

off_dist <- hist(
  off,
  breaks = seq(0, max(off) + 1, by = 1),
  col = "skyblue",
  border = "black",
  main = "Offspring distribution",
  xlab = "N offsprings",
  ylab = "Frequency"
)

#fit density to it
negbin <- MASS::fitdistr(off, densfun = "Negative Binomial")

# retrieve delay onset hospitalisation

delay_ons_out <- mvh$linelist |>
  select(
    onset_date,
    outcome_date
  ) |>
  mutate(delay_ons_out = as.numeric(outcome_date - onset_date))

#fit density to it
delay_ons_out_dist <- MASS::fitdistr(
  delay_ons_out$delay_ons_out,
  densfun = "normal"
)

# EVD generation time
# generation time mean of 14.4 (SD: 8.9) citing @Faye2015a, @WHOEbolaResponseTeam2014.

# Simulate chains --------------------------------------------------------
# using epichains package -
# the branching process is based on the offspring distribution defined above
# we can also provide a generation time function to estimate time of infection

generation_time_fn <- function(n) {
  evd_gamma <- epiparameter::convert_summary_stats_to_params(
    x = "gamma",
    mean = 14.4,
    sd = 8.9
  )

  gt <- rgamma(n, shape = evd_gamma$shape, scale = evd_gamma$scale)

  return(gt)
}

# function to simulate the outbreak, and return the longest chain
simulate_outbreak <- function(
  seed = 1,
  generation_time_fn,
  pop_size = 5000,
  n_chains = 500
) {
  set.seed(seed)

  sim_chains <- epichains::simulate_chains(
    pop = pop_size,
    n_chains = n_chains,
    statistic = "size",
    stat_threshold = 21,
    offspring_dist = rnbinom,
    mu = negbin$estimate[2],
    size = negbin$estimate[1],
    generation_time = generation_time_fn
  )

  # filter to keep the longest chain
  long_chain <- sim_chains[
    sim_chains$chain == which.max(unname(table(sim_chains$chain))),
  ]

  return(long_chain)
}

# simulate the outbreak
long_chain <- simulate_outbreak(
  seed = 1222,
  pop_size = 10000,
  n_chains = 500,
  generation_time_fn = generation_time_fn
)

# how long is the chain ?
nrow(long_chain)


# Prepare the linelist ---------------------------------------------------
# infection date are round up + made into Date
# onset are calculated using infection date + random draw from EVD incubation period

evd_incub <- epiparameter::convert_summary_stats_to_params(
  x = "gamma",
  mean = 9.1,
  sd = 7.3
)

# prepare linelist and tree
linelist <- long_chain |>
  as_tibble() |>
  select(id = infectee, t_inf = time) |>
  mutate(
    sex = sample(c("male", "female"), n(), replace = TRUE),
    age = sample(0:60, n(), replace = TRUE),
    t_inf = as.integer(t_inf),
    t_inf = if_else(
      t_inf == 0,
      min(mvh$linelist$onset_date),
      min(mvh$linelist$onset_date) + t_inf
    ),
    t_onset = t_inf +
      rgamma(1, shape = evd_incub$shape, scale = evd_incub$scale),
    t_outcome = t_onset +
      rnorm(
        1,
        mean = delay_ons_out_dist$estimate[1],
        sd = delay_ons_out_dist$estimate[2]
      ),
    classification = if_else(
      t_onset <= "2018-04-30",
      "suspected",
      sample(c("confirmed", "probable"), n(), replace = TRUE, prob = c(.4, .6))
    ),
    outcome = if_else(
      classification == "suspected",
      "dead",
      sample(c("dead", "recovered"), n(), replace = TRUE, prob = c(.5, .5))
    ),

    hospitalised = sample(
      c("yes", "no"),
      size = n(),
      replace = TRUE,
      prob = c(.8, .2)
    ),

    health_zone = sample(
      c("Bikoro", "Iboko", 'Ingende', "Wangata"),
      replace = TRUE,
      size = n(),
      prob = c(.4, .3, .2, .1)
    )
  ) |>
  as_tibble() |>
  relocate(c(health_zone, hospitalised), .after = age) |>
  relocate(outcome, .after = hospitalised)

# keep edges
edges <- long_chain |>
  as_tibble() |>
  select(from = infector, to = infectee, t_inf = time) |>
  mutate(
    branch_length = if_else(
      is.na(match(from, to)),
      0,
      t_inf - t_inf[match(from, to)]
    ),
    branch_length = round(branch_length, digits = 2)
  ) |>
  filter(!is.na(from))

# Make epicontact and plot
epi_chain <- make_epicontacts(
  linelist,
  edges,
  id = "id",
  from = "from",
  to = "to",
  directed = TRUE
)

# Plot epichains with temporal axis
epicontacts::vis_temporal_interactive(
  epi_chain,
  x_axis = "t_inf",
  network_shape = "rectangle"
)

# save epicontact
saveRDS(epi_chain, here::here(output, "simulated_epicontact.rds"))
saveRDS(epi_chain$linelist, here::here(output, "simulated_linelist.rds"))
saveRDS(epi_chain$contacts, here::here(output, "simulated_chain.rds"))

# Contact matrix from transmission tree ------------------------------------
# from this chains I can simulate a contact network using the function in outbreaker2

sim_ctd <- function(tTree, eps, lambda) {
  # make both as character
  tTree <- apply(tTree, 2, as.character)

  if (any(c(eps, lambda) < 0) | any(c(eps, lambda) > 1)) {
    stop('eps and lambda must be probabilities')
  }

  if (ncol(tTree) != 2) {
    stop("tTree must have two columns")
  }

  id <- unique(c(tTree[, 1], tTree[, 2]))
  id <- id[!is.na(id)]

  ## Sort tTree by value or alphabetically, This ensures A:B and B:A are both
  ## recognised when querying the contacts dataframe for transmission pairs
  tTree <- tTree %>%
    stats::na.omit() %>%
    apply(1, sort, decreasing = FALSE) %>%
    t() %>%
    as.data.frame(stringsAsFactors = FALSE)

  tTree <- tTree[order(tTree[, 1]), ]
  names(tTree) <- c('V1', 'V2')
  if (nrow(tTree) == 0) stop("No transmission observed")

  ## Create a dataframe of all potential contacts
  contacts <- as.data.frame(
    t(utils::combn(sort(id), 2)),
    stringsAsFactors = FALSE
  )

  ## Create a column of logicals indicating whether a pair represents a
  ## transmission pair or not
  tTree$tPair <- TRUE

  ## Mark non-transmission pairs in the contacts dataframe. The merge function
  ## will mark pairs found in contacts but not in tTree as 'NA'. These are
  ## then converted to FALSE
  contacts <- merge(contacts, tTree, by = c('V1', 'V2'), all.x = TRUE)
  contacts$tPair[is.na(contacts$tPair)] <- FALSE

  ## Sample a number of rows given by a binomial distribution
  sampler <- function(x, prob) {
    x[sample(1:nrow(x), stats::rbinom(1, nrow(x), prob)), 1:3]
  }

  ## Sample transmission pairs with probability eps
  ## Sample non-transmission pairs with probability eps*lambda
  ctd <- rbind(
    sampler(contacts[contacts$tPair, ], eps),
    sampler(contacts[!contacts$tPair, ], eps * lambda)
  )

  ctd <- ctd[, c(1, 2)]

  colnames(ctd) <- c('i', 'j')
  rownames(ctd) <- NULL
  return(ctd)
}

# get all contact, and make a matrix
ctd_df <- sim_ctd(edges[, c("from", "to")], eps = .9, lambda = .1)

ctd_df <- ctd_df |>
  as_tibble() |>
  rename("from" = i, "to" = j) |>
  arrange(from) |>
  mutate(
    type_contact = sample(
      c("household", "nosocomial", "funeral", "community"),
      n(),
      replace = TRUE
    )
  )

# Get all unique IDs
unique_ids <- sort(unique(c(ctd_df$from, ctd_df$to)))

# Create an empty adjacency matrix filled with FALSE
contact_matrix <- matrix(
  FALSE,
  nrow = length(unique_ids),
  ncol = length(unique_ids),
  dimnames = list(unique_ids, unique_ids)
)

# Fill the matrix with TRUE where there is a recorded contact
for (k in 1:nrow(ctd_df)) {
  contact_matrix[
    as.character(ctd_df$from[k]),
    as.character(ctd_df$to[k])
  ] <- TRUE
  contact_matrix[
    as.character(ctd_df$to[k]),
    as.character(ctd_df$from[k])
  ] <- TRUE # Ensure symmetry
}

diag(contact_matrix) <- FALSE

# remove all contact for 41 so we have one without contact
ctd_df <- ctd_df |>
  filter(
    to != "41",
    from != 41
  )

# save the contact data but not the matrix
saveRDS(ctd_df, here::here(output, "simulated_contacts.rds"))
