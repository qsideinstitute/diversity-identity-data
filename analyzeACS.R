# -----------------------------------------------------------------------------
# Diversity and Economic Analysis of U.S. States based on the ACS dataset
# Author: Chad M. Topaz
# Date: 2022
# -----------------------------------------------------------------------------

# --- Load necessary libraries ------------------------------------------------
library(tidyverse)
library(rvest)
library(data.table)
library(pbmcapply)

# --- Load the ACS dataset ---------------------------------------------------
load("acsdata.Rdata")

# --- Define identity variables for analysis ----------------------------------
identityvars <- c("sex","age","marital","race","disability")

# --- Define helper functions -------------------------------------------------

# Calculate diversity metrics for a given state
calculateMetrics <- function(thisstate, weightcol) {
  data <- subset(acs, state == thisstate, select = c(identityvars, weightcol))
  data <- data %>% group_by_at(vars(one_of(identityvars))) %>% summarise(weight = sum(.data[[weightcol]]), .groups = "drop")
  completes <- complete.cases(data)
  discarded <- sum(data[!completes,]$weight) / sum(data$weight)
  data <- data[completes,]
  N <- sum(data$weight)
  C <- ncol(data) - 1
  CI <- prod(sapply(subset(data, select = identityvars), nlevels))
  I <- lapply(data[, 1:(ncol(data) - 1)], function(x) to.dummy(x, "X"))
  b <- t(list.cbind(I))
  P <- t(b) %*% b
  weightmatrix <- outer(data$weight, data$weight, '*')
  P <- P * weightmatrix
  S <- (sum(P[upper.tri(P, diag = TRUE)]) - sum(data$weight)) / (choose(N, 2) * C)
  ID <- (1 - sum(prop.table(data$weight)^2))
  ID <- ID * CI / (CI - 1)
  return(data.frame(state = thisstate, S = S, ID = ID))
}

# Check state diversity against baseline state
checkstate <- function(basestate, data, whichvar) {
  tmp <- subset(data, state == basestate)
  Sbase <- tmp$S
  IDbase <- tmp$ID
  rankbase <- tmp[[whichvar]]
  comps <- subset(data, S > Sbase & ID > IDbase)
  check <- TRUE
  good <- 0
  bad <- 0
  pct <- NA
  if (nrow(comps) > 0) {
    good <- sum(comps[[whichvar]] < rankbase)
    tot <- nrow(comps)
    bad <- tot - good
    pct <- good / tot
    check <- pct == 1
  }
  return(data.frame(good = good, bad = bad, pct = pct, check = check))
}

# Score the diversity of the data
score <- function(data) {
  tmp <- rbindlist(lapply(data$state, checkstate, data, "gdprank"))
  return(sum(tmp$good) / sum(tmp$good + tmp$bad))
}

# Randomly shuffle the GDP rankings for simulation
shuffle <- function(data) {
  data$gdprank <- sample(data$gdprank)
  return(data)
}

# --- Data Acquisition and Processing -----------------------------------------

# Scrape GDP per capita data for US states from Wikipedia
url <- "https://en.wikipedia.org/wiki/List_of_U.S._states_and_territories_by_GDP_per_capita"
tabs <- read_html(url) %>% html_table()
gdp <- as.data.frame(tabs[[1]])
gdp <- gdp[3:53, c(1,8)]
names(gdp) <- c("state","gdp")
gdp <- gdp %>%
  mutate(gdp = str_replace_all(gdp,"\\,|\\$","")) %>%
  mutate(gdp = as.numeric(gdp)) %>%
  arrange(desc(gdp)) %>%
  mutate(state = str_replace_all(state, "\\*|","")) %>%
  mutate(state = str_squish(state)) %>%
  dplyr::select(-gdp) %>%
  mutate(gdprank = row_number())
names(gdp) <- c("state","gdprank")
gdp$state <- tolower(gdp$state)

# Calculate diversity metrics for each state
diversity <- lapply(levels(acs$state), calculateMetrics, "weight")
diversity <- rbindlist(diversity)

# Merge diversity data with GDP rankings
diversity <- left_join(diversity, gdp)
diversity$gdprank <- as.numeric(diversity$gdprank)

# --- Simulation and Analysis -------------------------------------------------

# Simulate random shuffling to estimate chance performance
sims <- 1:10000
scores <- pbmclapply(sims, function(x) score(shuffle(diversity)), mc.cores = detectCores(logical = TRUE) - 1)
scores <- data.frame(score = unlist(scores))

# Score using alternative weightings for sensitivity analysis
scorealt <- function(weightcol) {
  diversity <- lapply(levels(acs$state), calculateMetrics, weightcol)
  diversity <- rbindlist(diversity)
  diversity <- merge(diversity, gdp)
  return(score(diversity))
}
altscores <- pbmclapply(paste0("altweight", 1:80), scorealt, mc.cores = detectCores(logical = TRUE) - 1)
altscores <- data.frame(score = unlist(altscores))