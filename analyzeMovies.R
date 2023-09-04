# -----------------------------------------------------------------------------
# Movie Data Analysis
# Author: Chad M. Topaz
# Date: 2022
# -----------------------------------------------------------------------------

# --- Load necessary libraries ------------------------------------------------
library(tidyverse)
library(varhandle)
library(data.table)

# --- Load and preprocess film data -------------------------------------------
# Read film data from CSV
film <- read.csv("moviedata.csv")

# Select relevant columns from the data
film <- film %>%
  dplyr::select(year, rank, movie, tconst, theaters, gross, releasedate, name, nconst, category, gender, ethnicity)

# Convert gender to factor
film$gender <- as.factor(film$gender)

# Replace 'other' and 'middleeastern' ethnicities
film$ethnicity[film$ethnicity == "other"] <- NA
film$ethnicity[film$ethnicity == "middleeastern"] <- "middleeast"

# Convert ethnicity to factor and drop unused levels
film$ethnicity <- as.factor(film$ethnicity)
film$ethnicity <- droplevels(film$ethnicity)

# Identify rows with complete data for gender and ethnicity
completes <- film %>%
  dplyr::select(gender, ethnicity) %>%
  complete.cases

# Filter out films without complete data
film <- film %>%
  filter(!tconst %in% unique(film[!completes, "tconst"]))

# --- Helper function for calculating diversity metrics -----------------------
calculateMetrics <- function(thistconst){
  # Subset data based on tconst value
  data <- subset(film, tconst == thistconst, select = c(identityvars))
  
  # Calculate basic metrics
  N <- nrow(data)  # Total number of people
  C <- ncol(data)  # Total number of identity categories
  CI <- prod(sapply(subset(data, select = identityvars), nlevels))  # Total number of intersectional identities
  
  # Convert data to dummy variables
  I <- lapply(data[, 1:(ncol(data))], function(x) to.dummy(x, "X"))
  b <- t(list.cbind(I))
  
  # Compute shared identity score
  P <- t(b) %*% b
  S <- sum(P[upper.tri(P, diag = FALSE)]) / (choose(N, 2) * C)
  
  # Calculate intersecting diversity score
  ID <- 1 - sum(prop.table(table(data$gender, data$ethnicity))^2)
  ID <- ID * CI / (CI - 1)  # Standardize ID to [0,1] interval
  
  # Return metrics for current movie
  return(data.frame(tconst = thistconst, S = S, ID = ID))
}

# --- Analyze and score the movies based on diversity metrics -----------------
# Define identity variables for analysis
identityvars <- c("gender", "ethnicity")

# Calculate diversity scores for each movie
diversity <- rbindlist(lapply(unique(film$tconst), calculateMetrics))

# Merge diversity scores with the main dataset
diversity <- merge(diversity, film, by = "tconst", all.x = TRUE, all.y = FALSE)

# Select relevant columns and ensure uniqueness
diversity <- diversity %>%
  dplyr::select(tconst, movie, S, ID) %>%
  unique()
