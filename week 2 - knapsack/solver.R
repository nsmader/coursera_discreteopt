# Code for solving knapsack problems
# Author: Nick Mader
# About: This is to solve homeworks assigned in the Coursera course on Discrete Optimization.
#        Input and output funtions are designed to match with established/required formats.

setwd("~/GitHub/coursera_discreteopt/week 2 - knapsack/")
library(dplyr)
library(tidyr)
library(data.table)

input <- "data/ks_100_0"

### Functions to read input, initialize forms, score, and generate output ------

read_knap <- function(input){
  d <- readLines(input)
  meta <- d[1] %>% strsplit(" ")
  items <-
    d[-1] %>%
    data.frame() %>%
    setnames(".", "x") %>%
    separate(x, into = c("val", "wt")) %>%
    mutate(i = row_number(),
           val = as.numeric(val),
           wt  = as.numeric(wt))
  list(n = as.numeric(meta[[1]][1]), capacity = as.numeric(meta[[1]][2]), items = items)
}

init_solution <- function(knap){
  rep(0, nrow(knap$items))
}

taken <- init_solution(knap)

solution_val <- function(knap, taken){
  taken %*% knap$items$val
}

solution_wt <- function(knap, taken){
  taken %*% knap$items$wt
}

solution_out <- function(knap, taken){
  out <- 
    paste(paste(solution_val(knap, taken), "0"),
          paste(taken, collapse = " "), sep = "\n")
  write(out, stdout())
}

#------------------------------------------------------------------------------#
### SOLVERS --------------------------------------------------------------------
#------------------------------------------------------------------------------#

### Greedy algorithm -- Best First ---------------------------------------------

solver_take <- function(knap, taken = rep(0, nrow(knap$items)), j = 1, sort_by_density = TRUE, return_type){
  # Inputs--
  # - knap  -- has all of the information--# items, capacity, item properties--that define the problem
  # - taken -- vector of indicators for what has been taken. Defaults to all zeros unless user gives a starting point.
  # - j     -- this is the item to start with. Defaults to 1, unless user gives a different starting point.
  # - sort_by_density -- allows users to go either by problem definition's order, or by value density order.
  if (!return_type %in% c("taken", "optimistic_val")) stop("Invalid argument for `return_type`.")
  if (sort_by_density){
    item_order <- order(with(knap$items, val / wt))
  } else {
    item_order <- 1:nrow(knap$items)
  }
  
  wt <- solution_wt(knap, taken)
  while (j < nrow(knap$items) & solution_wt(knap, taken) < knap$capacity) {
    i <- item_order[j]
    new_wt <- knap$items$wt[i]
    if (wt + new_wt > knap$capacity){
      if (return_type == "taken"){
        return(taken)
      } else if (return_type == "optimistic_val"){
        frac <- (knap$capacity - new_wt) / new_wt
        taken[i] <- frac
        return(solution_val(taken))
      }
    } 
    taken[i] <- 1
    wt <- wt + new_wt
    j <- j + 1
  }
  return(taken)
}

### Branch and Bound -----------------------------------------------------------

# This is a strategy where some take choices have already been made, and we want
# to assess the maximum potential score among the remaining items.
# This method relaxes the integer constraint of 

optimistic_val <- function(knap, taken){
  # Input is 
}

solver_branch_and_bound <- function(knap){
  
}

#------------------------------------------------------------------------------#
### RUN ALGORITHMS -------------------------------------------------------------
#------------------------------------------------------------------------------#

knap <- read_knap(input)

# Greedy type 1
take_first <- solver_take(knap, sort_by_density = FALSE, return_type = "taken")
solution_out(knap, take_first)

# Greedy type 2
take_by_density <- solver_take(knap, sort_by_density = TRUE, return_type = "taken")
solution_out(knap, take_by_density)


