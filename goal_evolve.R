#####
#
# Copyright (c) Joseph L. Breeden, 2023
#
# This is my simulator to evolve goals
#
# This code is provided for research purposes only. 
# It may not be used for any commercial purpose without written permission of the author.
# This code is provided without any warranty or promise of effectiveness.
#
#####

rm(list = ls())
setwd() # Set your working directory for the project. The scripts should be stored in a folder within your project.

library(data.tree)
library(plot.matrix)

#
# Load the utility functions and brain functions
#

source("scripts/util_funcs.R")
source("scripts/brain_tree.R")

# Simulation parameters

.FULL <- 50
.EMPTY <- 0
.MAPDIM <- 101
.MAPCENTER <- trunc(.MAPDIM/2) + 1
.NUMANTS <- 20
.NUMGUARDS <- 10
.VISIBILITY <- 3
.GUARDVISIBILITY <- 5
.MEMORY_DECREMENT <- 0.2
.MAXFOOD <- 200
.NUMFOOD <- 10
.REPLICATE_THRESHOLD <- .FULL * 2
.MUTATE_RATE <- 0.1
.CHANGE_TERMINAL_NODE <- 0.5
.CROSSOVER_RATE <- 0.1


###
### The main loop
###

# Update map

for (k in 1:100) {
  master.map <- updateMap(master.map, ants, guards, foods)
  
  plotMap(master.map)
  
  #
  # Run Ants
  #
  
  for (i in 1:length(world[["ants"]])) {
    world[["ants"]][[i]] <- observe(master.map, world[["ants"]][[i]])
    res <- run(world[["ants"]][[i]], world[["ants"]][[i]]$brain)
    world[["ants"]][[i]]$history <- c(res[[1:5]], world[["ants"]][[i]]$history)  # Store the last 5 thoughts as part of the history 
    
    # Adjust energy
    world[["ants"]][[i]]$energy <- world[["ants"]][[i]]$energy - 1
    if (world[["ants"]][[i]]$energy == 0) {
      world[["ants"]][[i]] <- NULL
    }
  }

  
  #
  # Run Guards
  #

  for (guard in world[["guards"]]) {
    world[["guards"]][[i]] <- observe(master.map, world[["guards"]][[i]])
    res <- run(world[["guards"]][[i]], world[["guards"]][[i]]$brain)
    world[["guards"]][[i]]$history <- c(res[[1:5]], world[["guards"]][[i]]$history)  # Store the last 5 thoughts as part of the history 

    # Adjust energy. Guards don't need to forage or "eat", but if they are killed (0 energy), they need to be replaced
    if (world[["guards"]][[i]]$energy == 0) {
      world[["guards"]][[i]] <- NULL
    }
  }
  
  
  #
  # Add Ants if needed
  #
  
  # Replicate the 'fat' ants
  for (i in 1:length(world[["ants"]])) {
    if (world[["ants"]][[i]]$energy > .REPLICATE_THRESH) {
      j <- min(which(is.null(world[["ants"]])), length(world[["ants"]]) + 1)  # Pick a slot for the new ant
      world[["ants"]][[j]] <- replicate.ant(world[["ants"]][[i]])
      world[["ants"]][[i]]$energy <- world[["ants"]][[i]]$energy - world[["ants"]][[j]]$energy
    }
  }
  
  # Make a list of dead ants and assign replacements
  dead.ants <- which(is.null(world[["ants"]]))
  if (length(dead.ants) > 0) {
    for (i in 1:length(dead.ants)) {
      candidates <- which(!(1:length(world[["ants"]]) %in% dead.ants))
      world[["ants"]][[dead.ants[i]]] <- 
        replicate(world[["ants"]][[candidates[round(runif(1, 1, length(candidates)))]]])
    }
  }
  
  
  #
  # Add Guards if needed
  #
  
  # Make a list of dead guards and assign replacements
  dead.guards <- which(is.null(world[["guards"]]))
  if (length(dead.guards) > 0) {
    for (i in 1:length(dead.guards)) {
      candidates <- which(!(1:length(world[["guards"]]) %in% dead.guards))
      world[["guards"]][[dead.guards[i]]] <- 
        replicate(world[["guards"]][[candidates[round(runif(1, 1, length(candidates)))]]])
    }
  }


  #
  # Add food if needed (This could be replaced with a food growth function)
  #
  
  for (i in 1:length(world[["foods"]])) {
    if (world[["foods"]][[i]]$volume <= 0) {
      world[["foods"]][[i]] <- newFood()
    } else {  # This bit is needed just to make the functions generic when we don't have pointers
      val <- min(world[["foods"]][[i]]$load, world[["foods"]][[i]]$energy)
      world[["foods"]][[i]]$load <- val
      world[["foods"]][[i]]$energy <- val
    }
  }
  
}
