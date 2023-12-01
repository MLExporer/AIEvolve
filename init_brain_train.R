#####
#
# Copyright (c) Joseph L. Breeden, 2023
#
# This code used to generate data based upon a hard-coded decision tree. Then an over-parameterized neural network 
# is created to replicate the movements of the ants, essentially translating the decision tree to a neural network.
#
# This code is provided for research purposes only. 
# It may not be used for any commercial purpose without written permission of the author.
# This code is provided without any warranty or promise of effectiveness.
#
#####

rm(list = ls())
gc()

setwd() # Set your working directory for the project. The scripts should be stored in a folder within your project.

#library(data.tree)
library(plot.matrix)

#
# Load the utility functions and brain functions
#

source("scripts/simple_brain.R")
source("scripts/world_util_funcs.R")  # This also initializes global objects: world, master.map


# Simulation parameters

.FULL <- 50
.EMPTY <- 0
.MAPDIM <- 101
.MAPCENTER <- trunc(.MAPDIM/2) + 1
.HOME <- .MAPCENTER
.NUMANTS <- 50
.NUMGUARDS <- 50
.VISIBILITY <- 10
.GUARDVISIBILITY <- 10
.MEMORY_DECREMENT <- 0.2  # 1 / (number of turns to forget completely)
.MAXFOOD <- 200
.NUMFOOD <- 50
.REPLICATE_THRESHOLD <- .FULL * 2
.MUTATE_RATE <- 0.1
.CHANGE_TERMINAL_NODE <- 0.5
.CROSSOVER_RATE <- 0.1
.MAXHISTORY

###
### Initial world and master.map
###

world <- NULL
world <- initWorld()

# Create map
master.map <- list(
  ants = matrix(0, nrow=.MAPDIM, ncol=.MAPDIM),
  guards = matrix(0, nrow=.MAPDIM, ncol=.MAPDIM),
  foods = matrix(0, nrow=.MAPDIM, ncol=.MAPDIM)
)

###
### First I need ants and guards to wander randomly in order to generate data
###

ant.to.ants <- list()
ant.to.guards <- list()
ant.to.foods <- list()
decision <- list()
ant.state <- list()

for (k in 1:1000) {
  master.map <- updateMap(master.map)
  
  plotMap(master.map)
  
  #
  # Run Ants
  #
  
  for (i in 1:length(world[["ants"]])) {
    world[["ants"]][[i]] <- observe(master.map, entity=world[["ants"]][[i]])
    world[["ants"]][[i]] <- run(brain=world[["ants"]][[i]]$brain, entity=world[["ants"]][[i]])
    
    # Adjust energy
    # world[["ants"]][[i]]$energy <- world[["ants"]][[i]]$energy - 1
    # if (world[["ants"]][[i]]$energy == 0) {
    #   world[["ants"]][[i]] <- NULL
    # }
    
    # Generate training data
    
    ant.to.guards[[(k-1)*length(world[["ants"]]) + i]] <- 
      list(self=world[["ants"]][[i]]$loc, map=map.lst(map=world[["ants"]][[i]]$map$guards), 
           nearest=nearest(center=world[["ants"]][[i]]$loc, map=world[["ants"]][[i]]$map$guards, n=3))
    ant.to.ants[[(k-1)*length(world[["ants"]]) + i]] <- 
      list(self=world[["ants"]][[i]]$loc, map=map.lst(map=world[["ants"]][[i]]$map$ants), 
           nearest=nearest(center=world[["ants"]][[i]]$loc, map=world[["ants"]][[i]]$map$ants, n=3))
    ant.to.foods[[(k-1)*length(world[["ants"]]) + i]] <- 
      list(self=world[["ants"]][[i]]$loc, map=map.lst(map=world[["ants"]][[i]]$map$foods), 
           nearest=nearest(center=world[["ants"]][[i]]$loc, map=world[["ants"]][[i]]$map$foods, n=3))
    
    decision[[(k-1)*length(world[["ants"]]) + i]] <- 
      decisionAnt(entity=world[["ants"]][[i]], 
                  nearest.ants=ant.to.guards[[(k-1)*length(world[["ants"]]) + i]]$nearest,
                  nearest.guards=ant.to.ants[[(k-1)*length(world[["ants"]]) + i]]$nearest,
                  nearest.foods=ant.to.foods[[(k-1)*length(world[["ants"]]) + i]]$nearest)

    ant.state.ki <- c(loc=world[["ants"]][[i]]$loc, food=world[["ants"]][[i]]$food, 
                      max.food=.MAXFOOD, home=.HOME)
    for (j in 1:length(world[["ants"]][[i]]$history)) {
      hj <- unlist(world[["ants"]][[i]]$history[[j]])
      names(hj) <- paste0(c(names(world[["ants"]][[i]]$history[[j]]), "Direction"), j)
      ant.state.ki <- c(ant.state.ki, hj)
    }
    # May need to unlist() ant.state.ki
    
    ant.state[[(k-1)*length(world[["ants"]]) + i]] <- ant.state.ki
  }
}

### Prepare training data

collectData <- function(source, nument) {
  inputs <- matrix(data=NA, nrow=length(source), ncol=2*nument+2)
  targets <- matrix(data=NA, nrow=length(source), ncol=2*3)
  for (k in 1:length(source)) {
    # Include randomly generated 'nearest' items in map list
    if (nrow(source[[k]]$map) < nrow(source[[k]]$nearest)) { 
      nrst <- angleToCoord(source[[k]]$nearest[(nrow(source[[k]]$map)+1):nrow(source[[k]]$nearest),])
      colnames(nrst) <- c("row", "col")
      source[[k]]$map <- rbind(source[[k]]$map, nrst)
    } else if (nrow(source[[k]]$map) > nument) {
      source[[k]]$map <- source[[k]]$map[1:nument, ]
    }
    in.k <- source[[k]]$map
    
    # Fill out map list with distant phantoms
    if (nrow(in.k) < nument) {
      fill.len <- nument-nrow(in.k)
      fill.start <- nrow(in.k) + 1
      in.k <- rbind(in.k, in.k[rep(nrow(in.k), fill.len), ])
      in.k[fill.start:nument, ] <- angleToCoord(data.frame(angle=runif(fill.len, 0, 2*pi), dist=2/3*.MAPDIM))  # Put entities not observed beyond the map boundaries
    }
    
    in.k <- t(in.k)
    in.k <- c(as.numeric(source[[k]]$self), c(rbind(in.k[1,], in.k[2,])))
    inputs[k, ] <- in.k
    
    trg.k <- t(source[[k]]$nearest)
    trg.k <- c(rbind(trg.k[1,], trg.k[2,]))
    targets[k, ] <- trg.k
  }
  
  colnames(inputs) <- c("self.x", "self.y", c(rbind(paste0("x", 1:nument), paste0("y", 1:nument)))) 
  colnames(targets) <- c(rbind(paste0("nearest.x", 1:3), paste0("nearest.y", 1:3)))
  
  return(list(inputs=inputs, targets=targets))
}

atg.data <- collectData(source=ant.to.guards, nument=.NUMGUARDS)
ata.data <- collectData(source=ant.to.ants, nument=.NUMANTS)
atf.data <- collectData(source=ant.to.foods, nument=.NUMFOOD)

save(atg.data, ata.data, atf.data, decision, file="base_brain_training.rdata")
#load(file="base_brain_training.rdata", verbose=1)

inputs <- rbind(atg.data$inputs, ata.data$inputs)
inputs <- rbind(inputs, atf.data$inputs)

targets <- rbind(atg.data$targets, ata.data$targets)
targets <- rbind(targets, atf.data$targets)

rm.ind <- which(targets[,2] < 1 | targets[,4] < 1 | targets[,6] < 1)
inputs <- inputs[-rm.ind, ]
targets <- targets[-rm.ind, ]

targets[, c(2,4,6)] <- log(targets[, c(2,4,6)])  # They are lognormally distributed
hist(targets[, c(2,4,6)])
hist(targets[, c(1,3,5)])

###
###  Build NN models of distance calculations and decision making
###

library("keras")
library("tensorflow")

###
### Model estimation parameters
###

test.frac <- 0.50

val_indices <- sample(1:nrow(inputs), trunc(test.frac * nrow(inputs)))
val_data <- inputs[val_indices, ]
val_targets <- targets[val_indices, ]

# Prepare the training data: all non-validation rows
train_data <- inputs[-val_indices,]
train_targets <- targets[-val_indices, ]

num_epochs <- 2000
nodes <- c(1.0, 1.0, 1.0, 1.0, 0.8, 0.4, 0.2)


###
### Begin estimation
###


#
# Create a multi-target model
#


CircErr <- function( y_true, y_pred ) {  # I will assume these are both vectors (ang1, logdist1, ang2, logdist2, ang3, logdist3)
  K <- backend()
  K$mean(abs(pi - abs(abs(y_true[,c(1,3,5)] - y_pred[,c(1,3,5)]) - pi)))/pi + 
    K$mean(abs(y_true[,c(2,4,6)] - y_pred[,c(2,4,6)]))

}

build_model <- function() {
  model <- keras_model_sequential() %>% 
    layer_dense(units = ncol(inputs), activation = "softplus", 
                input_shape = dim(inputs)[[2]])
  for (i in 1:length(nodes)) {
    model %>% layer_dense(units = trunc(ncol(targets) + (ncol(inputs) - ncol(targets)) * nodes[i]), 
                          activation = "softplus")
  }
  
  model %>% layer_dense(units = ncol(targets), activation = "softplus") %>% 
    layer_dense(units = ncol(targets)) 
  
  model %>% compile(
    optimizer = "rmsprop", 
    loss = CircErr, 
    metrics = c("mae")
  )
}


# Build the Keras model (already compiled)
model <- build_model()

# Train the model (in silent mode, verbose=0)
history <- model %>% fit(
  train_data, train_targets,
  validation_data = list(val_data, val_targets),
  epochs = num_epochs, batch_size = 32, verbose = 1
)


model.nearest <- load_model_tf(file="model_nearest.hdf5") #We'll remove dates from future names

###
### Now predict the decisions
###

targets <- matrix(NA, nrow=length(decisions), ncol=length(decisions[[1]]$Action)+1)

for (i in 1:length(decisions)) {
  targets[i, 1:length(decisions$Action)] <- as.numeric(decisions[[i]]$Action)
  targets[i, length(decisions$Action)+1] <- as.numeric(decisions[[i]]$Direction)
}
colnames(targets) <- c(names(decisions[[1]]$Action), "Direction")

# Inputs include ant state, history, system limits, and all nearest neighbors

neighbors <- rbind(atg.data$targets, ata.data$targets)
neighbors <- rbind(neighbors, atf.data$targets)

inputs.colnames <- c(names(decisions[[1]]$Action), "Direction", names(ant.state[[1]]), 
                     colnames(neighbors))

inputs <- matrix(NA, nrow=length(decisions), ncol=length(inputs.colnames))  # Have to count this up

naction <- length(decisions[[1]])

for (i in 1:length(decisions)) {
  inputs[i, 1:naction] <- as.numeric(decisions[[i]]$Action)
  inputs[i, naction+1] <- decisions[[i]]$Direction
  
  inputs[i, (naction+2):(naction+2+length(ant.state[[i]]))] <- as.numeric(ant.state[[i]])
  runstart <- naction+2+length(ant.state[[i]])
  inputs[i, runstart:(runstart+ncol(neighbors))] <- neighbors[i, ]
}

colnames(inputs) <- inputs.colnames



# Worry about training guards next
{
  
  #
  # Run Guards
  #
  
  for (i in 1:length(world[["guards"]])) {
    world[["guards"]][[i]] <- observe(master.map, world[["guards"]][[i]])
    world[["guards"]][[i]] <- run(brain=world[["guards"]][[i]]$brain, entity=world[["guards"]][[i]])

    # Adjust energy. Guards don't need to forage or "eat", but if they are killed (0 energy), they need to be replaced
    # if (world[["guards"]][[i]]$energy == 0) {
    #   world[["guards"]][[i]] <- NULL
    # }
  }
  
  
  #
  # Add Ants if needed
  #
  
  # Replicate the 'fat' ants
  # for (i in 1:length(world[["ants"]])) {
  #   if (world[["ants"]][[i]]$energy > .REPLICATE_THRESH) {
  #     j <- min(which(is.null(world[["ants"]])), length(world[["ants"]]) + 1)  # Pick a slot for the new ant
  #     world[["ants"]][[j]] <- replicate.ant(world[["ants"]][[i]])
  #     world[["ants"]][[i]]$energy <- world[["ants"]][[i]]$energy - world[["ants"]][[j]]$energy
  #   }
  # }
  
  # Make a list of dead ants and assign replacements
  # dead.ants <- which(is.null(world[["ants"]]))
  # if (length(dead.ants) > 0) {
  #   for (i in 1:length(dead.ants)) {
  #     candidates <- which(!(1:length(world[["ants"]]) %in% dead.ants))
  #     world[["ants"]][[dead.ants[i]]] <- 
  #       replicate(world[["ants"]][[candidates[round(runif(1, 1, length(candidates)))]]])
  #   }
  # }
  
  
  #
  # Add Guards if needed
  #
  
  # Make a list of dead guards and assign replacements
  # dead.guards <- which(is.null(world[["guards"]]))
  # if (length(dead.guards) > 0) {
  #   for (i in 1:length(dead.guards)) {
  #     candidates <- which(!(1:length(world[["guards"]]) %in% dead.guards))
  #     world[["guards"]][[dead.guards[i]]] <- 
  #       replicate(world[["guards"]][[candidates[round(runif(1, 1, length(candidates)))]]])
  #   }
  # }
  # 
  
  #
  # Add food if needed (This could be replaced with a food growth function)
  #
  
  # for (i in 1:length(world[["foods"]])) {
  #   if (world[["foods"]][[i]]$volume <= 0) {
  #     world[["foods"]][[i]] <- newFood()
  #   } else {  # This bit is needed just to make the functions generic when we don't have pointers
  #     val <- min(world[["foods"]][[i]]$load, world[["foods"]][[i]]$energy)
  #     world[["foods"]][[i]]$load <- val
  #     world[["foods"]][[i]]$energy <- val
  #   }
  # }
  
  
  
}
