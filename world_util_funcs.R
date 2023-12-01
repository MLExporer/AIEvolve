
#####
#
# Copyright (c) Joseph L. Breeden, 2023
#
# Utility functions for goal_evolv.R
#
# This code is provided for research purposes only. 
# It may not be used for any commercial purpose without written permission of the author.
# This code is provided without any warranty or promise of effectiveness.
#
#####

randLoc <- function() {
  lc <- round(rnorm(2, mean=trunc(.MAPDIM/2)+1, sd=.MAPDIM/5))
  lc[lc < 0] <- 0
  lc[lc > .MAPDIM] <- .MAPDIM
  return(lc)
}

randMove <- function(loc=c(0,0)) {
  inc <- cut(runif(2, -1.5, 1.5), breaks=c(-2, -0.5, 0.5, 2), labels=FALSE) - 2
  return(loc + inc)
}

uniqueLoc <- function(loc) {
  a.dists <- NULL
  g.dists <- NULL
  f.dists <- NULL
  attempts <- 0
  repeat {
    if (length(world[["ants"]]) > 0) {
      for (i in 1:length(world[["ants"]])) {
        a.dists[i] <- distance(loc, world$ants[[i]]$loc)
      }
    }
    if (length(world[["guards"]]) > 0) {
      for (i in 1:length(world[["guards"]])) {
        g.dists[i] <- distance(loc, world$guards[[i]]$loc)
      }
    }
    if (length(world[["foods"]]) > 0) {
      for (i in 1:length(world[["foods"]])) {
        f.dists[i] <- distance(loc, world$foods[[i]]$loc)
      }
    }
    ind <- c(which(a.dists < 1), which(g.dists < 1), which(f.dists < 1))
    if (length(ind) == 0 | attempts > 10) {
      break  # We don't want an infinite loop
    }
    else {
      loc <- randLoc()
      attempts <- attempts + 1
    }
  }
  return(loc)
}

distance <- function(loc1, loc2) {
  return(sqrt(sum((loc1 - loc2)^2, na.rm=TRUE)))
}

assertBoundaries <- function(loc) {
  while (loc[1] > .MAPDIM | loc[1] < 0 | loc[2] > .MAPDIM | loc[2] < 0) {
    loc <- randMove(loc)
  }
  
  return(loc)
}

updateMap <- function(master.map) {
  master.map$ants[] <- 0
  for (i in 1:length(world$ants)) {
    master.map$ants[as.numeric(world$ants[[i]]$loc[1]), as.numeric(world$ants[[i]]$loc[2])] <- i  # Store ant ID at a map location
  }
  
  master.map$guards[] <- 0
  for (i in 1:length(world$guards)) {
    master.map$guards[as.numeric(world$guards[[i]]$loc[1]), as.numeric(world$guards[[i]]$loc[2])] <- i  # Store guard ID at a map location
  }
  
  master.map$foods[] <- 0
  for (i in 1:length(world$foods)) {
    master.map$foods[as.numeric(world$foods[[i]]$loc[1]), as.numeric(world$foods[[i]]$loc[2])] <- world$foods[[i]]$food  # Store food amount at a map location
  }
  
  return(master.map)
}

plotMap <- function(master.map) {
  # Need some nice plotting code
  
  gmap <- master.map$guards
  gmap[gmap > 0] <- gmap[gmap > 0] + 100
  fmap <- master.map$foods
  fmap[fmap > 0] <- fmap[fmap > 0] + 200
  map.graph <- master.map$ants + gmap + fmap
  map.graph <- matrix(cut(map.graph, breaks=c(0,1,100,200,300),
                          labels=FALSE), nrow=nrow(master.map$ants), ncol=ncol(master.map$ants))
  
  #plot(map.graph, col=c("white", "blue", "red", "green"))
}


initWorld <- function() {
  # Store everything in the "world"
  world <- list(ants = list(), guards = list(), foods = list())
  
  # Create ants
  for (i in 1:.NUMANTS) {
    world$ants[[i]] <- newAnt()
  }
  
  # Create guards
  for (i in 1:.NUMGUARDS) {
    world$guards[[i]] <- newGuard()
  }
  
  # Create food
  for (i in 1:.NUMFOOD) {
    world$foods[[i]] <- newFood()
  }
  
  return(world)
}

# TODO: Convert all map logic to sparse matrix, since we're not going to use convolutional networks
observe <- function(master.map, entity) {
  # It would be better if the submap were round instead of square
  
  # Forget what was seen before, a little bit
  entity$map$prob <- entity$map$prob - .MEMORY_DECREMENT
  entity$map$prob[entity$map$prob < 0] <- 0
  
  xrng <- as.numeric(entity$loc[1] - .VISIBILITY):as.numeric(entity$loc[1] + .VISIBILITY)
  yrng <- as.numeric(entity$loc[2] - .VISIBILITY):as.numeric(entity$loc[2] + .VISIBILITY)
  
  xrng[xrng < 1 | xrng > .MAPDIM] <- NA
  yrng[yrng < 1 | yrng > .MAPDIM] <- NA
  ind <- as.matrix(na.omit(data.frame(xrng, yrng)))
  
  master.map$ants[ind]
  
  # See something new
  entity$map$ants[ind] <- master.map$ants[ind]
  entity$map$guards[ind] <- master.map$guards[ind]
  entity$map$foods[ind] <- master.map$foods[ind]
  
  entity$map$prob[ind] <- 1  # TODO: This is not needed. Just decrement the ant / guard / food values directly
  
  return(entity)
}

# Each ant will have its own world map, it's own code, status indicators, history

newHistory <- function() {
  history <- list()
  for (i in 1:.MAXHISTORY) {  # initialize with empty, 0s.  (NN brains don't like NAs)
    history <- list(Action=c(Go=0, Eat=0, Talk=0, Replicate=0), Direction=0)
  }
  return(history)
}

newAnt <- function() {
  return(list(
    map = newMap(),
    loc = uniqueLoc(randLoc()),
    history = newHistory(), # place to store sequence of events
    food = .FULL,
    brain = randomAntBrain  # Assign the needed function
  ))
}

newGuard <- function() {
  return(list(
    map = newMap(),
    loc = uniqueLoc(randLoc()),
    history = list(), # place to store monitoring of ants
    food = .FULL,
    brain = randomGuardBrain
  ))
}

newFood <- function() {
  vol <- trunc(runif(1, 0, .MAXFOOD/2) + .MAXFOOD/2)
  
  return(list(
    loc = uniqueLoc(randLoc()),
    food = vol
  ))
}




