
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


### Create data registry for 'entity'

dataRegistry <- function(entity, lvl=NULL) {
  dlist <- sapply(entity, typeof)
  dlen <- sapply(entity, length)
  elem.names <- names(dlist)
  if (length(elem.names) == 0) elem.names <- "Empty"
  reg <- data.frame(index = paste(lvl, 1:length(dlist), sep="."), elem = elem.names, type=as.vector(dlist),
                    len = dlen, ind = 1:length(dlist))
  
  # We can't reference into functions, only data; and for now we'll exclude brain references
  reg <- reg[reg$type != "closure" & rownames(reg) != "brain", ] 
  
  for (i in 1:nrow(reg)) {
    if (reg$type[i] == "list" & reg$len[i] > 0) {
      reg <- rbind(reg, dataRegistry(entity[[reg$ind[i]]], lvl=paste(lvl, reg$ind[i], sep=".")))
    }
  }
  return(reg)
}

lookup <- function(entity, loc) {
  loclist <- as.numeric(strsplit(loc, ".", fixed=TRUE)[[1]])
  if (is.na(loclist[1])) loclist <- loclist[-1]
  if (length(loclist) > 1) {
    lookup(entity[[loclist[1]]], paste(loclist[-1], collapse="."))
  } else {
    return(entity[[loclist]])
  }
}


randMove <- function(loc=c(0,0)) {
  inc <- cut(runif(2, -1.5, 1.5), breaks=c(-2, -0.5, 0.5, 2), labels=FALSE) - 2
  return(loc + inc)
}


whichEntity <- function(loc) {
  for (i in 1:length(world[["ants"]])) {
    if (all(world[["ants"]][[i]]$loc == loc)) {
      return(list("ants", i))
    }
  }
  
  for (i in 1:length(world[["guards"]])) {
    if (all(world[["guards"]][[i]]$loc == loc)) {
      return(list("guards", i))
    }
  }
  
  for (i in 1:length(world[["foods"]])) {
    if (all(world[["foods"]][[i]]$loc == loc)) {
      return(list("foods", i))
    }
  }
  
  return(NULL)
}

isType <- function(x, type) {
  if (type == "scalar") {
    return(length(x) == 1) 
    
  } else if (type == "loc") {
    return(length(x) == 2) 
    
  } else if (type == "vector") {
    return(length(x) > 0 & dim(x) == 0) 
    
  } else if (type == "matrix") {
    return(length(dim(x)) == 2)
    
  } else if (type == "entity") {
    return("loc" %in% names(x))  # All entities have a location
    
  } else return(FALSE)
}


logodds <- function(x) { return(1/(1+exp(-x)))}

# This is more error-correcting than the typical R type conversion
# Correct types are scalar, loc, vector, and matrix
assertType <- function(x, type) {
  if (type == "scalar") {
    return(mean(as.numeric(x), na.rm=TRUE))
    
  } else if (type == "loc") {
    if (length(x) == 0) {
      return(list(0,0))
    } else if (length(x) == 1) {
      return(list(x,x))
    } else if (length(x) == 2) {
      return(as.numeric(x))
    } else if (length(x) > 2) {
      return(x[runif(2, 1, length(x))])
    }
    
  } else if (type == "vector") {
    return(as.numeric(x))
    
  } else if (type == "matrix") {
    if (length(dim(x)) != 2) {
      return(matrix(as.numeric(x)), nrow(length(x)), ncol=1)
    } else {
      return(x)
    }
  }
}

newMap <- function(dim = .MAPDIM) {
  return(list(
    ants = matrix(0, nrow=dim, ncol=dim), 
    guards = matrix(0, nrow=dim, ncol=dim), 
    foods = matrix(0, nrow=dim, ncol=dim), 
    prob = matrix(0, nrow=dim, ncol=dim)))
}

# Each ant will have its own world map, it's own code, status indicators, history

newAnt <- function() {
  return(list(
    map = newMap(),
    loc = uniqueLoc(randLoc()),
    history = list(), # place to store sequence of events
    energy = .FULL,
    load = .EMPTY,
    brain = randomAntBrain(),
    home = c(.MAPCENTER, .MAPCENTER)
  ))
}

scanBrain <- function(brain, lvl=1) {
  nodelist <- as.data.frame(brain)
  
  # nodelist <- lvl
  # 
  #   if (!is.null(brain$children) & length(brain$children) > 0) {
  #     for (i in 1:length(brain$children)) {
  #       nlvl <- paste(lvl, i, sep=".")
  #       nodelist <- c(nodelist, scanBrain(brain$children[[i]], nlvl))
  #     }
  #   }
  
  return(nodelist)
}

rbrain <- function(brain) {
  nodelist <- scanBrain(brain)
  
  # Now we have a list. Pick a node
  rind <- round(runif(1, 1, length(nodelist)))
  return(list(ref=nodelist[rind], brainLookup(brain, ref=nodelist[rind])))
}

brainLookup <- function(brain, ref) {
  reflist <- as.numeric(strsplit(ref, ".", fixed=TRUE)[[1]])
  currnode <- brain  # This is the first "1" in the reflist
  for (i in 2:length(reflist)) {
    currnode <- currnode$children[[reflist[i]]]
  }
  return(currnode)
}

replicateAnt <- function(ant) {
  newant <- newAnt()
  newant$brain <- Clone(ant$brain)
  
  # Now, with some probability, mutate the brain or cross with another brain
  rnd <- runif(1)
  if (rnd < .MUTATE_RATE) {
    res <- rbrain(brain=newant$brain)  # Pick a random note of the tree
    node <- res[[2]]
    
    if (is.null(node$children)) {
      rnd2 <- runif(1)
      
      if (rnd2 < .CHANGE_TERMINAL_NODE) {
        func.new <- terminal.funcs[round(runif(1, 0.51, length(terminal.funcs)+0.5))]  # This could be popularity-weighted
        node.new <- newNode(func.net)
        
        # Need to assign this node to the tree, but how in R? 
        # Trace up one level and replace the corresponding child
      } else {
        
      }
    }
    # Mutate res$node
    
    # If a terminal node, can insert another function or change to another terminal function
    # use get("func_name")
    
    # If a non-terminal node, can delete and suture, can insert, can change the function, 
    #   can change the data, or change the parameter
    
  } else if (rnd < .MUTATE_RATE + .CROSSOVER_RATE) {
    # Pick a second ant. Randomly choose nodes from both ants. Swap this ant's subtree for the other ant's subtree
    
  }
  return(newant)
}

newGuard <- function() {
  return(list(
    map = newMap(),
    loc = uniqueLoc(randLoc()),
    history = list(), # place to store monitoring of ants
    energy = .FULL,
    brain = randomGuardBrain(),
    load = 0  # Always. They don't carry anything
  ))
}

replicateGuard <- function(guard) {
  newguard <- newGuard()
  #newguard$brain <- guard$brain
  return(newguard)
}

newFood <- function() {
  vol <- trunc(runif(1, 0, .MAXFOOD/2) + .MAXFOOD/2)
  
  return(list(
    loc = uniqueLoc(randLoc()),
    load = vol,
    energy = vol  # Since we can't use pointers, we'll have to enforce the relationship of load = energy 
    # for food with each turn. Sloppy.
  ))
}

observe <- function(master.map, entity) {
  # It would be better if the submap were round instead of square
  
  # Forget what was seen before, a little bit
  entity$map$prob <- entity$map$prob - .MEMORY_DECREMENT
  entity$map$prob[entity$map$prob < 0] <- 0
  
  xrng <- (entity$loc[1] - .VISIBILITY):(entity$loc[1] + .VISIBILITY)
  yrng <- (entity$loc[2] - .VISIBILITY):(entity$loc[2] + .VISIBILITY)
  
  # See something new
  entity$map$ants[xrng, yrng] <- master.map$ants[xrng, yrng]
  entity$map$guards[xrng, yrng] <- master.map$guards[xrng, yrng]
  entity$map$foods[xrng, yrng] <- master.map$foods[xrng, yrng]
  
  entity$map$prob[xrng, yrng] <- 1
  
  return(entity)
}
