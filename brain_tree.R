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


node.funcs <- c("branch.brain", "previous.brain", "nearest.brain", "distance.brain", "path.brain", 
                "pathavoid.brain", "avoid.brain")

terminal.funcs <- c("go.brain", "load.brain", "eat.brain", "talk.brain")


# newNode <- function(func, data=NULL, params=NULL) {
#   return(list(
#     func = func,  # Name of a function
#     data = data,  # Address of a data element in this entity's brain, i.e. brain specific
#     params = params,  # A numeric value with a random number generator (bounded distribution) attached
#     children = list()  # 'pointers' to the next level of processing. Not binary and not recursive (yet)
#   ))
# }


baseAntBrain <- function() {
  # Assign the base node
  brain <- Node$new(func="branch.brain", data="load", params=list("0.5"))  # If load is less than 1 unit, load or forage, else return home
  
  nearest <- brain$addChild(func="nearest.brain", data=".1.3") # find food
  distanceto <- nearest$addChild(func="distance.brain") # distance to ...
  iffood <- distanceto$addChild(func="branch.brain", params=c(1.5))
  
  loadfood <- iffood$addChild(func="load.brain")
  
  pathtofood <- iffood$addChild(func="pathavoid.brain", data=c(".1.1",".1.2"))  # Avoid hitting other ants and guards
  gotofood <- pathtofood$addChild(func="go.brain") 
  
  ### Check the coordinates for Home ###
  pathtohome <- brain$addChild(func="pathavoid.brain", data=".1.7", data2=c(".1.1",".1.2")) # loaded, so return home.  Need to add home to the entity
  gotohome <- pathtohome$addChild(func="go.brain")
  
  # When the ant reaches home, it is automatically unloaded and given a small reward as food.
  # If the number of ants is below the max (someone died), a random ant is replicated. 
  # Replication will be automatic if an ant exceeds an energy threshold.
  
  return(brain)
}

baseGuardBrain <- function(guard) {
  # If there is a neighboring ant, and if that ant has no load, then a certain probability of killing the ant
  #    This process culls, a bit randomly, the non-productive workers
  
  # An advanced version of this would watch ants and make decisions about their activities
}



run <- function(entity, brain, input=NULL) {
  res <- do.call(brain$func, args=list(entity, brain=brain$children, input=NULL))
  return(res)
}

branch.brain <- function(entity, brain, input=NULL) {
  val <- input[[1]]
  if (is.null(val)) {  
    val <- lookup(entity, brain$data)
  }
  val <- assertType(val, "scalar")
  if (is.null(val)) { # Still..., then it's a random selection
    val <- runif(1)
  }
  
  ind <- min(cut(val, breaks=c(-Inf, params, Inf), labels=FALSE), length(brain$children))
  res <- do.call(brain$children[[ind]]$func, args=list(entity, brain$children[[ind]], input=c(val,input)))

  return(res)
}

previous.brain <- function(entity, brain, input=NULL) {
  val <- NULL
  if (is.numeric(brain$params)) {
    val <- input[[brain$params]]
  } else if (is.character(brain$params)) {
    for (i in 1:length(input)) {
      if(isType(input[[i]], brain$params)) {
        val <- input[[i]]
        break
      }
    }
  }
  res <- do.call(brain$children[[ind]]$func, args=list(entity, brain$children[[ind]], input=c(val,input)))
  
  return(res)
}

nearest.brain <- function(entity, brain, input=NULL) {
  val <- previous.brain(entity, brain=list(params="matrix"), input)
  if (is.null(val)) {  
    val <- lookup(entity, brain$data)  # Hopefully to choose a map
  }
  val <- assertType(val, "matrix")

  neighbor <- NULL
  if (!is.null(val)) {
    nlist <- which(val>0, arr.ind = T)
    neighbor <- as.numeric(nlist[which.min(sqrt(rowSums(sweep(nlist, 1, as.numeric(entity$loc))^2))), ])
  }
  
  res <- do.call(brain$children[[1]]$func, args=list(entity, brain$children[[1]], input=c(neighbor, input)))
  
  return(res)
}

distance.brain <- function(entity, brain, input=NULL) {
  val <- input[[1]]
  if (is.null(val)) {  
    val <- lookup(entity, brain$data)  # Hopefully to choose a map
  }
  val <- assertType(val, "loc")
  
  dist <- distance(val, entity$loc)

  res <- do.call(brain$children[[1]]$func, args=list(entity, brain$children[[1]], input=c(dist, input)))
  
  return(res)
}

path.brain <- function(entity, brain, input=NULL) {
  move <- NULL
  # To make this more robust, we scan back to find the last location thought about
  dest <- previous(entity, brain=list(params="loc"), input) # Create a mini-brain here, just to pass params
  if (is.null(dest)) {  # No destination, so choose a random direction
    move <- randMove()
    
  } else {  # Otherwise, move toward the destination
    move <- sign(dest - as.numeric(entity$loc))
  }
  
  res <- do.call(brain$children[[1]]$func, args=list(entity, brain$children[[1]], input=c(move, input)))
  
  return(res)
}

avoid.brain <- function(entity, brain, input=NULL) {
  move <- assertType(input[[1]], "loc")
  bad <- list()
  for (i in 1:length(brain$data)) {
    bad[[i]] <- assertType(lookup(entity, brain$data[[i]]), "matrix")  # Hopefully to choose a map
  }
  
  if (is.null(move)) {  # No destination, so choose a random direction
    move <- randMove()
    
  } else {  # Otherwise, move toward the destination
    move.tst <- NULL
    
    for (rot in seq(0, 2*pi, pi/4)) {  # Don't use a while loop, because it could become infinite
      ang <- atan(move[2] / move[1]) + pi/4
      move.tst <- c(sign(sin(ang)), sign(cos(ang)))
      if (bad[cbind(t(move.tst))] == 0) {
        break
      }
    }

    move <- move.tst
  }
  
  res <- do.call(brain$children[[1]]$func, args=list(entity, brain$children[[1]], input=c(move, input)))
  
  return(res)
}

# For robustness, have a function that does both
pathavoid.brain <- function(entity, brain, input=NULL) {
  move <- path.brain(entity, brain, input)
  newmove <- avoid.brain(entity, brain, input=c(move, input))
  
  res <- do.call(brain$children[[1]]$func, args=list(entity, brain$children[[1]], input=c(newmove, input)))
  
  return(res)
}

go.brain <- function(entity, brain, input=NULL) {
  val <- previous(entity, brain=list(params="loc"), input)  # Find the most recent change in coordinates, hopefully
  if (is.null(val)) {  
    val <- lookup(entity, brain$data)  # Hopefully to choose a loc
  }
  val <- assertType(val, "loc")
  
  entity$loc <- assertBoundaries(entity$loc + val) 
  
  return(c("go", input))
}

### This is a special function that can alter other entities, namely food
### If load() is applied to food, it's picked up
### If load() is applied to another ant, it's stealing
### If load() is applied to a guard, nothing happens
load.brain <- function(entity, brain, input=NULL) {
  val <- previous(entity, brain=list(params="loc"), input)  # Find the most recent location to load food from
  if (is.null(val)) {  
    val <- lookup(entity, brain$data)  # Hopefully to choose a loc
  }
  val <- assertType(val, "loc")
  
  ent <- whichEntity(val)
  avail <- .FULL - world[[ent[[1]]]][[ent[[2]]]]$load
  amt <- min(avail, world[[ent[[1]]]][[ent[[2]]]]$load)
  world[[ent[[1]]]][[ent[[2]]]]$load <- world[[ent[[1]]]][[ent[[2]]]]$load - amt  ##### This changes a variable in the global space. Probably not good
  entity$load <- entity$load + amt
  
  return(c("go", input))
}

### This is a special function that can alter other entities
### If eat() is applied to food, it becomes energy
### If eat() is applied to another ant, it kills the ant and absorbs it's energy
### If eat() is applied to a guard, it kills the guard and absorbs it's energy
eat.brain <- function(entity, brain, input=NULL) {
  val <- previous(entity, brain=list(params="loc"), input)  # Find the most recent location to load food from
  if (is.null(val)) {  
    val <- lookup(entity, brain$data)  # Hopefully to choose a loc
  }
  val <- assertType(val, "loc")
  
  ent <- whichEntity(val)
  avail <- .FULL - world[[ent[[1]]]][[ent[[2]]]]$energy
  amt <- min(avail, world[[ent[[1]]]][[ent[[2]]]]$energy)
  world[[ent[[1]]]][[ent[[2]]]]$energy <- world[[ent[[1]]]][[ent[[2]]]]$energy - amt  ##### This changes a variable in the global space. Probably not good
  entity$energy <- entity$energy + amt
  
  return(c("go", input))
}

talk.brain <- function(entity, brain, input=NULL) {  # Communication between ants
  val <- previous(entity, brain=list(params="loc"), input)  # Find the most recent location to load food from
  if (is.null(val)) {  
    val <- lookup(entity, brain$data)  # Hopefully to choose a loc
  }
  val <- assertType(val, "loc")
  
  ent <- whichEntity(val)

    ### Trade map information, both directions
  
}
