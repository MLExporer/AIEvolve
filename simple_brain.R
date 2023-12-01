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

angleToCoord <- function(angle, dist=1.5) { # dist=1.5 rounds to a step to any octant
  aToC <- function(angle, dist) { 
    loc <- round(dist*c(sin(angle), cos(angle))) 
    return(data.frame(row=loc[1], col=loc[2]))
  }
  
  if (is.data.frame(angle)) {  # Then 'dist' information is packed into 'angle'
    if (nrow(angle) > 1) {
      return(do.call(rbind.data.frame, apply(angle, 1, angleToCoord)))
    } else {
      return(aToC(angle[1,1], angle[1,2]))
    }
    
  } else if (length(angle) > 1) {  # Again, 'dist' information is packed into 'angle', but a single vector
    return(aToC(angle[1], angle[2]))
    
  } else {
    return(aToC(angle, dist))
  }
}

distanceTo <- function(loc1, loc2) {
  return(sqrt(sum((loc1 - loc2)^2, na.rm=TRUE)))
}

angleTo <- function(loc1, loc2) {
  return(atan2(loc2[2] - loc1[2], loc2[1] - loc1[1]))
}

map.lst <- function(map) {
  return(as.data.frame(which(map > 0, arr.ind=T)))
}

nearest <- function(center, map, n) {
  nz.ind <- map.lst(map)
  
  # If we can see enough nearby items, then fill with random, distant items
  if (nrow(nz.ind) < n) {
    for (j in (nrow(nz.ind)+1):n) {
      repeat {
        loc <- randLoc()
        if (distanceTo(center, loc) > .VISIBILITY) { # Then we keep it 
          nz.ind[j, ] <- loc
          break
        }
      }
    }
  }
  
  for (j in 1:nrow(nz.ind)) {
    nz.ind$angle[j] <- angleTo(as.numeric(center), as.numeric(nz.ind[j, c(1,2)]))
    nz.ind$dist[j] <- distanceTo(as.numeric(center), nz.ind[j, c(1,2)])
  }
  
  return(nz.ind[1:n, c("angle", "dist")])
}

newMap <- function(dim = .MAPDIM) {
  return(list(
    ants = matrix(0, nrow=dim, ncol=dim), 
    guards = matrix(0, nrow=dim, ncol=dim), 
    foods = matrix(0, nrow=dim, ncol=dim), 
    prob = matrix(0, nrow=dim, ncol=dim)))
}



run <- function(brain, entity) {
  res <- brain(entity)
  
  # The function should return probabilities of Go, Eat, Talk, and Replicate with a direction for the first three
  # Direction is just an angle in radians
  
  action <- paste0("action.", names(which.max(res$Action)))
  entity <- do.call(action, args=list(angle=res$Direction, entity=entity))
  
  entity$history[2:.MAXHISTORY] <- entity$history[1:(.MAXHISTORY-1)]
  entity$history[[1]] <- res  # Store the last 5 thoughts as part of the history 
  
  return(entity)
}

### This moves the ant
action.Go <- function(angle, entity) {

  entity$loc <- assertBoundaries(entity$loc + angleToCoord(angle)) 
  return(entity)
}

randomAntBrain <- function(ant) {
  # Assign the base node as a random move
  # The next generation will compute a direction based upon a NN analysis of the Ant's map
  
  return(list(Action=c(Go=1, Eat=0, Talk=0, Replicate=0), Direction=runif(1, 0, 2*pi)))
}

randomGuardBrain <- function(guard) {
  # Assign the base node as a random eat (kill), so the guards won't be moving
  return(list(Action=c(Go=0, Eat=1, Talk=0, Replicate=0), Direction=runif(1, 0, 2*pi)))
}

avoid <- function(desired, obstacles) {
  if (is.null(obstacles)) {
    return(desired)
  }
  if (nrow(obstacles) == 0) {
    return(desired)
  }
  
  # This finds the nearest available angle to the desired angle.
  # Compute all blocked angles relative to desired, convert to octants, invert to available
  # Then return nearest available octant, convert to an angle, add back original (reference) angle
  
  blocked <- round(obstacles$angle - desired / (pi/4))
  blocked[blocked > 4] <- blocked[blocked > 4] - 4
  avail <- seq(-4, 4)[! seq(-4, 4) %in% blocked]
  return(avail[which.min(abs(avail))] * pi/4 + desired)
}

decisionAnt <- function(entity, nearest.ants, nearest.guards, nearest.foods) {
  dcsn <- list(Action=c(Go=0, Eat=0, Talk=0, Replicate=0), Direction=0) # Just making a template
  
  # The starting ant brain ignores other ants and guards, except to avoid colliding.
  
  if (entity$food >= .MAXFOOD) {
    dcsn$Action[["Go"]] <- 1
    dcsn$Direction <- angleTo(entity$loc, .HOME)
    
    obstacles <- rbind(nearest.ants[nearest.ants$dist < 1.5, ],
                       nearest.guards[nearest.guards$dist < 1.5, ])
    dcsn$Direction <- avoid(desired=dcsn$Direction, obstacles=obstacles)
  
    } else if (min(nearest.foods$dist) < 1.5) { # On a retangular grid, a diagonal neighbor is sqrt(2) away
    dcsn$Action[["Eat"]] <- 1
    dcsn$Direction <- nearest.foods$angle[which.min(nearest.foods$dist)]

      } else {  # Go to nearest food
    dcsn$Action[["Go"]] <- 1
    dcsn$Direction <- nearest.foods$angle[which.min(nearest.foods$dist)]
    
    obstacles <- rbind(nearest.ants[nearest.ants$dist < 1.5, ],
                       nearest.guards[nearest.guards$dist < 1.5, ])
    dcsn$Direction <- avoid(desired=dcsn$Direction, obstacles=obstacles)
  }
  
  return(dcsn)
}
  
  
  
