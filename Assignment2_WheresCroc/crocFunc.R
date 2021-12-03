myCrocFunc <-function(moves, readings, tourists, paths, holes){
  
  if(moves$mem$status > moves$mem$prevStatus){
    moves$mem$state = 0
    moves$mem$turn = NULL
    moves$mem$transitionMatrix = NULL
  }
  if(is.null(moves$mem$turn) == TRUE){
    moves$mem$prevStatus = moves$mem$status
    moves$mem$status ++ 
    
    holeProbs <- calcProbs(readings, holes)
    state <- createInitialState(tourists)
    transitionMatrix <- createTransitionMatrix(paths)
    newState <- createNewState(transitionMatrix,state, holeProbs)
    newState <- removeBackpackerPos(newState, tourists)
    moves$mem$state = newState
    moves$mem$transitionMatrix = transitionMatrix
    
    maxIndex <- which.max(newState)
    
    moves$moves[[1]] <- getMoves(tourists[[3]], maxIndex, paths)
    moves$moves[[2]] <- getMoves(moves$moves[[1]], maxIndex, paths)
    moves$mem$turn = 1
    
    
  }
  else if(moves$mem$turn > 0)
  {
    holeProbs <- calcProbs(readings, holes)
    prevState = moves$mem$state
    transitionMatrix = moves$mem$transitionMatrix
    
    newState <- createNewState(transitionMatrix,prevState, holeProbs)
    cat("1: ", tourists[1],"__","2: ", tourists[2],"\n")
    if(tourists[1] < 0|| tourists[2] < 0){
      print("dead")
      newState <- checkBackpackerDead(newState, tourists)
      
    }
    newState <- removeBackpackerPos(newState, tourists)
    moves$mem$state = newState
    
    maxIndex <- which.max(newState)
    moves$moves[[1]] <- getMoves(tourists[[3]], maxIndex, paths)
    moves$moves[[2]] <- getMoves(moves$moves[[1]], maxIndex, paths)
    
  }
  return(moves)
}

createInitialState <- function(tourists){
  stateVector = c(rep(1,40))
  position1 = tourists[1]
  position2 = tourists[2]
  stateVector = stateVector * (1/38)
  stateVector[position1] = 0
  stateVector[position2] = 0
  
  return(stateVector)
}

createTransitionMatrix <-function(paths){
  init <- matrix(0, 40, 40, byrow = TRUE)
  
  for(i in 1:nrow(init)){
    avPaths <- which(paths==i, arr.ind = TRUE)
    appear = 1 + (length(avPaths)/2)
    init[i,i] = 1/appear
    
    for(j in 1:nrow(avPaths)){
      path <- paths[avPaths[j,1],]
      for(a in path){
        if(a!=i){
          init[a,i] = 1/appear
        } 
      }
      
    }
  }
  return(init)
}

removeBackpackerPos <- function(state, tourists){
  
  tourist1 = tourists[1]
  tourist2 = tourists[2]
  if(is.na(tourist1) == FALSE){
    state[tourist1] = 0
  }
  
  else if(is.na(tourist2) == FALSE){
    state[tourist2] = 0
  }
  
  else{
    return(state)
  }
  return(state)
}

checkBackpackerDead <- function (state, tourists){
  cat("Turist har dött", tourists[1], tourists[2])
  tourist1 = tourists[1]
  tourist2 = tourists[2]
  
  if(tourist1 < 0){
    state = replicate(40, 0)
    state[tourist1] = 1
    
  }
  else if(tourist2 < 0){
    state = replicate(40, 0)
    state[tourist1] = 1
  }
  return(state)
}

createNewState <- function(transitionMatrix, state, holeProbs){
  diagHoleProbs = diag(holeProbs,40,40)
  state_temp = rbind(state)
  state = state_temp
  temp = state %*% transitionMatrix 
  newState = temp %*% diagHoleProbs
  temp = normalize(newState)
  newState = temp
  return(newState)
}

normalize <- function(state){
  min = min(state)
  max = max(state)
  newState = ((state - min) / (max - min))
  
  return(newState)
}



probOfHole <- function(readings, holeVals){
  salProb <- dnorm(readings[1], mean = holeVals[1,1], sd = holeVals[1,2])
  phosProb <- dnorm(readings[2], mean = holeVals[2,1], sd = holeVals[2,2])
  nitProb <- dnorm(readings[3], mean = holeVals[3,1], sd = holeVals[3,2])
  return(salProb*phosProb*nitProb)
}

calcProbs <- function(readings, holes){
  numHoles <- nrow(holes[[1]])
  probs <- c()
  
  for(hole in 1:numHoles){
    holeVals = matrix(nrow=3,ncol=2)
    for(i in 1:3){
      holeVals[i,1] <- holes[[i]][hole,1]
      holeVals[i,2] <- holes[[i]][hole,2]
    }
    probs[[hole]] <- probOfHole(readings, holeVals)
    
  }
  return(probs)
}

getMoves <-function(current,goal,paths){
  #IF the current positon == goal position, return 0 to search for crocodile
  if(current==goal){
    return(0)
  }
  #Checks which indices in paths that are the same as current
  avPaths <- which(paths==current, arr.ind = TRUE)
  distances = c()
  
  #
  for(i in 1:nrow(avPaths)){
    if (length(nrow(avPaths)) == 0 || nrow(avPaths) == 0){
      return(0)
    }
    
    else if (current == goal){
      
      return(0)
    }
    else{
      #Path is possible nodes that the searcher can currently access
      path <- paths[avPaths[i,1],]
      a <- paths[avPaths[i,1],]
      for(a in path){
        if(a!=current){
          distances[[i]] <- abs(goal-a)
        }
      }
      
    }
    
  }
  return(paths[avPaths[which.min(distances)],which(paths[avPaths[which.min(distances)],]!=current)])
}