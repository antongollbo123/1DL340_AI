makeMoves <-function(moves, readings, tourists, paths, holes){
  
  if(moves$mem$status == 1){
    moves$mem$status = 0
    moves$mem$state = 0
    moves$mem$turn = NULL
  }
  if(is.null(moves$mem$turn) == TRUE){
    
    holeProbs <- calcProbs(readings, holes)
    state <- createInitialState(tourists)
    transitionMatrix <- createTransitionMatrix(paths)
    newState <- createNewState(transitionMatrix,state, holeProbs)
    newState <- removeBackpackerPos(newState, tourists)
    moves$mem$state = newState
    moves$mem$transitionMatrix = transitionMatrix
    
    maxIndex <- which.max(newState)
    
    path = bfsSearch(tourists[3], maxIndex, paths)
    moves$moves = checkPath(path)
    
    moves$mem$turn = 1
    
    
  }
  else if(moves$mem$turn > 0)
  {

    holeProbs <- calcProbs(readings, holes)
    prevState = moves$mem$state
    transitionMatrix = moves$mem$transitionMatrix
    
    newState <- createNewState(transitionMatrix,prevState, holeProbs)
    newState <- removeBackpackerPos(newState, tourists)
    
    if(tourists[1] < 0 && is.na(tourists[1]) != TRUE){
      newState <- checkBackpackerDead(newState, tourists)
      
    }
    else if(tourists[2] < 0 && is.na(tourists[2]) != TRUE){
      newState <- checkBackpackerDead(newState, tourists)
      
    }

    moves$mem$searchPos = NULL
    maxIndex <- which.max(newState)
    path = bfsSearch(tourists[3], maxIndex, paths)
    # moves$moves[[1]] <- getMoves(tourists[[3]], maxIndex, paths)
    # moves$moves[[2]] <- getMoves(moves$moves[[1]], maxIndex, paths)

    
    moves$moves = checkPath(path)
    
    if(moves$moves[[1]] == 0){
      newState[tourists[[3]]] = 0
      
    }
    else if(moves$moves[[2]] == 0){
      newState[moves$moves[[1]]] = 0
    }
    moves$mem$state = newState
    
  }
  return(moves)
}

checkPath <- function(path){
  path_length = length(path)
  if(path_length >= 2) {
    moves= c(path[1], path[2])
  }

if(path_length == 1){
  moves = c(path[1], 0)
}

if(path_length == 0){
  moves=c(0,0)  
}
  return (moves)
}

bfsSearch = function(node, goal, edges) {
  parent_list = replicate(40, 0)
  visit_list = c(node)
  q = c(node)
  parent_list[node] = -1
  while (length(q) != 0) {
    currentNode = head(q, n=1)
    q = setdiff(q, c(currentNode))
    neighbor_list = c(edges[which(edges[,1]==currentNode),2],edges[which(edges[,2]==currentNode),1],currentNode)
    neighbor_list = setdiff(neighbor_list, c(currentNode))
    neighbor_list = setdiff(neighbor_list, visit_list)
    for (i in neighbor_list) {
      if (!(i %in% visit_list)) {
        q = c(q, i)
        parent_list[i] = currentNode
        visit_list = c(visit_list, c(i))
      }
    }
  }
  
  currentNode = goal
  path = numeric()
  while (currentNode != -1) {
    if (parent_list[currentNode] != -1) {
      path = c(c(currentNode), path)
    }
    currentNode = parent_list[currentNode]
  }
  
  return (path)
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

  tourist1 = tourists[1]
  tourist2 = tourists[2]
  
  if(tourist1 < 0 && is.na(tourist1) != TRUE){
    state = replicate(40, 0)
    state[tourist1*-1] = 1

    
  }
  else if(tourist2 < 0 && is.na(tourist2) != TRUE){
    state = replicate(40, 0)
    state[tourist2*-1] = 1

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

