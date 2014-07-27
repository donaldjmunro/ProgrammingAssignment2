## Store a matrix and its inverse. The following two functions work in
## tandem. The first provide storage and access to that storage for the
## matrix and its inverse. The second function calulates and srores the
## inverse to the matrix or if the inverse has allready been calculated
## retrieves it.

## Stores the matrix and a placeholder for the inverse. Provides setter 
## and getter functions for both matrix and inverse. Returns all four 
## functions stored in a list.
makeCacheMatrix <- function(x = matrix()) {
  inverse <- Null
  ## Sets the matrix and resets inverse
  setMatrix <- function(aMatrix){
    x <<- aMatrix
    inverse <<- NULL
  }
  getMatrix <- function(){
    x
  }
  setInverse <- function(anInverse){
    inverse <<- anInverse
  }
  getInverse <- function(){
    anInverse
  }
  list(setMatrix = setMatrix, getMatrix = getMatrix,
       setInverse = setInverse, getInverse = getInverse )
}


## Checks for a inverse if it does not exist it is calculated and 
## stored, otherwise it is retrieved. Returns the inverse.
cacheSolve <- function(x, ...) {
  inverse <- x$getInverse()
  if(!is.null(inverse)){
    message("getting cached inverse")
    return(inverse)
  }
  aMatrix <- x$getMatrix()
  anInverse <- solve(aMatrix)
  x$setInverse(anInverse)
  anInverse
}
