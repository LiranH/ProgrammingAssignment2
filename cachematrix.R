## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix creates a new matrix. This matrix is a special matrix
## that is a list containing a function that will set and then get the
## value of a matrix, as well as set and get the value of the inverse.

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  
  setMatrix <- function(y){
    x <<- y
    inverse <<- NULL
  }
  
  getMatrix <- function() x
    setInverse <- function(solve) inverse <<- solve
    getInverse <- function() inverse
  
  list(setmatrix = setMatrix, getMatrix = getMatrix,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function calculates the inverse of the matrix created by the above function.
## It checks if the inverse has been calculated. If it has, the inverse is grabbed
## from the cache and the computation is skipped. If not, it calculates the inverse
## and sets its value in the cache via setInverse.

cacheSolve <- function(x, ...) {
    inverse <- x$getInverse()
    
    if(!is.null(inverse)){
      message("Getting the cached data")
      return(inverse)
    }
    
    data <- x$getMatrix()
    inverse <- solve(data, ...)
    x$setInverse(inverse)
    inverse    
}

