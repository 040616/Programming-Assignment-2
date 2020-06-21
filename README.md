## This function shows the inverse matrix of an invertible matrix.
makeCacheMatrix <- function(x = matrix()) {
  ## this function creates an array
  inverse <- NULL
  set <- function(y){
    x <<- y
    inverse <<- NULL
  }
  ## creates an inverse matrix
  get <- function() x
  setInverse <- function(matrix) inverse <<- matrix
  getInverse <- function() inverse
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## this function calculates the inverse of the matrix created in makeCacheMatrix
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse <- x$getInverse()
  if(!is.null(inverse)){
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setInverse(inverse)
  
}
