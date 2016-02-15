## The pair of functions below provides a mechanism to cache and 
## retrieve the inverses of matrices

## This function creates an object wrapping around the matrix x.
## Local variable x, and inverse are stored in a different
## environment using <<- assignment. Getter and setter methods
## are defined.

makeCacheMatrix <- function(x = matrix()) {
  
  inverse <- NULL
  
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  
  get <- function() x
  
  setInverse <- function(i) inverse <<- i
  
  getInverse <- function() inverse
  
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Return a matrix that is the inverse of 'x'
## If inverse has been cached, just retrieve it, otherwise calculate it.

cacheSolve <- function(x, ...) {
        
  
  inverse <- x$getInverse()
  
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  
  data <- x$get()
  
  inverse <- solve(data, ...)
  x$setInverse(inverse)
  inverse
}
