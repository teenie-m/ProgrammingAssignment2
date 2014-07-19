## functions for cacheing the computed inverse of a matrix
## based heavily on makeVector and cachemean from the Assignment 2 description
## "m" and "mean" variables changed to "inverse.cache" and "inverse" respectively

## makeCacheMatrix takes a matrix 'x' as an argument
## 'x' and 'inverse.cache' (stores inverse of 'x') are "private" variables
## i.e. they exist only in the environment defined by makeCacheMatrix
## makeCacheMatrix returns list of functions for setting or retrieving them

makeCacheMatrix <- function(x = matrix()) {
  inverse.cache <- NULL
  set <- function(y) {
    x <<- y # '<<-' sets x and inverse.cache in parent (makeCacheMatrix) environment
    inverse.cache <<- NULL
  }
  get <- function() x
  set.inverse <- function(inverse) inverse.cache <<- inverse
  get.inverse <- function() inverse.cache
  list(set = set, get = get,
       set.inverse = set.inverse,
       get.inverse = get.inverse)
}

## cacheSolve takes list defined by makeCacheMatrix as input
## calls functions therein to set and/or retrieve inverse.cache

cacheSolve <- function(x, ...) {
  inverse <- x$get.inverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  matrix.in <- x$get()
  inverse <- solve(matrix.in, ...)
  x$set.inverse(inverse)
  inverse #returns inverse whether previously cached or not
}
