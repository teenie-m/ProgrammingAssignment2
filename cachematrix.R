## based heavily on makeVector and cachemean from Assignment 2 description
## code adjusted to take the inverse of a matrix rather than the mean of a vector
## "m" and "mean" variables changed to "inverse.cache" and "inverse"

## returns functions that set/retrieve matrix (x) and its inverse (inverse.cache)
## x and inverse.cache are "private" variables
## i.e. they exist only within the environment defined by makeCacheMatrix

makeCacheMatrix <- function(x = matrix()) {
  inverse.cache <- NULL
  set <- function(y) {
    x <<- y # <<- sets x and inverse.cache in parent (makeCacheMatrix) environment
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
## calls functions therein to set and/or retrieve inverse.cache variable

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
