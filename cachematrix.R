## Given that matrix inversion is sometimes a costly computation, there are
## benefits to caching the inverse of a matrix rather than computing it repeatedly.
## The two functions below will cache the inverse of a matrix, 
## assuming that the matrix supplied is indeed invertible.

## The makeCacheMatrix function makes a "special matrix" that can cache its inverse.
## The "special matrix" is a list containing a function to set the value of the matrix,
## get the value of the matrix, set the value of the inverse of the matrix,
## and get the value of the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## The cacheSolve function calculates the inverse of the "special matrix"
## created by the makeCacheMatrix function.
## It first checks to see if the inverse of the matrix has already been calculated.
## If it has, it gets the inverse from the cache and skips the computation of the inverse.
## If it has not already been calculated, it calculates the inverse of the matrix
## and sets the value of the inverse in the cache.

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}

