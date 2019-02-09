#Matrix inversion is usually a costly computation and there may be some benefit
#to caching the inverse of a matrix rather than compute it repeatedly (there are
#also alternatives to matrix inversion that we will not discuss here). Your
#assignment is to write a pair of functions that cache the inverse of a matrix.

#Write the following functions:
  
#makeCacheMatrix: This function creates a special "matrix" object that can cache
#its inverse.

#Assume that the matrix supplied is always invertible

makeCacheMatrix <- function(x = matrix()) {
  #Set the inverse to NULL as a placeholder
    i <- NULL
  
  #Define a function to set the matrix x to a new matrix y, and reset i to NULL
    set <- function(y) {
      x <<- y
      i <<- NULL
    }
  
  #Define a function to return matrix x
    get <- function() x
  
  #Define function to set the inverse
    setinv <- function(inverse) i <<- inverse
  
  #Define function to return the inverse
    getinv <- function() i
  
  #Return a list with the four functions defined above
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}

#cacheSolve: This function computes the inverse of the special "matrix" returned
#by makeCacheMatrix above. If the inverse has already been calculated (and the matrix
#has not changed), then the cachesolve should retrieve the inverse from the cache.
     
cacheSolve <- function(x, ...) {
  #Get the cached value of the inverse
    i <- x$getinv()
     
  #If the cache is not empty, return the cached value of the inverse
    if(!is.null(i)) {
        message("getting cached data")
    return(i)
    }
     
  #If the cache is empty, get the value of the matrix, then calculate, cache and
  #return the inverse
    data <- x$get()
    i <- solve(data, ...)
    x$setinv(i)
    i
}
  
