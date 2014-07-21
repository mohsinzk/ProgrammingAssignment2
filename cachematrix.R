## The two functions below are meant to take an invertible square matrix
## and return its inverse. If the same matrix is applied successively
## to the function makeCacheMatrix, the second function cacheSolve will
## retrieve the inverse from cache

## function makeCacheMatrix takes invertible square matrix as argument 
## and returns a list with 4 functions

makeCacheMatrix <- function(x = matrix()) {

  inv <- NULL
  
  set <- function (y)
  {
    x <<- y 
    inv <<- NULL
  }
  
  get <- function() x
  setSolve <- function(inverse) inv <<- inverse
  getSolve <- function() inv
  
  list (set = set, get = get, setSolve = setSolve, getSolve = getSolve)

}


## function cacheSolve takes a list as argument 
## and returns its inverse from either cache or a new one
## depending on whether the same matrix is being sent

cacheSolve <- function(x, ...) {
        
  inv <- x$getSolve()
  if (!is.null(inv))
  {
    message("Getting cached data..")
    return(inv)
  }
  
  matrix <- x$get()
  inv <- solve(matrix)
  x$setSolve(inv)
  inv  
}
