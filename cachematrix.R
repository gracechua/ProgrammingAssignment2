## This is a pair of functions that caches the inverse of a matrix
## rather than having to repeatedly compute it. 
## Modified from example code provided in Assignment 2 readme. 

## This function creates a 'matrix' object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    y <- NULL ## sets variable y to null 
    set <- function(z) {  ## sets x to the argument z, sets y to null 
      x <<- z
      y <<- NULL
    }
    get <- function() {  ## 'get' is a function that returns x 
      x
    }
    setinverse <- function(solve) {
      y <<- solve
    }
    getinverse <- function() {
      y
    }
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
  }
  


## Retrieves and returns the cached inverse matrix if it already exists.
## If it does not exist, solves to get the inverse matrix, then returns that.

cacheSolve <- function(x, ...) {
  y <- x$getinverse()  ## attempts to get the inverse matrix if it was cached previously
  if(!is.null(y)) {  ## if y already exists, return 'getting cached data' and return m 
    message("getting cached data")
    return(y)
  }
  data <- x$get() ## if not, 
  y <- solve(data) ## get and cache the inverse of x 
  x$setinverse(y)
  y  
}
