## The first functions takes a matrix as and input and creates a cache that can be written, along 
## with methods(functions) that can be applied to the special object. The second function takes the
## output of the first function and check if a cache exists. If not, it writes a new one with the 
## inverse matrix.

## makeCacheMatrix creates a special "matrix" list: a matrix with a cache and a list of functions 
## in order to set and get either the matrix or its cache

makeCacheMatrix <- function(x = matrix()) {
cacheMatrix <- NULL
      set <- function(y) {
            x <<- y
            cacheMatrix <<- NULL
      }
      get <- function() x
      setInverse <- function(solve) cacheMatrix <<- solve
      getInverse <- function() cacheMatrix
      list(set = set, get = get,
           setInverse = setInverse,
           getInverse = getInverse)
}


## cacheSolve checks the special "matrix" object for a cached inverse, and if it exists, it gets it. 
## Otherwise it solves for the inverse matrix

cacheSolve <- function(x, ...) {
      cacheMatrix <- x$getInverse()
      if(!is.null(cacheMatrix)) {
            message("Getting cached matrix data")
            return(cacheMatrix)
      }
      data <- x$get()
      cacheMatrix <- solve(data, ...)
      x$setInverse(cacheMatrix)
      cacheMatrix
}
