## Matrix inverse computation cache module.

## **makeCacheMatrix**
## makeCacheMatrix function wraps a matrix into an object that can store
## two matrixes: original matrix and its cached inverse.
## The result object itself is a list of named functions that share same parent evironment.
## Original matrix can be accessed with get() and set() functions.
## Cached inverse can be accessed with getInverse() and setInverse() functions.

makeCacheMatrix <- function(x = matrix()) {
  
  # define variables to store matrix and its inverse in makeCacheMatrix environment
  
  value <- x
  cachedInverse <- NULL
  
  # define accessor functions
  
  setValue <- function(newX = matrix()) {
    value <<- newX
    cachedInverse <- NULL
  }

  getValue <- function() value
  
  setInverseValue <- function(newInverse = matrix()) {
    cachedInverse <<- newInverse
  }
  
  getInverseValue <- function() cachedInverse
  
  ## return list of named accessor functions
  
  list(set = setValue, get = getValue,
       setInverse = setInverseValue, getInverse = getInverseValue)
}


## **cacheSolve**
## cacheSolve functions takes a matrix wrapped with makeCacheMatrix
## function and returns the inverse value of the matrix.
## Actual computation is performed only once per each wrapped matrix.
## Consequent calls to cacheSolve on the same wrapped matrix
## will return the cached result.

cacheSolve <- function(x, ...) {
  inverse <- x$getInverse()
  if (!is.null(inverse)) {
    ## the inverse was already stored in x
    message("Using cached inverse value")
    return(inverse) 
  }
  
  ## the inverse wasn't stroed in x, compute
  inverse <- solve(x$get(), ...)
  
  ## store computed value in x, and return it
  x$setInverse(inverse)
  inverse
}









