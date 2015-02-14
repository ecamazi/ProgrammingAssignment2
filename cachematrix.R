## Calculates the inverse of a matrix and cache the result for later use

## Takes an invertible matrix as argument and returns a list containing
## functions to get/set the matrix and its inverse. The list serves as input for the
## next function

makeCacheMatrix <- function(x = matrix())
{
  inv <- NULL
  set <- function(y)
  {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## Let A = an invertible matrix.
## Takes the returnvalue of makeCacheMatrix$set(A) as argument and
## returns the inverse of A. 
## If the result was calculated and cached before, the cached value is returned,
## otherwise the inverse is calculated

cacheSolve <- function(x, ...) 
{
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv))
  {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv  
}
