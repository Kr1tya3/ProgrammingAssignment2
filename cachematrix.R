## Put comments here that give an overall description of what your
## functions do

## Returns a wrapper for a matrix that allows it to cache
## the result of an inversion calculation


makeCacheMatrix <- function(matrixValue = matrix()) {
  cachedInverse <- NULL
  set <- function(newValue) {
    matrixValue <<- newValue
    cachedInverse <<- NULL
  }
  get <- function() matrixValue
  setinverse <- function(inverse) cachedInverse <<- inverse
  getinverse <- function() cachedInverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Takes a makeCacheMatrix, returns the inverse of its 
## wrapped matrix and caches the result. 
## If the cached result is available, then it is returned straight away.

cacheSolve <- function(cacheMatrix, ...) {
  cachedInverse <- cacheMatrix$getinverse()
  if(!is.null(cachedInverse)) {
    message("getting cached data")
    return(cachedInverse)
  }
  data <- cacheMatrix$get()
  inverse <- solve(data, ...)
  cacheMatrix$setinverse(inverse)
  inverse
}
