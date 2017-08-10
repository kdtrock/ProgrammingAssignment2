## makeCacheMatrix creates a special "matrix" object that can cache its inverse
## cache solve wil compute the inverse of the special objcet returned by makeCacheMatrix,
## if it's already been calculated, it will retrieve it from the cache

## makeCacheMatrix, takes an inversible matrix as argument and returns a 
## list of four named functions:set, get, setsolve, and getsolve, that
## can store the inverse of our matrix and be used as a cache
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
set <- function(y) {
  x <<- y
  m <<- NULL
}
get <- function() x
setsolve <- function(solve) m <<- solve
getsolve <- function() m
list(set = set, get = get,
     setsolve = setsolve,
     getsolve = getsolve)

}


## cachesolve takes as argument the special object created by makeCachematrix
## and calls the functions by name.It returns the inverse of the original matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
