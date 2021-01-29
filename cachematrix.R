## These functions take the inverse of a matrix and store the values in cache.


## Generates a list of functions that will be used to get a vector, calculate
##the inverse and set the inverse in cache

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(z) m <<- solve(z)
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## This function checks to see if an matrix inverse has been carried out before,
##if not, it carries out the calculations

cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
