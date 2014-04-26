## Put comments here that give an overall description of what your
## functions do

# Write a short comment describing this function
# A list contains a function to
# set the value of the matrix
# get the value of the matrix
# set the value of the inverse
# get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list (set = set, get = get,
        setinv = setinv,
        getinv = getinv)
}

## Write a short comment describing this function
# If the inverse has been calculated, it gets the inverse from the cache.
# Otherwise, it calculates the inverse of the matrix and sets the value of inverse in cache via setinv function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data,...)
  x$setinv(m)
  m
}