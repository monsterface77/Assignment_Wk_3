## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##This creates a space for the cache, setting it at NULL when they function is 
##ran is sets the inverse fo the function then retreives it and creates a list 
##with the inverse value of the matrix
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function (y) {
    x <<- y
    inv <<- NULL
  }
  get <- function () {x}
  setInverse <- function (inverse) {inv <<- inverse}
  getInverse <- function () {inv}
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function
##this solves the function and if the inverse is already known it will retreive
##it, otherwise it will solve the function.
cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message ("Getting Cached Data")
    return (inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv               
}
  ## Return a matrix that is the inverse of 'x'
test <- makeCacheMatrix(matrix(1:4, nrow =2, ncol=2))
test$get()
test$getInverse()
cacheSolve(test)
