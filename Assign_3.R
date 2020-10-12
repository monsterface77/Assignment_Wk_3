MakeCacheMatrix <- function (x= matrix()) {
  inv <- NULL
  set <- function (y) {
    x <<- y
    inv <<- NULL
  }
  get <- function () {x}
  setInverse <- function (inverse) {inv <<- inverse}
  getInverse <- function () {x}
  list(set = , get = get, setInverse = setInverse, getInverse = getInverse)
}

CacheSolve <- function (x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message ("Getting Cache Date")
    return (inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setinverse(inv)
  inv
}
