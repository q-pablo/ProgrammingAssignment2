## calling cacheSolve(makeCacheMatrix(A)) where A is a num matrix, will either
## calculate the inverse of A or fetch the cached value if its inverse was
## previously calculated.


## this function stores the matrix and the inverse. It also gets the values on
## request
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(mean) m <<- solve
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function will verify if the given matrix's inverse is in storage and if
## not, it'll calculate the value, cache it, and return it.
cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinv(m)
  m
}
