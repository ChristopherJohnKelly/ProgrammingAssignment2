## Stores a matrix, calculates and stores the inverse of that
## matrix

## gets or sets x, an invertible matrix, and gets or sets its
## inverse

makeCacheMatrix <- function(x = matrix()) {
  x.inv <- NULL
  set <- function(y) {
    x <<- y
    x.inv <<- NULL
  }
  get <- function() x
  setinv <- function(inv) x.inv <<- inv
  getinv <- function() x.inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## For x, checks if the matrix has changed. If so calculates
## its inverse. If not checks if inverse is calculated, and
## either returns the cache or calculates

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  if (x == x$get){
    x.cacheinv <- x$getinv
    if (!is.null(x.inv)) {
      return(x.cacheinv)
    }
  } else {
    x$set = x
  }
  
  x.calcinv <- solve(x)
  x$setinv = x.calcinv
  return(x.calcinv)
  
}
