## The "makeCacheMatrix" function creates a special "matrix" object that can 
## cache it's inverse and the "cacheSolve" function computes the inverse of 
## the special "matrix" returned by the "makeCacheMatrix" function. If the 
## inverse has already been calculated (and the matrix has not changed), then 
## the "cachesolve" function should retrieve the inverse from the cache.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  m <- x$getmean()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- mean(data, ...)
  x$setmean(m)
  m
}
