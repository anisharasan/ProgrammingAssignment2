## The first function creates a cache matrix object, which is then used in the second function to calculate the inverse or retrieve it from the cache. 

## This first function creates the special matrix object that can cache its inverse. It does this by setting and getting the matrix, and then setting and getting the inverse. This is used as an input for the next function. 

makeCacheMatrix <- function(x = matrix()) {
  v <- NULL
  set <- function(y) {
    x <<- y
    v <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) v <<- inverse
  getinverse <- function() v
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This second function computes the inverse, or if it has already been computed it retrieves it from the cache. If it has been calculated, it skips the steps to calculate it, and just retrieves it. If not, it calculates it and sets it in the cache.  

cacheSolve <- function(x, ...) {
  v <- x$getinverse()
  if(!is.null(v)) {
    message("getting cached data")
    return(v)
  }
  data <- x$get()
  v <- solve(data, ...)
  x$setinverse(v)
  v
}
