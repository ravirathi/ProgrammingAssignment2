## This function takes the matrix as input. It has subfunctions to cache its inverse, pass the matrix and its inverse to the
## subsequent function from where this function is called.

makeCacheMatrix <- function(z = matrix()) {
    x <<- z
    inv <<- NULL
  
  get <- function() return(x)
  setinv <- function(inverse) inv <<- inverse
  getinv <- function()
  {
    return(inv)
  }
  list(get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse
##from the cache.

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinv(inv)
  inv
}
