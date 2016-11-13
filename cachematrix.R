
# makeCacheMatrix creates a list containing a function to
# a. set the value of the matrix
# b. get the value of the matrix
# c. set the value of inverse of the matrix
# d. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(z) {
    x <<- z
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}
          
# The following function returns the inverse of the matrix. Before computing, 
# it checks if inverse has been computed before. If so, it gets the cached 
# result and skips the computation. If not, it computes the inverse, sets the 
# value in the cache via setinverse function.
          
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("Retrieve cached data")
    return(inv)
  }
  indata <- x$get()
  inv <- solve(indata)
  x$setinverse(inv)
  inv
}
