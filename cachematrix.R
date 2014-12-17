## This script file define functions for creating and manipulating 
## "cachable" matrix object that can cache its inverse itself


## NAME : makeCacheMatrix
## ARGUMENT(S)
##     x : matrix object
## RETURN TYPE : "cachable" matrix object
## DESCRIPTION : This function turn the input matrix object
##     into a "cachable" matrix object and return it

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## NAME : cacheSolve
## ARGUMENT(S)
##     x : "cachable" matrix object
##   ... : further arguments passed to solve() function if 
##         calculation is needed
## RETURN TYPE : matrix object
## DESCRIPTION : This function return the inverse of the input
##     "cachable" matrix. If cached data found, this function will 
##     return cached data directly instead of calculate it again

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
