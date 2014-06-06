## Assignment: Caching the Inverse of a Matrix

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  cacheI<-NULL
  set <- function(y)  {
    x <<- y
    cacheI <<- NULL  #reset to NULL 
  }
  
  get <- function() {
    x
  }
  
  setInverse <- function(inverse) {
    cacheI <<- inverse
  }
  
  getInverse <- function()  {
    cacheI;
  }
  
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}



## This function return a matrix that is the inverse of 'x' from cache if available
cacheSolve <- function(x, ...) {
  cacheI <-x$getInverse()
  if (!is.null(cacheI))  {
    message("getting cached inverse")
    return (cacheI)
  }
  data <-x$get()
  cacheI <- solve(data, ...)
  x$setInverse(cacheI)
  cacheI
  
}