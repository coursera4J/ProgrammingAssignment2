## Assignment: Caching the Inverse of a Matrix

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  cacheI<-NULL  #set the cache variable to NULL initially
  set <- function(y)  {
    x <<- y
    cacheI <<- NULL  #reset to NULL when the matrix is changed
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
  if (!is.null(cacheI))  { # returned cached value if available
    message("getting cached inverse")
    return (cacheI)
  }
  
  #caculate inverse if there is no cached value
  data <-x$get()
  cacheI <- solve(data, ...)
  x$setInverse(cacheI)
  cacheI
  
}