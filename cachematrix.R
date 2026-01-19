## Put comments here that give an overall description of what your
## functions do: The makeCacheMatrix function creates a special "matrix"
## object cache the inverse. The cacheSolve function computes the inverse
## given by the makeCacheMatrix function. If the inverse is allready 
## computed (and the matrix did not change) then the cacheSolve function
## should get the inverse from the cache.

## Write a short comment describing this function: This function creates 
## a special "matrix" object cache the inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x 
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, 
       get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)
}


## Write a short comment describing this function: This function computes
## the inverse of the special "matrix" from the makeCacheMatrix function.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
        ## Return a matrix that is the inverse of 'x'
