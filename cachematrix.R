## The functions aim to cache the inverse of a matrix.

## makeCacheMatrix: creates a special type of matrix object, 
## a cacheable matrix, which can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL          
  set <- function(y) {      
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) inverse <<- solve
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## This function aims to calculate the inverse of 
## the special "matrix" returned by makeCacheMatrix function. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      inverse <- x$getinverse()
      if(!is.null(inverse)) {
          message("getting cached data")
          return(inverse)
      }
      ## if not
      data <- x$get()
      inverse <- solve(data, ...)
      x$setinverse(inverse)
      inverse
}
