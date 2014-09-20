## Cache the inverse of a matrix
## Example:
## x = makeCacheMatrix(matrix(c(1,2,3,4), nrow=2, ncol=2))
## cacheSolve(x)
## x$set(matrix(c(1,5,3,4), nrow=2, ncol=2))

## Input: a matrix
## Returns: essentially an object with four public methods
##  set: assigns a new value to the matrix "field" and clears the inverse field
##  get: returns the matrix
##  setInverse: compute inverse of matrix and store 
##  getInverse: return the inverse of matrix 
makeCacheMatrix <- function(x = matrix()) {
   inv <- NULL
   set <- function(y) {
      x <<- y
      inv <<- NULL
   }
   get <- function() x
   setInverse <- function(solve) inv <<- solve
   getInverse <- function() inv
   list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## if inverse of x is not cached, compute it and cache it
## Input: a matrix x
## Returns: the cached inverse of x 
cacheSolve <- function(x, ...) {
   ## Return a matrix that is the inverse of 'x'
   inv <- x$getInverse()
   if (!is.null(inv)) {
      message("Getting cached data")
      return(inv)
   }
   data <- x$get()
   inv <- solve(data, ...)
   x$setInverse(inv)
   inv
}
