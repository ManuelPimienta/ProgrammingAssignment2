## The following functions calculate the inverse of an array 
## and store them in cache memory for later access to avoid 
## recalculating the same operation many times.

## The function makeCacheMatrix receives and stores a matrix object 
## and calculates its inverse and stores it in the memory 
## of the function's working environment. In addition, 
## a set of functions are created that are returned to 
## the main environment.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) inv <<- solve
  getsolve <- function() inv
  list(set = set, 
       get = get,
       setsolve = setsolve,
       getsolve = getsolve)

}


## The 'cacheSolve' function retrieves the inverse of the matrix 
## that is generated in the working environment of 'makeCacheMatrix' 
## through the call of the functions created by 'makeCacheMatrix'.
 
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getsolve()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setsolve(inv)
  inv
}
