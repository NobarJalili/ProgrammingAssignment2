## This function creates a special "matrix" object that can cache its inverse
## sets the value of the matrix
## gets the value of the matrix
## sets the value of inverse
## gets the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
 }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}
## This function calculates the inverse of the special "matrix" created with the above function.
##However, it first checks to see if the inverse has already been calculated.
##If so, it gets the inverse from the cache and skips the computation.
##Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the setinverse function.

cacheSolve <- function(x) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
    ## Return a matrix that is the inverse of 'x'
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}