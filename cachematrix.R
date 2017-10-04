## write a pair of functions namely "makeCacheMatrix()" and "cacheSolve()" that cache the inverse of a matrix.

## makeCacheMatrix() is a function which creates a special "matrix" object that can cache its inverse(invertible square matrix).

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    setMatrix <- function(y){
      x <<- y
      inv <<- NULL
    }
    getMatrix <- function() x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    
    list(setMatrix=setMatrix, getMatrix=getMatrix, setInverse=setInverse, getInverse= getInverse)
}


## cacheSolve() is a function which computes the inverse of the special "matrix" returned by makeCacheMatrix() function. 
## If the inverse has already been calculated (and the matrix has not changed), then the cacheSolve() retrieves 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$getMatrix()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
}
