## Below are two functions that are used to create a special object
## that stores a numeric matrix and cache's its inverse

## Creates a special "matrix" object that can cache its inverse.
## It is  a list containing functions to
## - set the value of the matrix
## - get the value of the matrix
## - set the value of the inverse
## - get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
    x.inverse <- NULL
    set <- function(y) {
      x <<- y
      x.inverse <<- NULL
    }
    get <- function() x
    setinverse <- function(inv) x.inverse <<- inv
    getinverse <- function() x.inverse
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Returns the inverse of the special "matrix" x
## It first checks to see if the inverse has already been calculated.
## If so, it gets the inverse from the cache and skips the computation.
## Otherwise, it calculates the inverse and sets the result in the cache.
cacheSolve <- function(x, ...) {
  inv <-x$getinverse()
  if(!is.null(inv)) {
    return (inv)
  }
  inv <- solve(x$get())
  x$setinverse(inv)
  inv
}
