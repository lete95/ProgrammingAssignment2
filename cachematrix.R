##Matrix inversion is usually a costly computation and 
##there may be some benefit to caching the inverse of a 
##matrix rather than compute it repeatedly (there are also 
##alternatives to matrix inversion that we will not discuss here). 
##In this assgnment it is written a pair of functions that cache the 
##inverse of a matrix.

##makeCacheMatrix: This function creates a special "matrix" object 
##that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

##cacheSolve: This function computes the inverse of the special
##"matrix" returned by makeCacheMatrix above. If the inverse has 
##already been calculated (and the matrix has not changed), 
##then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  inverse <- x$getInverse()
  if (!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}




