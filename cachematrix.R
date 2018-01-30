## Caching the Inverse of A Square Invertible Matrix
## stores a matrix and caches its inverse

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    invrs <- NULL
    set <- function(y) {
      x <<- y
      invrs <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) invrs<<- inverse
    getInverse <- function() invrs
    
    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" 
## created by the above function.
## If the inverse has already been calculated, then 
## it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invrs <- x$getInverse()
        if (!is.null(invrs)){
          message("getting cached data")
          return(invrs)
        }
        mtrx <- x$get()
        invrs<- solve(mtrx, ...)
        x$setInverse(invrs)
        invrs
}
