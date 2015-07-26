## These two functions cache the inverse of a matrix, assuming matrix is always
##invertible

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inverseM <- NULL
    set <- function(y) {
        x <<- y
        inverseM <<- NULL
    }
    get <- function() x
    setInverse <- function(solve) inverseM <<- solve
    getInverse <- function() inverseM
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## If the inverse has already been calculated (and the matrix has not changed), 
##then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    
    inverseM <- x$getInverse()
    if(!is.null(inverseM)) {
        message("getting cached data")
        return(inverseM)
    }
    data <- x$get()
    inverseM <- solve(data, ...)
    x$setInverse(inverseM)
    inverseM
}
