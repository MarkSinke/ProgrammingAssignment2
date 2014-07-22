## The functions in this file create a special "kind" of matrix that caches its inverse.
## When the matrix changes, the inverse is invalidated, so a next call to retrieve the
## inverse will actually compute it again.

## Make a "caching" matrix.
##
## makeCacheMatrix creates a special "matrix" (really a list of the "set", "get", "setinverse"
## and "getinverse" functions that respectively set and get the data of the matrix and set and
## get its inverse.

makeCacheMatrix <- function(x = matrix()) {
    ## cached inverse
    inverse <- NULL
    ## cached arguments used to compute the inverse - if an solution is requested with
    ## different arguments, the cached inverse will not be returned.
    arguments <- NULL

    set <- function(y) {
        x <<- y
        inverse <<- NULL
        args <<- NULL
    }
    get <- function() x
    setInverse <- function(inv, ...) { 
        inverse <<- inv
        arguments <<- list(...)
    }
    getInverse <- function(...) {
        if (identical(list(...), arguments)) inverse else NULL
    }
    
    list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Compute the inverse of the matrix ("solve the matrix")
##
## This function will cache the results of the last solve operation as long as the matrix
## stays the same. If the remaining arguments to the cacheSolve function are different
## from the last call, a new inverse will be computed and stored in the cache.
##
## This fixes the incorrect behavior exhibited in the previous version of this function
## (see previous commit).
cacheSolve <- function(x, ...) {
    m <- x$getInverse(...)
    if (!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    inverse <- solve(data, ...)
    x$setInverse(inverse, ...)
    inverse
}
