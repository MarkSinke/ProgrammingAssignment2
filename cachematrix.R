## The functions in this file create a special "kind" of matrix that caches its inverse.
## When the matrix changes, the inverse is invalidated, so a next call to retrieve the
## inverse will actually compute it again.

## Make a "caching" matrix.
##
## makeCacheMatrix creates a special "matrix" (really a list of the "set", "get", "setinverse"
## and "getinverse" functions that respectively set and get the data of the matrix and set and
## get its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setInverse <- function(inv) inverse <<- inv
    getInverse <- function() inverse
    
    list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Compute the inverse of the matrix ("solve the matrix")
##
## This function will cache the results of the operation as long as the matrix stays the same.
## Note that any differences in arguments to the cacheSolve function are not
## taken into account when caching the solved value. For example, when the cacheSolve
## function is used a second time to actually solve a set of linear equations, with different right-hand
## sides of the linear system, instead of the (default) identity matrix, the function
## will (incorrectly) return the previously cached inverse, instead of the actual solution.
##
## For example:
## > m <- makeCacheMatrix(matrix(1:4, 2, 2))
## > cacheSolve(m)
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## Now solving the matrix for another right-hand side matrix, like
## > solve(m$get(), matrix(c(2, 0, 0, 2), 2, 2))
##      [,1] [,2]
## [1,]   -4    3
## [2,]    2   -1
## but
## > cacheSolve(m, matrix(c(2,0,0,2), 2, 2))
## getting cached data
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## which is clearly incorrect.
cacheSolve <- function(x, ...) {
    m <- x$getInverse()
    if (!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    inverse <- solve(data, ...)
    x$setInverse(inverse)
    inverse
}
