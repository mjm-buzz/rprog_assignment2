# The following pair of functions cache the inverse of a matrix.
# Caching the inverse is generally favorable to repetitive computation.

# makeCacheMatrix creates a special "matrix" object that can cache its inverse.
# Specifically, it makes a List of functions that do the following:
#       1. set the value of the matrix
#       2. get the value of the matrix
#       3. set the value of the inverse of matrix
#       4. get the value of the inverse of matrix

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
    setInverse = setInverse, getInverse = getInverse)
}


# cacheSolve computes the inverse of the special "matrix" returned by
# makeCacheMatrix above. If the inverse has already been calculated
# (and the matrix has not changed), then cacheSolve will retrieve the
# inverse from the cache.

cacheSolve <- function(x, ...) {
    inverse <- x$getInverse()
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data, ...)
    x$setInverse(inverse)
    inverse
}
