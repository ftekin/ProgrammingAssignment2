## Since matrix inversion is usually a costly computation, there may be some
## benefit to caching the inverse of a matrix rather than computing it
## repeatedly. Thus, the following functions can compute and cache the inverse
## of a matrix.

## This function creates a special matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
	## Set the matrix and get it
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
	## Set the inverse and get it
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get, 
         setinverse = setinverse, getinverse = getinverse)
}

## This function computes the inverse of the matrix returned by
## makeCacheMatrix(). If the inverse has already been calculated and the matrix
## has not changed, it will retrieve the inverse from the cache directly.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    ## If the inverse has already been calculated and is in the cache
    if (!is.null(inv)) {
            message("getting cached data")
            return(inv)
    }
    ## Otherwise calculate the inverse
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
