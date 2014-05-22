## The functions avoid repeated computations of inverse of an invertible matrix
## by caching it.

## This function creates a special matrix object to cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## This function will compute the inverse of the output of the function makeCacheMatrix above. However, if the inverse has
## already been computed, then it will retreive it from cache, provided the matrix has not changed.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
        message("getting cached data")
        return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
## Return a matrix that is inverse of x.
}