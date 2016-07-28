## Two functions which calculate the inverse of a matrix,
## if already not calculated, and stores it in the cache
## for future use.

## This function creates a special matrix object which can be
## used to cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list (set = set, get = get,
              setinverse = setinverse,
              getinverse = getinverse)
}


## This function computes inverse of the special "matrix" object
## created by makeCacheMatrix(). If the inverse has already been
## calculated, then the cacheSolve() should retrieve it from the
## cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
