## Put comments here that give an overall description of what your
## functions do

## So basically we create an inverse of a matrix and store it
# to be retreived later without re-computation

makeCacheMatrix <- function(x = matrix()) {
        nvrs <- NULL
        set <- function(y) {
                x <<- y
                nvrs <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) nvrs <<- inverse
        getInverse <- function() nvrs
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## That's a cache-enhanced matrix inverser :)

cacheSolve <- function(x, ...) {
        nvrs <- x$getInverse()
        if (!is.null(nvrs)) {
                message("getting cached data")
                return(nvrs)
        }
        mtrx <- x$get()
        nvrs <- solve(mtrx, ...)
        x$setInverse(nvrs)
        nvrs
}
