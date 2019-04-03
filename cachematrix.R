## Uses caching to speed up solving the inverse of a set of matrices

## Create a structure of cached inverses

makeCacheMatrix <- function(x = matrix()) {
    xr <- NULL
    set <- function(y) {
        x <<- y
        xr <<- NULL
    }
    get <- function() x
    setinv <- function(inv) xr <<- inv
    getinv <- function() xr
    list(set = set, get = get,
    setinv = setinv,
    getinv = getinv)
}


## solve new matrices

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    xr <- x$getinv()
    if(!is.null(xr)) {
        message("getting cached data")
        return(xr)
    }
    data <- x$get()
    xr <- solve(data, ...)
    x$setinv(xr)
    xr
}
