## These are two functions which helps you invert matrix.
## Main difference from original `solve` that `cacheSolve`
## creates a cache of already inverted matrix, so you will
## not spend copmuting time to inverse matrix which already
## have been inverted.

makeCacheMatrix <- function(x = matrix()) {
    # Creates a special 'vector' (list) containing functions
    # to set matrix, get matrix, set inverse matrix and get
    # inverted matrix
    # Args:
    #   x: invertable matrix
    # Returns:
    #   list of functions
    
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

cacheSolve <- function(x, ...) {
    # Inverting matrix created with `makeCacheMatrix` function
    # If matrix haven't been `solved` yet it will inverse it
    # and save `solved` matrix for future use
    # Args:
    #   x: object returned by `makeCacheMatrix`
    #   ...: further arguments passed to `solve` function
    # Returns:
    #   matrix that is the inverse of 'x'
    
    # Trying to get cached data
    m <- x$getinverse()
    if(!is.null(m)) {
        message("Getting cached data")
        return(m)
    }
    # No cache data was found, using standard `solve`
    # and caching it result
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
