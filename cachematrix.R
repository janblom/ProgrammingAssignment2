## This is an implementation of a matrix together with its cached inverse

## makeCacheMatrix creates a matrix "object" capable of caching its inverse

makeCacheMatrix <- function(x = matrix()) {
    cached <- NULL
    
    ## create getter and setter functions for the matrix and its cached inverse
    set <- function(y) {
        x <<- y
        cached <<- NULL
    }
    get <- function() x
    setInverse <- function(mean) cached <<- mean
    getInverse <- function() cached
    
    ## return the methods for this "object"
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## cacheSolve returns the inverse of a cachematrix object,
## using a cached result of the inverse has been computed already

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    ## if there is a cached inverse, return it
    cached <- x$getInverse()
    if (!is.null(cached)) {
        return(cached)
    }
    
    ## compute the inverse, cache it and return it
    data <- x$get()
    cached <- solve(data, ...)
    x$setInverse(cached)
    cached
}
