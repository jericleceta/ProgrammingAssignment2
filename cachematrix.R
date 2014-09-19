## I modeled these functions after the example of caching the mean of a vector
## and only modified the appropriate parts needed

## this function is similar to the one in the instructions where it produces
## a list of four functions similar to the one in the instructions
makeCacheMatrix <- function(x = matrix()) {
    ## creating a new special matrix clears the cached inverse
    inv <- NULL
    
    ## create a function to set the values of the matrix from a regular matrix y
    set <- function(y) {
        x <<- y
        
        ## the following line ensures that if the matrix is changed, the cached
        ## inverse will be cleared
        inv <<- NULL
    }
    
    ## get the values of the special matrix
    get <- function() {
        x
    }
    
    ## set the value of the cached inverse of the special matrix
    setinv <- function(inverse) {
        inv <<- inverse
    }
    
    ## get the cached inverse of the special matrix
    getinv <- function() {
        inv
    }
    
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## this function is similar to the one in the instructions where it first checks
## if the inverse is available in the cache and computes for it otherwise
cacheSolve <- function(x, ...) {
    ## check if the cached inverse is available
    inv <- x$getinv()
    if (!is.null(inv)) {
        message ("getting cached data")
        return (inv)
    }
    
    ## if not, compute and cache it
    data <- x$get()
    inv <- solve(data)
    x$setinv(inv)
    inv
}
