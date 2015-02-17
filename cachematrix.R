## makeCacheMatrix and cacheSolve provide a way to cache the invserve of a Matrix. 
## Matrix inversion is considered to be a costly operation and hence cacheing the same 
## helps in saving CPU and time. 

## makeCacheMatrix function creates a special "matrix" object (from a matrix object passed)
## that can cache its inverse. 

makeCacheMatrix <- function(x = matrix()) {
    inverseOfX = NULL
    set <- function(y) {
        x <<- y
        inverseOfX <<- NULL
    }
    get <- function() x
    setSolve <- function(solve) inverseOfX <<- solve
    getSolve <- function() inverseOfX
    list (set = set, get = get,
          setSolve = setSolve, 
          getSolve = getSolve)
}


## cacheSolve function returns the inverse of the special matrix created by makeCacheMatrix function. 
## if the inverse is already available in the cache it will return the cached value else will calcuate the 
## inverse and cache it before returning. 

cacheSolve <- function(x, ...) {
    inverseOfX <- x$getSolve()
    if (!is.null(inverseOfX)) {
        message("getting cached data")
        return(inverseOfX)
    }
    data = x$get()
    inverseOfX = solve(data)
    x$setSolve(inverseOfX)
    inverseOfX
}
