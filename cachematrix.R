## Matrix inversion is usually a costly computation and there may be some
## benefit to caching the inverse of a matrix rather than compute it repeatedly 
## The following functions cache the inverse of a matrix.


## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    ix <- NULL
    
    set <- function(y){
        x <<- y
        ix <<- NULL
    }
    
    get <- function() x
    setInverse <- function(inversed) ix <<- inversed
    getInverse <- function() ix
    
    list(set = set, get = get, 
        setInverse = setInverse, 
        getInverse = getInverse)

}


## This function computes the inverse of the special "matrix" returned
## by makeCacheMatrix. 
## If the inverse has already been calculated it returns it from the cache.

cacheSolve <- function(x, ...) {
        
    ix <- x$getInverse()
    if(!is.null(ix)) {
        message("getting cached data")
        return(ix)
    }
    
    data <- x$get()
    ix <- solve(data, ...)
    x$setInverse(ix)
    ix
}
