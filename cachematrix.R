## Functions to cache the inverse of a matrix 

## This function takes a matrix 'x' as argument and creates an R object which is
## capable of storing the matrix and the corresponding inverse. 
## It provides getter and setter functions to manipulate the "cache" matrix.

makeCacheMatrix <- function(x = matrix()) {
    invrs <- NULL
    set <- function(y){
        x <<- y
        invrs <<- NULL
    }
    get <- function() x
    setinverse <- function(inv) invrs <<- inv
    getinverse <- function() invrs
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## This function takes at least a matrix 'x' and optional additional arguments
## that must be applicable for the original 'solve' function. It returns the 
## inverse of 'x' either from cache if available or calculates it and puts it
## into the cache. Note: Optional arguments '...' that might change the
## result are not reevaluated once it was put into the cache. 

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    invrs <- x$getinverse()
    if(!is.null(invrs)){
        message("getting cached inverse")
        return(invrs)
    }
    # get the matrix for which the inverse should be calculated
    data <- x$get()
    # Calculate inverse of x
    invrs <- solve(data, ...)
    # Store the result in the special matrix
    x$setinverse(invrs)
    # return the result
    invrs
}