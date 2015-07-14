## Computing the inverse of a matrix is usually a costly operation. 
## The following functions help to avoid computing the inverse repeatedly
## by caching the result and reusing it.

## makeCacheMatrix constructs a special matrix object. It allows to:
## 1. set the matrix
## 2. get the matrix
## 3. set the inverse of the matrix
## 4. get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    
    # set the value of the matrix
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    
    # get the value of the matrix
    get <- function() x
    
    # set the value of the inverse of the matrix
    setInverse <- function(inv) {
        inverse <<- inv
    }
    
    # get the value of the inverse of the matrix
    getInverse <- function() inverse
    
    # return the list of functions
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## cacheSolve returns the inverse of a given matrix object created with the 
## function makeCacheMatrix. If the inverse has already been calculated, the
## function retrives its value from the cache and skips the computation.
## Otherwise, it computes the inverse and writes its value in the cache using
## the setInverse() function.

cacheSolve <- function(x, ...) {
    # get the inverse of x
    inverse <- x$getInverse()
    
    # return the inverse if it has already been computed
    if(!is.null(inverse)) {
        message("Getting cached data...")
        return(inverse)
    }
    
    # compute the inverse using solve()
    matrix <- x$get()
    inverse <- solve(matrix, ...)
    
    # write the inverse in the cache
    x$setInverse(inverse)
    inverse
}
