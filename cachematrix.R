################################################################################
## makeCacheMatrix: This function takes an invertible matrix, from which it 
## creates a special "matrix" object that can cache its inverse.
################################################################################ 
makeCacheMatrix <- function(x = matrix()) {
    xInv <- NULL
    set <- function(y) {
        x <<- y
        xInv <<- NULL
    }
    get <- function() x 
    setInverse <- function(solve)  xInv <<- solve 
    getInverse <- function() xInv 
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

################################################################################
## cacheSolve: This function computes the inverse of the special "matrix" object
## returned by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then cacheSolve returns the cached inverted
## matrix. #####################################################################
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    xInv <- x$getInv()
    if(!is.null(xInv)) {
        message("getting cached data")
        return(xInv)
    }
    data <- x$get()
    xInv <- solve(data, ...)
    x$setInv(xInv)
    xInv
}
