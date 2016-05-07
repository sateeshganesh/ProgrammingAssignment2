## Compute the inverse of a matrix and cache it for reuse.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    invMat <- NULL
    setM <- function(maty) {
        x <<- maty
        invMat <<- NULL
    }
    getM <- function() x
    setInvMat <- function(InvMatrix) invMat <<- InvMatrix
    getInvMat <- function() invMat
    list(setM = setM, getM = getM,
         setInvMat = setInvMat,
         getInvMat = getInvMat)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), 
##then cacheSolve will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    invMat <- x$getInvMat()
    if(!is.null(invMat)) {
        message("Getting cached Matrix data")
        return(invMat)
    }
    invMatdata <- x$getM()
    invMat <- solve(invMatdata, ...)
    x$setInvMat(invMat)
    invMat
}
