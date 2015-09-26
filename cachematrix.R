## this file corresponds to the second programming assignment
## of the coursera course in R Programming
## it contains a pair of functions that cache the inverse of
##a matrix


## This function creates a special "matrix" object
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    invMtrx <- NULL
    set <- function(y) {
        x <<- y
        invMtrx <<- NULL
    }
    get <- function() x
    setInvMtrx <- function(inv) invMtrx <<- inv
    getInvMtrx <- function() invMtrx
    list(set = set, get = get,
         setInvMtrx = setInvMtrx,
         getInvMtrx = getInvMtrx)
    
}


## This function computes the inverse of the special
## "matrix" returned by makeCacheMatrix above.

cacheSolve <- function(x) {
    invMtrx <- x$getInvMtrx()
    if(!is.null(invMtrx)) {
        message("getting cached data")
        return(invMtrx) ## Return a matrix that is the inverse of 'x'
    }
    data <- x$get()
    invMtrx <- solve(data)
    x$setInvMtrx(invMtrx)
    invMtrx ## Return a matrix that is the inverse of 'x'
}
