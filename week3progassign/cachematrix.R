## The following two functions makeCacheMatrix and cacheSolve
## are designed to reduce the costly resource computation of inverting
## a matrix. It does so by caching the inverse of the provided matrix 
## instead of computing the inverse matrix repeatedly. 

## The makeCacheMatrix function creates a matrix object that is cached as the 
## inverse of the original matrix.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    ## Sets the values of the matrix.
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    ## Gets the values of the matrix.
    get <- function() x
    ##Sets the inverse values of the matrix.
    setinverse <- function(inverse) m <<- inverse
    ##Gets the inverse values of the matrix
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## The cacheSolve function computes the inverse of the inverted matrix returned
## by the makeCacheMatrix function above. As long as the inverse matrix
## has already been created and the matrix hasn't been changed, then the
## cacheSolve function should be able to pull the inverted matrix from the 
## cache. It is assumed that the matrix is a square matrix and is able to be
## inverted.

cacheSolve <- function(x, ...) {
    ## Returns a matrix that is the inverse of x.
    m <- x$inverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}


