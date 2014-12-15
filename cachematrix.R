## The two functions makeCacheMatrix and cacheSolve are used to create a matrix, that can
## cache its inverse. The cached value has to be calculated using cacheSolve before it
## can be used. When the matrix is updated, it has to be calculated again using cacheSolve

## This function creates a matrix that can cache its inverse using the cacheSolve function.
## Usage:
## 
## Create the matrix:
## myMatrix <- makeCacheMatrix(matrix(1:4, nrow = 2, ncol = 2))
##
## Get Matrix:
## myMatrix$get()
##
## Set Matrix:
## myMatrix$set(matrix(2:5, nrow = 2, ncol = 2))
## 
## Set the inverse (used by cacheSolve):
## myMatrix$setinverse(m)
##
## Get the cached inverse (cacheSolve has to be called before):
## myMatrix$getinverse()

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    # Set the matrix and NULL the cached value
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    #return the matrix
    get <- function() { x }
    #cache the inverse
    setinverse <- function(inverse) { m <<- inverse }
    #get the cached inverse
    getinverse <- function() { m }
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## This function caches the inverse of a matrix created by makeCacheMatrix
##
## Usage:
##
## Cache the inverse of the matrix created by makeCacheMatrix:
## cacheSolve(myMatrix)

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}