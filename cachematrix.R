
##-------- Coursera R Programming Assignment 2 ------------

## These two functions below allow you to cache the inverse of a matrix
## The underlying assumption is that the input matrix is always 
## a square matrix and invertible. This set of functions will not 
## work with matrices that are not square and non-invertible

## To get things working, first create a matrix, pass it to makeCacheMatrix.
## the result of makeCacheMatrix should then be passed on to cacheSolve
## in order to compute the inverse. The first time you call cacheSolve,
## the inverse is calculated and cached. Any subsequent calls will 
## then access the cache to return the inverse. 

## Example code
## > inputMatrix <- matrix(c(1:4), 2, 2)
## > matrixFunctionList <- makeCacheMatrix(inputMatrix)
## > cacheSolve(matrixFunctionList)

##---------------------------------------------------------
## This function takes an invertible matrix and returns a list of 
## functions that allow you to set and get the value of the matrix, 
## and set and get the inverse of the matrix
##---------------------------------------------------------

makeCacheMatrix <- function(x = numeric.matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

##---------------------------------------------------------
## This function takes the resulting list from the function call 
## above. If an inverse was already calulated for the matrix, it is
## returned from cache. Otherwise the inverse is calculated and 
## the cache is set using setinverse function, and the inverse is
## also returned.
##---------------------------------------------------------

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if (!is.null(inv)) {
        message("getting cached matrix")
        return(inv)
    }
    
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}
