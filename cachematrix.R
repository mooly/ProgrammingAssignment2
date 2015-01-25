## cachematrix.R caches the inverse of a matrix to avoid computing the same 
## invserse matrix repeatedly. First function (makeCacheMatrix) creates an 
## object that can cache the input matrix, retrieve the cached matrix, 
## cache the inverse of the matrix, and retrieve the cached inverse matrix. 
## Second function (cacheSolve) returns inverse of the input matrix by first
## checking the cache. If cache exists, cached value is returned; otherwise, 
## the inverse matrix is computed.

# Acknowledgement: Base code comes from examples provided by Dr. Peng, the
# "makeVector" and "cachemean" functions in Coursera R Programming, available at 
# https://class.coursera.org/rprog-010/human_grading/view/courses/973491/assessments/3/submissions

## makeCacheMatrix creates a "matrix" object that can cache its inverse.
## It contains nested functions that assign and retrieve values for the input
## matrix and its inverse.
    ## Input: 'x', a square invertible matrix
    ## Output: object with functions to store/obtain input matrix & its inverse

makeCacheMatrix <- function(x = matrix()) {
    # Initialize variable to be stored in parent environment:
        m <- NULL
    # set matrix value into parent environment (cache):
        set <- function(y) {
            x <<- y
            m <<- NULL
        }
    # retrieve matrix value:
        get <- function() x
    # set matrix inverse value into parent environment (cache):
        setinv <- function(solve) m <<- solve    
    # retrieve matrix inverse value:
        getinv <- function() m    
    # combine all nested functions into single output object:
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## cacheSolve computes inverse of "matrix" object returned by makeCacheMatrix.
## If the inverse has aleady been calculated, and matrix has not changed, 
## cacheSolve retrieves inverse from cache.
    ## Input: object returned by makeCacheMatrix
    ## Output: inverse matrix (either from cache or newly calculated)

cacheSolve <- function(x, ...) {
    # retrieve inverse matrix value:
        m <- x$getinv()
    # check if inverse already calculated and present in environment; 
    # if yes, cached value is returned:
        if(!is.null(m)) {
            message("getting cached data")
            return(m)
        }
    # else, retrieve matrix value from object created in makeCacheMatrix:
        data <- x$get()
    # solve for inverse matrix value:
        m <- solve(data, ...)
    # place inverse matrix value into parent environment (cache):
        x$setinv(m)
    # Return a matrix that is the inverse of 'x'
        m
}
