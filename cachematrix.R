## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function - Create a Cache Matrix to hold cache values of the inverse of a matrix by creating a list of functions to set the matrix, get the matrix, set the inverse and get the inverse.

makeCacheMatrix <- function(mx = matrix()) {
    i <- NULL
    set <- function(y) {
        mx <<- y
        i <<- NULL
    }
    get <- function() mx
    setinv <- function(inv) i <<- inv
    getinv <- function() i
    list(set = set, get = get,
    setinv = setinv,
    getinv = getinv)
}


## Write a short comment describing this function - Code to calculate the inverse of a matrix. if the matrix is not reset, get the cache value. if the matrix is reset, recalculate the inverse and cache the matrix.

cacheSolve <- function(mx, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- mx$getinv()
        if(!is.null(i)) {
            message("getting cached data")
            return(i)
        }
        data <- mx$get()
        i <- solve(data, ...)
        mx$setinv(i)
        i
}
