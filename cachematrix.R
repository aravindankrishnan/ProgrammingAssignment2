## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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


## Write a short comment describing this function

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
