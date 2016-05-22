## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## The below function will create a matrix object which is invertible, i.e. a non singular matrix.

makeCacheMatrix <- function(x = matrix()) {
    invmatrix <- NULL
    set <- function(y) {
        x <<- y
        invmatrix <<- NULL
    }
    get <- function() x
    setinv <- function(inv) invmatrix <<- inv
    getinv <- function() invmatrix
    list(set = set, get = get,
    setinv = setinv,
    getinv = getinv)

}


## Write a short comment describing this function
## the below function will find the inverse of the matrix denoted by 'x' , if it is already calculated once it will return the inverse from cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invmatrix <- x$getinv()
        if(!is.null(invmatrix)) {
            message("getting cached data")
            return(invmatrix)
        }
        data <- x$get()
        invmatrix <- solve(data, ...)
        x$setinv(invmatrix)
        invmatrix
}
