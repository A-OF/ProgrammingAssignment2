## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix creates a matrix object then can cache its inverse using
## "solve" function

makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    get <- function()x
    setsolve <- function(solve) s <<- solve
    getsolve <- function() s
    list(set = set, 
         get =get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## cacheSolve calculates the inverse of the special matrix created by
## the previous function. If the inverse has already been calculated it retrives
## the inverse matrix from the cache.

cacheSolve <- function(x, ...) {
    s <- x$getsolve()
    if(!is.null(s)){
        message("getting cached data")
        return(s)
    }
    data <- x$get()
    s <- solve(data, ...)
    x$setsolve(s)
    s
        ## Return a matrix that is the inverse of 'x'
}

## Example to test those functions work. Try:

## > my_matrix <-  makeCacheMatrix(matrix(1:4, 2, 2))
## > my_matrix$get()
## > cacheSolve(my_matrix)
