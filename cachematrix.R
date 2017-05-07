## Put comments here that give an overall description of what your functions do
## Caching the inverse of a Matrix:
## These functions introduce a special matrix object which caches its inverse.
## The create such a special matrix m to execute \code{m <- makeCacheMatrix(x)}
## where x is an ordinary matrix. The value can be returned with \code{m$get()}
## and the value can be changed with \code{m$set(y)} where y is an ordinary matrix. 
## Write a short comment describing this function
## Matrix inversion is usually a costly computation and there may be some benefit 
## in caching the inverse of a matrix rather than compute it repeatedly. 
## This function creates a special “matrix” object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        # Define function to set the value of the matrix. 
        # inverse from the cache
        set <- function(y) {
                x <<- y    # Set the value
                m <<- NULL # Clear the cache
        }
        # Define function to get the value of the matrix
        get <- function() x
        # Define function to set the inverse. 
        setInverse <- function(inverse) m <<- inverse
        # Define function to get the inverse
        getInverse <- function() m
        
        # Return a list with the above four functions
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}
        

## Write a short comment describing this function 
## This function computes the inverse of the special “matrix” created by makeCacheMatrix. 
## If the inverse has already been calculated without making any change in the matrix, it 
## should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
}
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
        }
