## These functions allow caching the inverse of a matrix rather 
## than compute it repeatedly. Therefore, when the contents of 
## the matrix are not changing, they will be cached and 
## next time they will be needed, they will be looked up in the cache 
## rather than recomputed.


## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        ## Initialize the variable to store the inverse of the matrix
        im <- NULL
        ## Set the value of the matrix
        setmatrix <- function(y) {
                x <<- y
                im <<- NULL
        }
        ## Get the value of the matrix
        getmatrix <- function() x
        ## Set the value of the inverse of the matrix (store in cache)
        setinvmatrix <- function(invmatrix) im <<- invmatrix
        ## Get the value of the inverse of the matrix (from cache)
        getinvmatrix <- function() im
        ## Return of the function
        list(setmatrix = setmatrix, getmatrix = getmatrix,
             setinvmatrix = setinvmatrix,
             getinvmatrix = getinvmatrix)
}


## This function computes the inverse of the special "matrix" returned by 
## the makeCacheMatrix function. 
## If the inverse of the matrix has already been calculated and the matrix has not changed, 
## then this function retrieves the inverse from the cache. Otherwise the inverse
## will be compute.
cacheSolve <- function(x, ...) {
        ## Check whether the inverse of the matrix already exists in the cache
        im <- x$getinvmatrix()
        if (!is.null(im)) {
                ## Write a message to notify that the inverse is cached
                message("getting cached data")
                ## Return a matrix that is the inverse of 'x' (from the cache)
                return(im)
        }
        ## Compute the inverse of the matrix 'x' because it is not stored in the cache
        ## Get the content of the matrix
        data <- x$getmatrix()
        # compute its inverse
        im <- solve(data)
        ## Store the inverse of the matrix in the cache
        x$setinvmatrix(im)
        ## Return a matrix that is the inverse of 'x' (computed)
        im
}
