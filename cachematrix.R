## Assignment: Caching the Inverse of a Matrix

# makeCacheMatrix is a function that returns a list of functions
# Contains the following functions:
# 1. setMatrix      set the value of a matrix - take an argument
# 2. getMatrix      get the value of a matrix - doesn't take an argument
# 3. setInverse     set the cahced value (inverse of the matrix) - take an argument
# 4. getInverse     get the cahced value (inverse of the matrix) - doesn't take an argument

makeCacheMatrix <- function(x = matrix()) {
        # holds the cached value or NULL if nothing is cached
        # initially nothing is cached so set it to NULL
        m <- NULL
        # store a matrix
        setMatrix <- function(y) {
                x <<- y
                m <<- NULL
        }
        # returns the stored matrix
        getMatrix <- function() {
                x
        }
        # cache the given argument 
        setInverse <- function(solve) {
                m <<- solve
        }
        # get the cached value
        getInverse <- function() {
                m
        }
        # return a list of functions
        list(setMatrix = setMatrix, getMatrix = getMatrix,
             setInverse = setInverse,
             getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        # get the cached value
        m <- x$getInverse()
        # if a cached value exists return it
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        # otherwise get the matrix, caclulate the inverse and store it in
        # the cache
        data <- x$getMatrix()
        m <- solve(data)
        x$setInverse(m)
        
        # return the inverse of matrix
        m
}