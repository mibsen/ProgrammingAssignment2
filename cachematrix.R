## Pair of functions that cache the inverse of a matrix.

# This function creates a special "matrix" object that can cache its inverse.
#
# Args:
#  x: matrix - the matrix which should be possible to cache
#
# Returns: cacheMatrix a special "vector", which is really a list containing functions
# 
#  set: sets the value of the matrix
#  get: get the value of the matrix
#  setSolve: set the value of the solve
#  getSolve: get the value of the solve

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setSolve <- function(solve) m <<- solve
    getSolve <- function() m
    list(set = set, get = get,
         setSolve = setSolve,
         getSolve = getSolve)
}


# This function computes the inverse of the special "matrix" returned by 
# makeCacheMatrix above. If the inverse has already been calculated 
# (and the matrix has not changed)
#
# Args:
#  cacheMatrix
#  ...
#
# Returns:
#  the inverse metrix of 'x'


cacheSolve <- function(x, ...) {
  
    # retrives solvedMatrix from cacheMatrix
    m <- x$getSolve()
  
    # Validates if the solve cache is set
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    # solve the matrix and cache it in the cacheMatrix
    data <- x$get()
    m <- solve(data, ...)
    x$setSolve(m)
    m    
}
