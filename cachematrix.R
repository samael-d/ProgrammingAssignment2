## These functions can be used to cache the inverse of a matrix in order
## to avoid repeating time-consuming computations.

## makeCacheMatrix function takes a matrix as an argument and creates
## a list of 4 functions that can be used to set the matrix, get the matrix,
## set the inverse of a matrix and get the inverse.

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(matrix) inverse <<- matrix
        getinverse <- function() inverse
        
        list(get = get, set = set, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve function takes a list created by makeCacheMatrix function as an argument
## and first checks if the inverse has already been computed. If so, it returns it.
## Otherwise, it computes the reverse, caches it and returns it.

cacheSolve <- function(x, ...) {
        inverse <- x$getinverse()
        if (!is.null(inverse)) {
                message('getting cached data')
                return(inverse)
        }
        
        inverse <- solve(x$get(), ...)
        x$setinverse(inverse)
        inverse
}
