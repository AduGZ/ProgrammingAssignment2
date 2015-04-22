## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Function: Create a "matrix" object to cache its inverse
makeCacheMatrix <- function(x = matrix()) {
        xt <- NULL
        
        set <- function(m) {
                x <<- m
                xt <<- NULL
        }
        get <- function() x
        
        setInverse <- function(inverse) xt <<- inverse
        getInverse <- function() xt
        
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function
## Function: Compute the inverse of the "matrix" return by makeCacheMatrix above
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        mt <- x$getInverse()
        if(!is.null(mt)) {
                message("getting cached data")
                return(mt)
        }
        
        m <- x$get()
        mt <- solve(m, ...)
        x$setInverse(mt)
        mt
}
