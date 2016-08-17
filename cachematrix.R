

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = mtrx()) {
        inverse1 <- NULL
        
        set <- function(y) {
                x <<- y
                inverse1 <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inverse1 <<- inverse
        getInverse <- function() inverse1
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse1 <- x$getInverse()
        if (!is.null(inverse1)) {
                message("getting cached data")
                return(inverse1)
        }
        mat <- x$get()
        inverse1 <- solve(mat, ...)
        x$setInverse(inverse1)
        inverse1
}
