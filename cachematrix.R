##Coursera Assignement: Lexical Scoping
## This project aims to exploit cache features to prevent
## recomputing the inverse of a matrix if this was already computed. 

## Read the matrix and create its cache version

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL ## set the inverse to NULL
                }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv   
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Compute the inverse of the cached matrix. If already computed
## It just returns the previous solution 


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if (!is.null(inv)) {
                 message("using cached data")
                return(inv)
                }  
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
}
