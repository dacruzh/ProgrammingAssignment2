## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    
    ## Inverse property with NULL as default value
    inv <- NULL
    
    ## Nested method to set the matrix
    set <- function(userMatrix){
        x <<- userMatrix
        inv <<- NULL
    }
    
    ## Nested method to get the matrix
    get <- function() x
    
    ## Nested method to set the inverse of the matrix
    setInverse <- function(inverse) inv <<- inverse
    
    ## Nested method to get the inverse of the matrix
    getInverse <- function()inv
    
    ## return list of methods
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then the cachesolve should retrieve the inverse from
## the cache.

cacheSolve <- function(x, ...) {
    ## gets the value of the inverse of x, returning the NULL if we have not
    ## calculated the inverse yet
    inverseMatrix <- x$getInverse()
    
    ## if inverseMatrix is not null, we can return the inverse and skip
    ## unnecessary calculations
    if(!is.null(inverseMatrix)){
        message("getting cached data")
        return(inverseMatrix)
    }
    
    ## this method returns the matrix from our object x
    data <- x$get()
    
    ## solve method solv(a,b,...)
    ## This generic function solves the equation a %*% x = b for x, where b can
    ## be either a vector or a matrix. If b is missing is taken as the identity
    ## matrix
    inverseMatrix <- solve(data)
    
    ## set the inverse to x
    x$setInverse(inverseMatrix)
    
    ## return the inverse matrix
    inverseMatrix
}
