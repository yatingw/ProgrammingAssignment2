## This set of functions is used to create a matrix and cache
## the inverse of the matrix.


## "makeCacheMatrix" function creates a matrix object that can
## cache its inverse. This function consists of four sub-functions.
makeCacheMatrix <- function(x = matrix()) {
    
    m <- NULL
    
    ## set() sets the matrix.
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    ## get() returns the matrix which is set.
    get <- function() x
    
    ## setinverse() sets the inverse of the matrix.
    setinverse <- function(inverse) m <<- inverse
    
    ## getinverse() returns the inverse of the matrix which is set.
    getinverse <- function() m
    
    ## Return of the function makeCacheMatrix() returns a list of
    ## the four functions defined above.
    list(set = set,
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## "cacheSolve" function computes the inverse of the matrix returned
## by function "makeCacheMatrix". If the inverse has already been
## defined/calculated in function "makeCacheMatrix", then function
## "cacheSolve" will retrieve inverse from the cache.
cacheSolve <- function(x, ...) {
    
    ## Retrieve the inverse of matrix.
    ## If the object is NOT NULL, then print a message
    ## and return the value.
    m <- x$getinverse()
    if(!is.null(m)){
        message("getting cached data")
        return(m)
    }
    
    ## Otherwise, get the matrix first.
    ## Then use function "solve" to get the inverse
    ## of the matrix retrieved.
    ## At last, set the inverse of the matrix back to
    ## the special "matrix" object created in makeCacheMatrix.
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    
    ## Return of the function "cacheSolve" returns the inverse
    ## of the matrix calculated above.
    m
}
