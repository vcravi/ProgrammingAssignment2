## The functions here are used to cache the inverse of a matrix for future 
## access to avoid repeated computation 

## makeCacheMatrix creates a list object used to store the matrix and cache its
## inverse. The inverse is automatically invalidated when the matrix changes, so
## that it is recomputed everytime the matrix changes.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve returns the inverse of a matrix from a CacheMatrix object; if the 
## inverse has already been computed and stored, that is returned, else the 
## inverse is computed and also stored in the CacheMatrix object 

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solvecacheGenericsMetaData(data, ...)
    x$setinverse(m)
    m
}
