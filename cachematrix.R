## Create the inverse of a matrix, but store the result in an internal cache
## This cache is then used for subsequent calls to the  


## Create matrix object that has methods for 
##  set the matrix value
##  get the matrix value
##  set the inverse of the matrix
##  get the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setInverse <- function(inv) m <<- inv
    getInverse <- function() m
    list(set = set, 
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Invert the matrix x, and store it's inverse
## Return the version from the cache if already present
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setInverse(m)
    m
}
