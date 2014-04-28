## These funtions provide a method of caching matrices, so their inverse is only
## computed once. This saves resources on an expensive computation.

## This function wraps a matrix into an object that will store a cached version of its inverse.
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function takes a cache matrix and returns its inverse
## If the matrix has already been calculated, it will be stored in the inverse variable,
## so it can be output without being re-computed.
cacheSolve <- function(x) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}
