## These two functions cache the inverse of a matrix so that it is
## computed only once, and retrieved from the cache thereafter.

## makeCacheMatrix creates a special "matrix" object: a list of four
## functions that get and set the matrix and its cached inverse.

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

## cacheSolve returns the inverse of the special "matrix" created above.
## If the inverse is already cached it is returned directly; otherwise
## it is computed with solve(), stored in the cache, and returned.

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
