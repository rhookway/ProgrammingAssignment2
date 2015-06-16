## Pair of functions to cache matrix inverse

## Create a "CacheMatrix" with methods set, get, setinverse, getinverse.
## set and get are used to update and retrieve the value of the "CacheMatrix".
## Use cacheSolve to get the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
    m.inverse <- NULL
    set <- function(y) {
        x <<- y
        m.inverse <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m.inverse <<- inverse
    getinverse <- function() m.inverse
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Returns to inverse of the "CacheMatrix" which is its first parameter. This
## value must have been created using makeCacheMatrix function above. Returns the
## cached value of the inverse if it exists. Otherwise, computes the inverse, caches it
## and returns the cached value.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inverse <- x$getinverse()
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data)
    x$setinverse(inverse)
    inverse
}
