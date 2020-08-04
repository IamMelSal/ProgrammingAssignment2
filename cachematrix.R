## These functions create a special "matrix" object
## that can cache its inverse

## This function creates a matrix and caches its inverse

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
            x <<- y
            i <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
                setinverse = setinvers,
                getinverse = getinverse)

}


## This function computes the inverse of the matrix created.
## If the inverse has been created, this function then retrieves it
## from the cache. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
           message("getting cached data")
           return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
