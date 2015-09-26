## These functions will take advantage of lexical scoping by caching 
## the values of potentially time consuming computations so that the next
## time the value is needed, the computation does not need to be repeated
## and the value is readily available.

## This function stores a list of functions that set and retrieve
## the value of a matrix as well as set and retrieve the value of 
## the inverse of that matrix.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function will first check to see if there is a cached value
## for m. If there is a cached value, it will print "getting cached 
## data" and then print m. If m is NULL, the inverse of the matrix 
## will be calculated, set as m, and then printed.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
	m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m	
}
