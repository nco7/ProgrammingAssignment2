## The two functions below take advantage of lexical scoping to calculate and 
## store the inverse of matrix.

## makeCacheMatrix creates a special matrix that builds a set of functions
## within a list and also stores two data objects: matrix, x, and inverse, i.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get, 
             setinverse = setinverse, 
             getinverse = getinverse)
}


## cacheSolve populates the inverse matrix of x and retrieves the inverse 
## matrix from object type makeCacheMatrix().

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
