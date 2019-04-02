## Put comments here that give an overall description of what your
## functions do

## WThis function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(a) {
        x <<- a
        inverse <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse2) inverse <<- inverse2
    getinverse <- function() inverse
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

##This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache
## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
    inverse <- x$getinverse()
    if(!is.null(inverse)) {
        message("hope this works")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data)
    x$setinverse(inverse)
    inverse
}
