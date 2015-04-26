## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function


##COMMENTS:
## makeCatcheMartix creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Write a short comment describing this function

##COMMENTS:
##catchSolve computes the inverse of the matrix returned by makeCacheMatrix. 
##If the inverse has already been calculated (and the matrix has not changed), 
##then function will retrieve the inversed matrix from the cache.
cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("Getting cached data, sir")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data) 
    x$setinverse(inv)
    inv
}