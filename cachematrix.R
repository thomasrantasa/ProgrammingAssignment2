## cachematrix.R implements a solution to cache the inverse of a matrix
## This solution will save expensive compute time in case of repeated lookups
## of the inverse of a matrix by storing it in an object and only computing
## if necessary

## makeCacheMatrix creates a CacheMatrix object by returning
## a list of functions that allow access and setting of the
## underlying matrix data as well as the inverse of it 
## the inverse is stored locally in the object as a cache

makeCacheMatrix <- function(x = matrix()) {
    
    inverse <- NULL
    set <- function(y){
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setinverse <- function(newinverse) inverse <<- newinverse
    getinverse <- function() inverse
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## The cacheSolve function accepts a matrix as an argument and will return
## the inverse of that matrix. It first checks if an inverse is stored
## and if it finds one, it will return that cached inverse. If no cached
## inverse is found, it will compute, store it in the objects cache and return

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    ##get the inverse stored in x
    inverse <- x$getinverse()
    
    ##check if there was an inverse - if not the value will be NULL
    if(!is.null(inverse)){
        message("getting cached inverse data")
        return(inverse)
    }
    
    ##Otherwise we have to compute the inverse and store in the object
    data <- x$get()
    inverse <- solve(data, ...)
    x$setinverse(inverse)
    inverse
}



