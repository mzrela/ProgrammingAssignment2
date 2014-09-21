## This is Programming Assignement #2 of R course on Coursera

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(m=matrix()){

## It creates a special "matrix", which is really a list containing
## a function to
##    set the value of the matrix
##    get the value of the matrix
##    set the inverse of the  matrix
##    get the inverse of the mean
    
    
    i <- NULL
    set <- function(y) {
        m <<- y
        i <<- NULL
    }
    get <- function() m
    setInverse <- function(solve) i <<-solve
    getInverse <- function() i
    
    list(  set = set, get = get,
           setInverse = setInverse,
           getInverse = getInverse)
    
}

## This function computes the inverse of the special "matrix"...
##    returned by makeCacheMatrix function...
##    if the inverse has already been calculated
## NOTE CHECK that the original matrix has not changed)
## if already calculated cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    m <- x$getInverse()
    
    if (!is.null(m)) {
        message("Retrieving cached inversed matrix")
        return(m)
    }
    data <- x$get()
    m <- solve(data,...)
    x$setInverse(m)
    m    #value to be returned by 'cacheSolve'
}

