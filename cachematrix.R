## This is Programming Assignement #2 of R course on Coursera

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(m = matrix()){

## This function creates a special "matrix", which is really a four element list containing
## four functions to:
##    set the value of the matrix (set)
##    get the value of the matrix (get)
##    set the inverse of the  matrix (setInverse)
##    get the inverse of the mean (getInverse)
    
    
    i <- NULL
    set <- function(y) {
        m <<- y
        i <<- NULL
    }
    get <- function() m
    setInverse <- function(solve) i <<-solve  ## 'solve' returns the inverse matrix
    getInverse <- function() i                ## cached in the getInverse function
    
    list(  set = set, get = get,
           setInverse = setInverse,
           getInverse = getInverse)
    
    ## this 'list' is actually what this function returns
}

## *******************************************************************


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
    m <- solve(data,...)  ##if not cached, we need to solve it here 
    x$setInverse(m)
    m    #value to be returned by 'cacheSolve'
}

