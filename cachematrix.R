## These functions are used to create a special object that stores a matrix
## and cache's its inverse form

## makeCacheMatrix creates a list of functions to work with matrix
## set the value of the matrix
## get the value of the matrix
## set the value of inverse form of the matrix
## get the value of inverse form of the matrix

makeCacheMatrix <- function(x = matrix()) {
        
        i <- NULL
        set <- function(y){
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinvers  <- function(invers) i <<- invers
        getinvers  <- function() i
        
        list(set = set, get = get, setinvers = setinvers, getinvers = getinvers)
}


## CacheSolve calculate the inverse form of the matrix x
## But first this function check whether inverse value has already been calculated

cacheSolve <- function(x, ...) {
        
        i <- x$getinvers()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data,...)
        x$setinvers(i)
        
        i       
}