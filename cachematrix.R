## those are two functions. The 1st function create an special matrix object,
## and store a list of function to be used in future lines.
## The second function interacts with the first function and receive a object
## of the makeCacheMatrix  class to return the inverse of the matrix that
## was called in the argument of the 1st function.

## create an special object that store 4 functions in a list.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinver <- function(inver) i <<- inver
        getinver <- function() i
        list(set = set, get = get,
             setinver = setinver,
             getinver = getinver)
}


## get the argument of makeCacheMatrix class and return the inverse of a 
## matrix stored in the cache.

cacheSolve <- function(x, ...) {
        i <- x$getinver()
        if(!is.null(i)) {
                message("the inverse has already been calculated")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinver(i)
        ## Return a matrix that is the inverse of 'x'
        i
}