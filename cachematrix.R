## The function cache the value of the inverse matrix
## so that when we need it again, it can be looked up in the cache rather 
## than recomputed. 

## makeCacheMatrix() builds a set of functions and returns 
## the functions within a list to the parent environment
## When we create object myMatrix_object <- makeCacheMatrix(m1)
## this object contains four functions: set(), get(), setsolve(), and getsolve().
## It also includes the two data objects, x and m.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## cacheSolve() is required to retrieve the inverse matrix from an object of 
## type makeCacheVector(). The function attempts to retrieve 
## an inverse matrix from the object passed in as the argument.
## If the result of !is.null(m) is TRUE, we have a 
## cached inverse matrix and can return it to the parent environment.
## If the result of !is.null(m) is FALSE, cacheSolve() gets the vector from 
##the input object, calculates a solve(), uses the setsolve() function on 
## the input object to set the iverse matrix in the input object, 
## and then returns the value of the iverse matrix to the parent environment 
## by printing the inverse matrix object.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}
