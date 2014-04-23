## Put comments here that give an overall description of what your
## functions do

##
## makeCacheMatrix creates a special object using closures as internal data and defined functions as methods.
## Result is an object which can store value of type matrix and its inversed form.
## NOTE: Its better to calculate and cache in the getSolve function since this value depends only from
## the matrix and must not be set from outside.
##
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    getSolve <- function(...) {
        if(is.null(x)){
            NULL
        } else if (is.null(inv)) {
            inv <<- solve(x, ...)   
            inv
        } else {
            inv
        }
    }
    list(set = set, get = get,
         getSolve = getSolve)
}


## Return cached inverse or recalculate if needed passing ... to solve function

cacheSolve <- function(x, ...) {
    x$getSolve(...)
}
