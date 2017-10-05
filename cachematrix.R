## This function allows you to input a square matrix and calculate it's inverse
## while cache the result so that the same matrix does not have to be calculated
## a second time.

## This function creates 2 objects (x, sol) and 4 functions (set, get, setsol, getsol)
## that are then made avaialble to the global environment.

makeCacheMatrix <- function(x = matrix()) {
        sol <- NULL
        set <- function(y) {
                x <<- y
                sol <<- NULL
        }
        get <- function()x
        setsol <- function(solve) sol <<- solve
        getsol <- function() sol
        list(set = set, get = get,
             setsol = setsol,
             getsol = getsol)
}


## This function uses the objects created by makeCacheMatrix and, using the new
## function, solve, calculates the inverse of the matrix that was input as an argument
## in makeCacheMatrix, but only if the inverse hasn't already been calculated in the 
## desiganted environment.

cacheSolve <- function(x, ...) {
        sol <- x$getsol()
        if(!is.null(sol)) {
                message("getting cached data")
                return(sol)
        }
        data <- x$get()
        sol <- solve(data,...)
        x$setsol(sol)
        sol
}
