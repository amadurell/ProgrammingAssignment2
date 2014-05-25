## The following two functions optimize computation time by caching 
##  the value of the inverse of a matrix (for a given invertible
##  matrix)

## This function creates a list containing the following functions:
##  1. set (sets the value of its matrix)
##  2. get (retrieves the value of its matrix)
##  3. setsolve (sets the value of its inverse matrix)
##  4. getsolve (retrieves the value of its inverse matrix)
## Note: Notice that the value of its inverse matrix is removed as
##  soon as the matrix changes (whatever value was stored as its
##  inverse matrix is no longer valid, so it is set to NULL).

makeCacheMatrix <- function(x = matrix()) {
    w <- NULL
    set <- function(y) {
        x <<- y
        w <<- NULL        
    }
    get <- function() x
    setsolve <- function(solve) w <<- solve
    getsolve <- function() w
    list("set" = set, 
         "get" = get, 
         "setsolve" = setsolve, 
         "getsolve" = getsolve)
}

## This function returns the value of the inverse matrix of the list
##  object from the previous function ("cached matrix object"), from
##  its cached value if it exists, or calculating the inverse matrix
##  from scratch if it doesn't (and caching its value in the list 
##  object so it is available for later use).

cacheSolve <- function(x) {
        ## Return a matrix that is the inverse of 'x'
    w <- x$getsolve()
    if (!is.null(w)) {
        message("getting cached inverse matrix")
        return(w)
    }
    m <- x$get()
    w <- solve(m)
    x$setsolve(w)
    w
}


## ASSIGNMENT 2 NOTES:
##  Since we assume that the given matrix is always an invertible one
##  and we want to calculate its inverse matrix, I wasn't too sure 
##  about the additional arguments for the solve function.
## The stub function had (x,...) as arguments, whereas the solve
##  function documentation shows (a,b,...) but the b argument needs
##  to be missing in order for the solve function to return the 
##  inverse of the original matrix, so I opted to remove the "..." 
##  from the stub function so as not to risk passing arguments that 
##  wouldn't make sense.
## If I then test:
##      x <- matrix(c(2,2,3,2),2,2)
##      cacheSolve(makeCacheMatrix(x))
## it returns the correct inverse matrix:
##           [,1] [,2]
##      [1,]   -1  1.5
##      [2,]    1 -1.0