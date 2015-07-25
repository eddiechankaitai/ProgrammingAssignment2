## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
       inv_m <- NULL
        set <- function(y) {
                x <<- y
                inv_m <<- NULL
        }
        get <- function() x
        setInverseMatrix <- function(inverseMatrix) inv_m <<- inverseMatrix
        getInverseMatrix <- function() inv_m
        list(set = set, get = get,
             setInverseMatrix = setInverseMatrix,
             getInverseMatrix = getInverseMatrix)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		
       inv_m <- x$getInverseMatrix()
        if(!is.null(inv_m)) {
                message("getting cached matrix")
                return(inv_m)
        }
        data <- x$get()
        inv_m <- solve(data, ...)
        x$setInverseMatrix(inv_m)
        inv_m		
}
