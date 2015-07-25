## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## function makeCacheMatrix is for storing the invertible matrix and its inverse matrix
## variable inv_m stores inverse matrix
## variable x stores the invertible matrix
## function set is to set the invertible matrix to variable x
## function get is to get the invertible matrix
## function setInverseMatrix is to set the calculated inverse matrix to varible inv_m
## function getInverseMatrix is to get the inverse matrix

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
## function cacheSolve is for calculating inverse matrix using function makeCacheMatrix
## if makeCacheMatrix has stored the inverse matrix (i.e. the inverse matrix has been already calculated), it directly get the inverse matrix using getInverseMatrix function without calculation
## if makeCacheMatrix has not stored, it calculate its inverse matrix using 'solve' function and store it in makeCacheMatrix using setInverseMatrix function

## You may try typing the following command to find the result.
##> x <- matrix(1:4, nrow=2, ncol=2)
##> objCacheMatrixX <- makeCacheMatrix()
##> objCacheMatrixX$set(x)
##> cacheSolve(objCacheMatrixX)
##     [,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5
##> cacheSolve(objCacheMatrixX)
##getting cached matrix
##     [,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5


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
