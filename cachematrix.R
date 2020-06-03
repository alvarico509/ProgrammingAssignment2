## These two functions are used to create a matrix and calculate its inversed matrix.
## If it has already been calculated, it skips the calculation and reads it from the cache.



## This function creates a matrix, which is really a function containing a function to:
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse matrix
## get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        im <- NULL
        set <- function(y) {
                x <<- y
                im <<- NULL
        }
        get <- function() x
        setim <- function(inversedmatrix) im <<- inversedmatrix
        getim <- function() im
        list(set = set, get = get,
             setim = setim,
             getim = getim)

}


## Write a short comment describing this function
## The following function calculates the inverse matrix of the special "matrix" created with 
## the above function. However, it first checks to see if the inverse matrix has already been calculated. 
## If so, it gets the inverse matrix from the cache and skips the computation. Otherwise, it calculates 
## the inverse matrix of the data and sets the value of the inverse matrix in the cache via the setim function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        im <- x$getim()
        if(!is.null(im)) {
                message("getting cached data")
                return(im)
        }
        data <- x$get()
        im <- solve(data, ...)
        x$setim(im)
        im
}

## You can check how it works with the following two lines (As an example)

matrix_1 <- makeCacheMatrix(matrix(c(-1, 1, 3/2, -1), nrow = 2, ncol = 2))
cacheSolve(matrix_1)


