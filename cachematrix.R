## This set of functions is designed to make a square matrix, 
##solve that matrix, and cache the result.

## This function returns a list containing the matrix in the argument,
## as well as code to cache and print the inverse of the matrix
# supplied by the cacheSolve funtion.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) i <<- solve
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## This matrix checks to see if the list from the previous function conatins a solution to
## the matrix. If it does, it returns that solution. If not, it computes the inverse and
## stores it in the list.

cacheSolve <- function(x, ...) {
        
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
        ## Return a matrix that is the inverse of 'x'
}
