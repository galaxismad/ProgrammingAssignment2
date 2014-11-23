## Creating an object of a matrix in the first function and 
## calculating the inverse matrix of this matrix 

## makeCacheMatrix creates a list of methods for calculating the inverse matrix

makeCacheMatrix <- function(x = numeric()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setinverse <- function(matrix) inverse <<- matrix
    getinverse <- function() inverse
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## calculating the inverse matrix and saving it in an object of the list

cacheSolve <- function(x, ...) {
    inverse <- x$getinverse()
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data, ...)
    x$setinverse(inverse)
    inverse
}
