## This functions calculates the inverse of the matrix
## If the inverse was already calculated once, it reports the results
## from cache instead of performing the inverse again on the matrix

## Perform Smart Inverse on a Matrix

makeCacheMatrix <- function(x = matrix()) {
        inv  <- NULL
        set  <- function(y){
                x <<- y
                inv <<- NULL 
        }
        get  <- function() x
        setinverse  <- function(inverse) inv  <<- inverse
        getinverse  <- function() inv
        list(set= set, get = get, 
             setinverse = setinverse, 
             getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv  <- x$getinverse()
        if (!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        data  <- x$get()
        inv  <- solve(data, ...) ## inverses the matrix
        x$setinverse(inv)
        inv

}
