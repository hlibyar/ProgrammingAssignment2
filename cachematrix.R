## To caching the inverse of a matrix may help to get computation to finish
## faster, better than compute it repeatedly. 

## This function is to create a list containing a function to set, get the value
## of the matrix, set and get value of inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## This function returns the inverse of the matrix.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        ## Return a matrix that is the inverse of 'x'
        inv <- solve(data)
        x$setinverse(inv)
        inv
}
