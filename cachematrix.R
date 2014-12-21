setwd("~/Box Sync/Coursera/R Programming/Homework")
##This function creates a matrix object that will cache its inverse
makeCacheMatrix <- function(x = matrix()) {
        xinv <- NULL
        set <- function(y) {
                x <<- y
                xinv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) xinv <<- inverse
        getinverse <- function() xinv
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}
#If the inverse is set to NULL, this function computes the inverse of the matrix
#Otherwise it returns the cached matrix inverse without calculation
cacheSolve <- function(x, ...) {
        xinv <- x$getinverse()
        if(!is.null(xinv)) {
                message("getting cached data.")
                return(xinv)
        }
        else{
        data <- x$get()
        xinv <- solve(data)
        x$setinverse(xinv)
        xinv
        }
}
