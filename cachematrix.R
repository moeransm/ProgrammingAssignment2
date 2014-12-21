setwd("~/Box Sync/Coursera/R Programming/Homework")

##This function creates a matrix object that will cache its inverse
makeCacheMatrix <- function(x = matrix()) {
        set <- function(y) {
                x <<- y
        }
        get <- function() x #returns value of original matrix
        setmatrix <- function(inverse) {m<<-inverse}
        getmatrix<-function(){m} #return the cached value to cachematrix()
        list(set = set, get = get,
             setmatrix = setmatrix,
             getmatrix = getmatrix) #list of functions access methods
}
        #This function computes the inverse of the matrix object
        cacheSolve <- function(x, ...) {
                xinv <- NULL
                
                if(!is.null(xinv)) {              # if xinv was already cached (not NULL) ...
                        message("getting cached data")  # ... send this message to the console
                        return(xinv)              # return the cached inverse
                }
                else{
                        ## Return a matrix that is the inverse of 'x'
                        data <- x$get()        # if x$getinverse() returned NULL, execute this part
                        getinverse <- function(x){
                                xinv <<- solve(data)   # if xinv was NULL then we calculate the xinv
                                x$setmatrix(xinv)           # store the calculated mean value in x (see setmean() in makeVector
                                xinv
                        }
                        getinverse()
                        xinv
                }                
        }
        cachemean <- function(x, ...) {   # the input x is an object created by makeVector
                m <- x$getmean()               # accesses the object 'x' and gets the value of the mean
                if(!is.null(m)) {              # if mean was already cached (not NULL) ...
                        
                        message("getting cached data")  # ... send this message to the console
                        return(m)                       # ... and return the mean ... "return" ends 
                }
                data <- x$get()        # we reach this code only if x$getmean() returned NULL
                m <- mean(data, ...)   # if m was NULL then we have to calculate the mean
                x$setmean(m)           # store the calculated mean value in x (see setmean() in makeVector
                m               # return the mean to the code that called this function
        }
        
        makeVector <- function(x = numeric()) {
                m <- NULL
                set <- function(y) {
                        x <<- y
                        m <<- NULL
                }
                get <- function() x
                setmean <- function(mean) m <<- mean(x)
                getmean <- function() m
                list(set = set, get = get,
                     setmean = setmean,
                     getmean = getmean)
        }        
