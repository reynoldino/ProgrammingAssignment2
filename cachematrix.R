## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates a special "matrix" object that can cache its inverse
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

## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix above 
## It first checks to see if the inverse has already been calculated (and not changed). 
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse  of the data and sets the value of the inverse in the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse() ## Check if inverse has already been calculated
        if(!is.null(m)) { ## Inverse found
                message("Getting cached data")
                return(m) ## ready, return m
        }
        data <- x$get() ## Get data from x
        m <- solve(data, ...) ## Compute the inverse
        x$setinverse(m) ## Set the inverse for the following time
        m ## Return the value of m
}
