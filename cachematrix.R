## Put comments here that give an overall description of what your
## functions do
## These 2 functions create a special "matrix" object that can cache and compute its inverse.

## Write a short comment describing this function
## This functions creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL                           ## inistializ the inverse
        set <- function(y) {                      ## define set function
                x <<- y 
                inverse <<- NULL                  ## reset inverse to NULL when there is a new function

}
        get <- function() x
        setinverse <- function(inverse1) inverse <<- inverse1
        getinverse <- function(inverse)
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
         

## Write a short comment describing this function
## This function calculates the mean of the special "vector" created with the above function. 
## However, it first checks to see if the mean has already been calculated. If so, 
## it gets the mean from the cache and skips the computation. 
## Otherwise, it calculates the mean of the data and sets the value of the mean in the cache via the setmean function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getinverse()
        if(!is.null(inverse)) {
                message('getting cached data')
                return(inverse)
}
        data <- x$get()
        inverse <- solve(data, ...)
        x$setinverse(inverse)
        inverse
}
