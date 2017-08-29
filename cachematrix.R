## Put comments here that give an overall description of what your
## functions do

## My first function makes a CacheMatrix i.e. A matrix for which we will check cached results
## Second function checks if result already exists or if we have to compute it again


## Write a short comment describing this function
## This function is declaring all the required variables and functions in order to get our matrix 
makeCacheMatrix <- function(x = matrix()) {
	inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(mean) inverse <<- mean
        getinverse <- function() inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function
## This function will first get the inverse and will check if it exists
## If it does, It will simply return the inverse
## Else it will compute the inverse, store it and will return it
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data) %*% data
        x$setinverse(m)
        m
}
