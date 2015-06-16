## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This functions creates a special matrix that will be used for caching the 
## inverse matrix

makeCacheMatrix <- function(x = matrix()){
	m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
	setinverse <- function(solve) m <<- solve
        getinverse <- function() m

	list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## Write a short comment describing this function
## Calculates and returns the inverse matrix from the special matrix
## created with the function makeCacheMatrix

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}