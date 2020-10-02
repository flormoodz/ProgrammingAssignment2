##Programming Assignment 2: Lexical Scoping

## This R file contains a pair of functions that cache the inverse
## of a matrix.

## The first function below creates a special "matrix" object that 
## can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	inverse <- NULL
	
	##set the value of the matrix
	set <- function(y) {
                x <<- y
                inverse <<- NULL
      }
	
	##get the value of the matrix
      get <- function() x
	
	##set the value of the inverse of the matrix
      setinverse <- function(inv) inverse <<- inv	
	
	##get the value of the inverse of the matrix
	getinverse <- function() inverse
	list(set = set, get = get,
		setinverse = setinverse,
            getinverse = getinverse)
}


## The second function below computes the inverse of the special 
## "matrix" returned by the first function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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
