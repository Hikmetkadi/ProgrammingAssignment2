## These set of functions are created to prevent unnecessary calculations
## that is to invert a matrix.

## This makeCacheMatrix function create an empty matrix which will store our
## set of matrices.

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) inv <<- inverse
	getinverse <- function() inv
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}
## This function evaluates the input and if the input is already inverted,
## it will return data from the cache, if not, it will invert the new matrix.

cacheSolve <- function(x, ...) {
	inv <- x$getinverse()
	if(!is.null(inv)) {
		message("getting cache data")
		return(inv)
	}
	data <- x$get()
	inv <- solve(data, ...)
	x$setinverse(inv)
	inv
}
        ## Return a matrix that is the inverse of 'x'
