## These two functions together allow to calculate the inverse of an 
## input matrix, cache it, and retrieve it if the input had not been 
## changed or recalculate if it had been.

## The makeCacheMatrix function creates a list of functions that set
## and get a matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
	s <- NULL
	set <- function(y) {
		x <<- y
		s <<- NULL		
	}
	get <- function() x
	setsolve <- function(solve) s <<- solve
	getsolve <- function() s
	list(set = set, get = get, setsolve = setsolve,
	getsolve = getsolve)
}


## The cacheSolve function calculates and caches the inverse of the
## matrix stored in the environment of the makeCacheMatrix or 
## retrieves it from cache if a valid inverse is already there

cacheSolve <- function(x, ...) {
	s <- x$getsolve()
	if(!is.null(s)) {
		message("getting cached data")
		return(s)
	}
	data <- x$get()
	s <- solve(data, ...)
	x$setsolve(s)
	s  
}