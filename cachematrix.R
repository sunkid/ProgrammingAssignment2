## This script defines two functions to return the inverse
## of a square matrix. The inverse matrix is either returned
## from cache if it has been previously calculated and the 
## given matrix has not changed, or it is computed and cached. 
## 
## Use as follows:
## x <- makeCacheMatrix(matrix(1:9,3,3))
## y <- cacheSolve(x) # calculates and caches the inverse
## cacheSolve(x) # retrieves the inverse from cache

## makeCacheMatrix generates a list containing several functions
## that can be used to store or retrieve the cached inverse of
## a given matrix.

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL # initialize i
	
	# below are the set, get, setinv, and getinv functions
	set <- function(y) {
		x <<- y 
		i <<- NULL
	}
	get <- function() x
	setinv <- function(inv) i <<- inv
	getinv <- function() i
	list(set = set, get = get,
		 setinv = setinv,
		 getinv = getinv)
}


## cacheSolve returns the inverse of a matrix by first checking
## if it has previously been computed and returning it from
## cache, if that is the case. Alternatively, it calculates
## the inverse using the solve() function, stores it in the
## cache and returns it.

cacheSolve <- function(x, ...) {
	i <- x$getinv() # get i from cache
	if (!is.null(i)) { # if it was in the cache, simply return it
		message("getting inverse from cache")
		return(i)
	}
	y <- x$get() # get x
	i <- solve(y, ...) # we are assuming the matrix is invertible
	x$setinv(i) # set the inverse in the cache
	return(i) # return the inverted value
}
