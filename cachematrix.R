## The following two methods are utilized in tandem to cache the inverse of a matrix. 
## This prevents us from needing to compute the inverse of the samme matrix mutliple times.
## This is a method that, when the contents of an invertible matrix do not change, will
## save us valuable time.

## The first fuction is the makeCacheMatrix function. This function creates a list
## that contains a function that (1) sets the value to the matrix, (2) gets the value
## of the matrix, (3) sets the value to the inverse of the matrix, and (4) gets the value
## inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {

	inv <- NULL

	set <- function(y) {
	
		x <<- y
		inv <<- NULL
	}
	get <- function() x

	setInvMat <- function(InvMat) inv <- InvMat
	getInvMat <- function() inv
	
	list(set = set, get = get, setInvMat = setInvMat, getInvMat = getInvMat)
}

## The second fuction, cacheSolve, checks whether the inverse has been computed.
## If this is not the case, it computes the inverse and sets the value in the cache.
## If the inverse hbas been computed, it gets the inverse and skips the calculation.

cacheSolve <- function(x, ...) {

	inv <- x$getInvMat()

	if(!is.null(inv)) {
		
		message("getting cached data.")
		return(inv)
	}

	mat <- x$get()
	inv <- solve(mat)
	x$setInvMat(inv)
	inv
}
