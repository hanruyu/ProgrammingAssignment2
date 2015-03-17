## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates a special "matrix" object that can cache its inverse.
## x is the input matrix, defaulf value is matrix().
## set() can change the input matrix.
## get() can get the input matrix.
## setinv() can cache the inverse.
## getinv() can get the cached inversion. It will return NULL if there is no cached inversion.

makeCacheMatrix <- function(x = matrix()) {
  	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setinv <- function(inversion) inv <<- inversion
	getinv <- function() inv
	list(set = set, get = get,
 	     setinv = setinv,
             getinv = getinv)
}


## Write a short comment describing this function
##  This function computes the inverse of the special
##  "matrix" returned by `makeCacheMatrix` above. If the inverse has
##  already been calculated (and the matrix has not changed), then
##  `cacheSolve` should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inv <- x$getinv()
	if(!is.null(inv)) {
		message("getting cached data")
		return(inv)
	}
	data <- x$get()
	inv <- solve(data, ...)
	x$setinv(inv)
	inv
}
