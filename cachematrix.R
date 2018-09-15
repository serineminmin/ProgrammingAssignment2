## Put comments here that give an overall description of what your
## functions do
## Matrix inversion can be very costly to compute repeatedly therefore there are obvious benefits to caching the inverse of a matrix. The following two functions are written to cache the inverse of a matrix.

## Write a short comment describing this function
## makeCacheMatrix is a function to create an object that can cache the 
## inverse of matrix
## Output will be a list of function to set the value of matrix, get the value of matrix, set the value of inverse and get the value of inverse
makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get<-function() x
	setinverse <- function(solve) inv <<- solve
	getinverse <- function() inv
	list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Write a short comment describing this function
## cacheSolve will retrieve from cache if inverse has been calculated
## otherwise it will calculate the inverse of matrix and set the value in cache via 
## setinverse function
## This function will first check to see if the inverse of matrix is already in cache, if yes it will retrieve and skip computation, if else, it will compute the inverse of the matrix and set to cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inv <- x$getinverse()
	if(!is.null(inv)){
		message ("getting cached data")
		return(inv)
	}
	mat <- x$get()
	inv <- solve(mat, ...)
	x$setinverse(inv)
	inv

}
