## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##	The first function, makeCacheMatrix creates "vector", which is really a list containing a function to
##	setmatrix -> set the value of the matrix
##	getmatrix -> get the value of the matrix
##	setinverse-> set the inverse of the matrix
##	getinverse-> get the inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {
  
	inverse <- NULL 
	y <- NULL
	setmatrix <- function(y) {  
		x <<- y 
	inverse <<- NULL
	}
	getmatrix <- function(){
		x
	}
	setinverse <- function (solve){
		inverse <<- solve
	}
	getinverse <- function (){
		inverse
	}

	list(setmatrix = setmatrix,
	   getmatrix = getmatrix,
	   setinverse = setinverse,
	   getinverse = getinverse)
}


## Write a short comment describing this function
## This function calculates the inverse of the input "matrix". If the inverse has already been calculated (and the matrix has not changed), then the cachesolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
	inverse <- x$getinverse()

	if(!is.null(inverse)){ 
	  return(inverse)
	}

	inverse <- solve(x$getmatrix(), ...) 

	x$setinverse(inverse) 

	inverse 
    ## Return a matrix that is the inverse of 'x'
}
