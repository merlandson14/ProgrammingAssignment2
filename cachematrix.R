## This repository makes two functions, one to make a cache of the inverse 
## of a matrix and one to solve and cache the inverse. 

## This function makes the external cache matrix to store the inverse. A list
## of commands is returned for external calling.

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL                    ##Creates blank inverse matrix.
	set <- function(y){            ##Function to set matrix x to the
		x <<- y                ##   incoming matrix and
		inv <<- NULL           ##   initializes blank inverse matrix.
	}
	get <- function() x            ##Function to grab stored matrix x.
	setinv <- function(solve) inv <<- solve ##Function to store inverse.
	getinv <- function() inv                ##Function to retrieve inverse.
	list(set=set, get=get, setinv=setinv, getinv=getinv) ##Returns commands.
}


## This function grabs the cached inverse matrix of x. If null, it computes
## the inverse. Either way, it returns the inverse matrix.

cacheSolve <- function(x, ...) {
        inv <- x$getinv()              ##Get inverse matrix from cache.
	if(!is.null(inv)){             ##If not null, then...
		message("retrieving cached data")  ##Prints this message,
		return(inv)            ##Returns inverse and exits function.
	}
	data <- x$get()                ##If null, get data.
	inv <- solve(data, ...)        ##Solve data for its inverse.
	x$setinv(inv)                  ##Cache inverse matrix.
	inv                            ##Return inverse matrix.
}
