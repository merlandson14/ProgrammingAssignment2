## This repository makes two functions, one to make a cache of the inverse 
## of a matrix and one to solve and cache the inverse. 

## This function makes the external cache matrix to store the inverse. A list
## of commands is returned for external calling.

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL                    ##Creates blank inverse matrix.
	set <- function(y){            ##Function to set inv=null if solving.
		x <<- y
		inv <<- NULL
	}
	get <- function() x            ##Function to grab data.
	setinv <- function(solve) inv <<- solve ##Function to set cache matrix.
	getinv <- function() inv       ##Funtion to retrieve cache matrix.
	list(set=set, get=get, setinv=setinv, getinv=getinv) ##This is returned.
}


## This function grabs the cached inverse matrix of x. If null, it computes
## the inverse. Either way, it returns the inverse matrix.

cacheSolve <- function(x, ...) {
        inv <- x$getinv()              ##Get inverse matrix from cache.
	if(!is.null(inv)){             ##If not null, then return inverse.
		message("retrieving cached data")
		return(inv)
	}
	data <- x$get()                ##If null, get data.
	inv <- solve(data, ...)        ##Solve data.
	x$setinv(inv)                  ##Cache inverse matrix.
	inv                            ##Return inverse matrix.
}
