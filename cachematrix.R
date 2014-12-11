## These functions allow the user to create a special kind of matrix
## that can cache its own calculate and cache its own inverse. 
## This ensures the inverse only needs to be calculated once and 
## is then accessed from the cache.

## makeCacheMatrix function stores the special matrix.  It creates a list containing
## functions that can set and get the actual matrix aswell as set and get 
## the inverse

makeCacheMatrix <- function(x = matrix()) {

		m<-NULL
		 
		set <- function(y) {
			x<<-y
			m<<-NULL
		}

		get <- function() x
		
		setsolve <- function(solve) m<<-solve
		
		getsolve <- function() m
		
		list(set=set, get=get,
		     setsolve=setsolve,
		     getsolve=getsolve) 

}


## cacheSolve function calcualtes and returns the inverse of the matrix
## returning the cached inverse if it exists

cacheSolve <- function(x, ...) {
	
		m<-x$getsolve()
		if(!is.null(m)) {
			message("getting cached data")
			return(m)
		}
		
		data<-x$get()
		m<-solve(data, ...)
		x$setsolve(m)
		
		m
}
