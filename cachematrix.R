## These functions allow the user to create a special kind of matrix
## that can cache its own calculate and cache its own inverse. 
## This ensures the inverse only needs to be calculated once and 
## is then accessed from the cache.

## makeCacheMatrix function stores the special matrix.  It creates a list containing
## functions that can set and get the actual matrix aswell as set and get 
## the inverse

makeCacheMatrix <- function(x = matrix()) {

		m<-NULL
		 
    ## set a new matrix (x) and reset the cached inverse matrix (m)
		set <- function(y) {
			x<<-y
			m<<-NULL
		}

    ## return the original matrix
		get <- function() x
		
    ## set the inverse (solve) of the matrix
		setsolve <- function(solve) m<<-solve
		
    ## return the inverse matrix
		getsolve <- function() m
		
    ## set a list of functions to set and get the original matrix and the inverse
		list(set=set, get=get,
		     setsolve=setsolve,
		     getsolve=getsolve) 

}


## cacheSolve function calcualtes and returns the inverse of the matrix
## returning the cached inverse if it exists

cacheSolve <- function(x, ...) {
	  
    ## get the current cached inverse matrix if it exists 
		m<-x$getsolve()
    ## check if the inverse exists (not null)
		if(!is.null(m)) {
      ## if it exists return cached value
			message("getting cached data")
			return(m)
		}
		
    ## otherwise get the matrix (x) and use solve to set the inverse (m)
		data<-x$get()
		m<-solve(data, ...)
		x$setsolve(m)
		
    ## return the inverse matrix
		m
}
