## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
	
    ##initialising inverse matrix
        inv <- NULL
		
	##function for setting value of matrix
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
		
	##function for getting value of matrix
        get <- function() x
	
	##function for setting value of inverse matrix
        setInverse <- function(inverse) inv <<- inverse
		
	##function for getting value of inverse matrix
        getInverse <- function() inv
		
	##return list of functions
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
		 ##get value of inverse matrix from cache
		inv <- x$getInverse()
        
	    ##check if cache value is not null and use the cached data and return
		if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
		
		##this part is executed if cached data was null
		##fetch the data or the orginal matrix
        mat <- x$get()
		
	    ##calculate the inverse of the matrix
        inv <- solve(mat, ...)
		
	    ##set the inverted matrix value in the cache
        x$setInverse(inv)
		
	    ##return inverse matrix
        inv
}
