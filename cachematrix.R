## Creates special list of functions that will be used on the input
## x 

makeCacheMatrix <- function(x = matrix()) {
	# initialize inv to NULL
	inv <- NULL
	
	# Define the set function that can be used to set/change the 	 	  value of the matrix x
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	
	# Define the get function that will return the value of the    		  matrix x
	get <- function() x
	
	# Define function to set the inverse of matrix x
	setinverse <- function(inverse) inv <<- inverse
	
	# Define function to get the cached inverse of matrix x
	getinverse <- function() inv
	
	# Return the list of functions
	list(set = set, get = get, 
	setinverse = setinverse, getinverse = getinverse)
}


## This function will return the inverse of the matrix, or if it is      	NULL, calculate it, cache it and return it

cacheSolve <- function(x, ...) {
        ## Get the inverse
        inv <- x$getinverse()
        
        # If the inverse is set (i.e. is not NULL), return the 				  cached value
        if(!is.null(inv)) {
        	message("getting cached data...")
        	return(inv)
        }
        
        # Otherwise, calculate the inverse
        data <- x$get()
        inv <- solve(data)
        # Cache inverse value
        x$setinverse(inv)
        # Return inverse
        inv
}

# this is a comment
