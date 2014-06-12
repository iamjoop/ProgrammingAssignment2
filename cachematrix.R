## These two functions will create an objects in R that will cache the value of the inverse of a matrix in a different environment.  When the solve function is called (to find the inverse), the cacheSolve function will first check to see if the inverse has been cached, thus saving calculation time.

## This function will create a copy of the matrix in a different environment.  It will return a named list of functions applied to the matrix input and an inverse matrix initialized to NULL.
makeCacheMatrix <- function(x = matrix()) {
	#Initialize the m variable within this function with the value NULL
	m <- NULL
	# This creates a function within this environment that creates a cached version of the matrix input y and stores it as matrix x in the new environment.
	# It then initializes a second matrix m that will eventually be the inverse of x
	set <- function(y) {
		x <<- y #create the matrix x in the new environment
		m <<- NULL #intialize m to be NULL in the new environment
	}
	# This function retrieves x from the new environment.  It takes no arguments and simply returns x.
	get <- function() {
		x #return the value of x
	}
	# This function takes an input which is the solve function.
	# It stores the output of the solve function to the matrix m when called.
	setinv <- function(solve) {
		m <<- solve #Store the output of the solve function to the matrix m in the new environment.
	}
	# This function retrieves m from the new environment.  It takes no arguments and simply returns m.
	getinv <- function() {
		m #return the value of m
	}
	#Finally, return a named list containing the four functions defined within this function
	list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This function will check to see if the getinv functions returns a non-null value (an inverse).  If so, print a message and return the inverse.  If not, call the setinv function within the list input to store the value of the inverse in the different environment.
cacheSolve <- function(x, ...) {
	m <- x$getinv() #Call the function that retrieves the cached inverse, store the output as m.
	if(!is.null(m)) { #Check to see if the value of m is not null
			message("getting cached inverse") # Print a message to the console that the inverse has been retrieved from the cache rather than re-calculated
			return(m) # End the function and return the value of m
	}
	# If the function makes it to this point, the value of m was NULL, and the inverse of x needs to be calculated.
	thisMatrix <- x$get() #Retrieve the matrix from the environment.
	m <- solve(thisMatrix, ...) #Compute the inverse of x and store it as the variable m.
	x$setinv(m) #Set the value of m in the new environment.
	m #return the inverse matrix
}

#test it out
# testMe <- matrix(rnorm(1000000),nrow=1000,ncol=1000,byrow=TRUE)
# x <- makeCacheMatrix(testMe)
# system.time(cacheSolve(x))
# system.time(cacheSolve(x))
