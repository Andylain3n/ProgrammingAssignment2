##Functions makeCacheMatrix and cachesolve cache a value
##of a matrix and solve the value of its inverse and
##cache it. 


##makeCacheMatrix stores a matrix and its inverse. The inverse
##is set as NULL by default until its calculated with cacheSolve.
##It specifies four functioins for setting a value for a
##matrix, getting that value, setting a value for an inverse
##of that matrix and getting its inverse.
##Function takes a matrix (assumed to be square) as an input.

makeCacheMatrix <- function(x = matrix()) {
	##The inverse of matrix x is stored as a variable "inv".	
	inv <- NULL
	##set defines its input as a new value for the matrix
	##and resets the value of its inverse. 						 
	set <- function(y) {
			##"<<-" operator is used to define both the values
			##for x and inv in the parent enviroment of the 
			##function "set".		
			x <<- y
			inv <<- NULL
	}
	##get returns the matrix.	
	get <- function() x

	##setinv sets a value for the inverse in
	##makeCacheMatrix (its parent enviroment)
	## using the "<<-" operator.
	setinv <- function(inverse) inv <<- inverse

	##getinv retuns user the inverse of matrix x.
	getinv <- function() inv

	##The functions specified above are stored 
	##in a list.
	list(set = set, get = get, setinv = setinv,
		getinv = getinv)
}


##Function cacheSolve solves the inverse for the matrix given
##to the makeCacheMatrix function using the functions specified
##in makeCacheMatrix. The input for cacheSolve is a list created
##by makeCacheMatrix.

cacheSolve <- function(x, ...) {

	##function getinv is called from the list x to get
	##the value of the inverse which is stored in function
	##makeCacheMatrix.
      inv <- x$getinv()

	##If the value of inverse (inv) is not default (null)
	##it can be assumed that its value is already calculated
	##and no recalculation is needed. Therefore the existing
	##value is returned. 
	if(!is.null(inv)) {
		  message("getting cached data")
		  return(inv)
	}

	##If the value of the inverse is default (not calculated)
	##the function "get" inside makeCacheMatrix is called by
	##subsetting the list x created in makeCacheMatrix 
	##to get the stored value of the matrix.
	data <- x$get()

	##The inverse for this is calculated using function solve.
	##Note that this variable is now located inside cacheSolve.
	inv <- solve(data, ...)

	##Value of the inverse is stored inside the object created 
	#in makeCacheMatrix by calling the function "setinv" from
	#the list x.
	x$setinv(inv)
	
	#Finally the value of the inverse is returned.
	inv 
}
