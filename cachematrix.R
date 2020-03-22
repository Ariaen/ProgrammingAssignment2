
# The first function, `makeMatrix` is fully based on the example 'makeVector'
# that was shared by R.D. Peng, as part of the course material.
# It creates object with a list that returns four functions that can then be executed 
# on the matrix that it receives when called. It can be used to create a matrixobject 
# that can create an inverse for the matrix it receives and cash that result.

#These are the functions that will be available in the created object:
#1.  set the value of the matrix
#2.  get the value of the matrix
#3.  set the value of the inverse of the matrix
#4.  get the value of the mean

 makeMatrix <- function(x = matrix()) { #stored a solvablematrix as default

	# The function is called to create the functions object so create empty cache
	im <- NULL			 	

	# The set function creates the matrix so it needs to empty the cache in parent environment
	# and asign value there to object x
      set <- function(y) {		
      	x <<- y
		im <<- NULL
	}

	# The get function returns the value of object x
	get <- function(){
		x
	}

	# setInverse stores a result in im (the cache which will now not be NULL anymore
	setInverse <- function(inverse){ 
		im <<- inverse
	}

	# getInverse returns the value stored in cache
	getInverse <- function(){
		 im
	}
	
	# create a list with the defined objects so they can be called when needed.
	list(set = set, get = get,
	setInverse = setInverse,
	getInverse = getInverse)
    }

# The following function is also fully based on the example 'cachemean'
# that was shared by R.D. Peng, as part of the course material.
# It calculates the inverse using the special "matrix" object created with the 
# above function. However, it first checks to see if the
# inverse has not already been calculated. If availabe, it gets the inverse
# from the cache and skips the computation. Otherwise, it calculates the inverse 
# of the matrix and stores the inverse in the cache via the `setInverse`
# function.


# when calling cacheInverse, provide it with the object created with make Matrix.
cacheInverse <- function(x, ...) {
		
	# get the inversed cache from the object's get-function
	im <- x$getInverse()
	
	# if there is a cached calculation (im is not NA), then say so and return cached value 
	if(is.null(im) == FALSE) {
		message("getting cached inverse")
		return(im)	# this ends execution of the function as well
	}
	
	# function was not ended,so cache was NA and calculation is needed
	# first get the matrix given as input when the special matrix object was created
	matrix <- x$get()
	# then calculate the inverse (for this quiz assumption is made that the matrix can be inversed)
	im <- solve(matrix, ...)
	# Use setInverse function of the object to strore the result in cache 
	x$setInverse(im)
	# return the value of the calculation as result.
	im
    }


