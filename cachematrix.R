## These two functions work together to cache data in matrix a matrix to cut
##     back on processing time.
##
### * makeCacheMatrix(x): Creates a new matrix and caches it returning a list
###			  of functions to access this data in memory.   
### * cacheSolve(x, ...): Computes the inverse of the matrix created in
###			  makeCacheMatrix.

## This function creates a special "matrix" object that can cache its inverse.
## It takes a matrix as it's sole argument.

makeCacheMatrix <- function(x = matrix()) 
{
	# create and initialize a private variable for inverse matrix
        my_inv <- NULL

	# Use scoping assignment operator to cache vars
        set <- function(y) 
	{
            x <<- y
            my_inv <<- NULL
        }

	# Declare methods for accessing/modifying cached vars
        get <- function() x
        set_inverse <- function(inverse) my_inv <<- inverse
        get_inverse <- function() my_inv
	
	# Return a list of methods to the user
        list(set = set, get = get,
             set_inverse = set_inverse,
             get_inverse = get_inverse)
}


## This function computes the inverse of the special "matrix" returned by 
## `makeCacheMatrix` above. If the inverse has already been calculated 
## (and the matrix has not changed), then `cacheSolve` should retrieve the 
## inverse from the cache.
##
## cacheSolve(): requires a list argument made from makeCacheMatrix and allows 
##		 for extra arguments to be passed through to solve()

cacheSolve <- function(x, ...) 
{
	# retrieve current cached value for inverse if any
        my_inv <- x$get_inverse()

	# If found let user know and return cached data
        if(!is.null(my_inv)) 
	{
            message("getting cached data")
            return(my_inv)
        }

	# Use solve to get the inverse of the current cached matrix
        my_inv <- solve(x$get(), ...)

	# cache the new matrix
        x$set_inverse(my_inv)

        ## Return a matrix that is the inverse of 'x'
        my_inv
}
