## Put comments here that give an overall description of what your
## functions do

## Goal of Function:
## Create a matrix object that can be cached
## inverse has already been determined, then this function will return the inverse of the
## matrix from the cache.
## Input: 
## list
## Returns:
## matrix

makeCacheMatrix <- function(x = matrix()) 
{
	i <- NULL

	set <- function(y) 
		{
			x <<- y
			i <<- NULL
		}

	get <- function() x
	setInv <- function(inverse) i <<- inverse
	getInv <- function() i
	list(set = set, get = get, setInv = setInv , getInv = getInv)

}


## Goal of Function:
## Determine the inverse of a matrix created in the makeCacheMatrix function.  If the
## inverse has already been determined, then this function will return the inverse of the
## matrix from the cache.
## Input: 
## list
## Returns:
## matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

	i <- x$getInv()

	if(!is.null(i))
	{
		message("getting cached data")

		return(i)
	}

	matrixData <- x$get()

	i <- solve(matrixData, ...)

	x$setInv(i)

	i
}
