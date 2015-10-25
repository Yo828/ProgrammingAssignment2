## Two functions for Programming Assignemnt 2

## Note that these functions as well as the comments, have been based on
## the Example: Caching the Mean of a Vector.  

## These functions provide the ability to cache the inverse of a matrix
## as Matrix Inversion is usually a costly computation and there could be a
## benefit in caching this rather than repeatedly computing it.

## Example:
## 	(1) Create an instance of the special matrix 	
##		myMatrix <- makeCacheMatrix (matrix(c(4,3,3,2),nrow=2,ncol=2))
##	(2) Use the following whenever the inverse of the matrix is required
##		cacheSolve(myMatrix)
## 			*Use multiple times to see message displayed

## Function 1: This function creates a special "matrix" object that can cache 
## its inverse. It returns a list containing functions to
##    1. set the value of the matrix
##    2. get the value of the matrix
##    3. set the value of the inverse of the matrix
##    4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
       m <- NULL
        ## Special 'matrix' function to store new values in matrix
        set <- function(y) {
                x <<- y
                m <<- NULL
        }

        ## Special 'matrix' function to return values of matrix
        get <- function() x

        ## Special 'matrix' function to store inverse values 
        setinverse <- function(solve) m <<- solve

        ## Special 'matrix' function to return inverse values 
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## Function 2: This function returns the inverse of a special 'matrix' 
## created by makeCacheMatrix above. If the matrix hasn't changed since the
## last call to this function, then the inverse will be retrieved form cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()

        ## If this is the first time that cacheSolve has been called since
        ## the special 'matrix' has been created, then m would be NULL
        ## For subsequent calls it will be the inverse of the matrix as it
        ## has been set later on in this function
        if(!is.null(m)) {
        		## the inverse of the matrix is stored in variable m, so 
        		## just retrun this value and exit the function
                message("getting cached data")
                return(m)
        }
        ## The inverse of the matrix (m) does not exist in cache, so
        ## Get the matrix values, calculate the inverse and store it in cache	
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
