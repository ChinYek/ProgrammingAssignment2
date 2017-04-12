## These two functions are used to compute the inverse of an invertible 	
## matrix (which has to be square matrix)

## The process is to call out the function makeCacheMatrix first(which 	
## creates a special 'matrix' object to cache the inverse of matrix)
 	
## Then, it will call out the second function cacheSolve(which compute 
## the inverse of 'matrix' returned in makeCacheMatrix; this function 
## will return the inverse of input matrix and cache the inverse if the 
## input matrix is new to the function; else if the inverse has been 
## calculated and cached before, the inverse will retrieve from cache)

## makeCacheMatrix is a function which will create a special 'matrix'
## object (a list), use to cache the inverse of input matrix

makeCacheMatrix <- function(x = matrix()) {
	n <- NULL
	setmatrix <- function(y) {
		x <<- y
		n <<- NULL
	}
	getmatrix <- function() x
	setinverse <- function(inverse) n <<- inverse
	getinverse <- function() n
	list(setmatrix = setmatrix, getmatrix = getmatrix,
	setinverse = setinverse,getinverse = getinverse)
}


## cacheSolve is a function that return the inverse of matrix (input
## of makeCacheMatrix); it plays 2 roles, if the inverse of input 
## function has been calculated and cached before, cacheSolve will 
## deirectly return the inverse; else if the input matrix is a new
## comer, cachesolve will return 'getting cached data' and inverse
## out

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	n <- x$getinverse()
	if(!is.null(n)) {
		message("getting cached data")
            return(n)
	}
      datamatrix <- x$getmatrix()
	n <- solve(datamatrix)
      x$setinverse(n)
      n
}
