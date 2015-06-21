## These  two functions can be used to find and cache the inverse of a
## matrix, potentially saving time when repeating costly calculations

## the first function creates four functions that can be used to cache and
## retrieve the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y){
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setinverse <- function(solve) inv <<- solve
	getinverse <- function() inv
	list(set = set, get = get,
		setinverse = setinverse, 
		getinverse = getinverse)
}


## the second function will return the inverse matrix, saving time, when 
## possible by returning a cached result instead of re-computing.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
        	message("getting cached data")
        	return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
