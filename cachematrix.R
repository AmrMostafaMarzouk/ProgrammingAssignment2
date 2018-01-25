## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than compute it repeatedly 

## The below function is to create a special matrix that can cache it's inverse
##set the value of the matrix
##get the value of the matrix
##set the value of the inverse
##get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y){
		x <<- y
		inv <<- NULL
	}
	get <- function() {
		x
	}
	setinverse <- function(inverse) {
		inv <<- inverse
	}
	getinverse <- function() {
		inv
	}
	
	list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

##This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the ##cachesolve should retrieve the inverse from the cache.
## The below function; first checks to see if the inverse has already been calculated. If so, it gets the inverse from the cache and skips the calculation. Otherwise, it calculates the ##inverse of the data and sets the value of the inverse in the cache via the setinverse function.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		inv <- x$getinverse()
		if(!is.null(inverse)){
		
			message("getting cached data")
            return(inv)
		}
		matrix <- x$get()
		inv <- solve(matrix,...)
		x$setinverse(inv)
		inv
}
