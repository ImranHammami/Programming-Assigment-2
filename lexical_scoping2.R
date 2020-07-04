
## ##I simply set the input x as a matrix
## and then set the solved value "s" as a null
## then I changed every reference to "mean" to "solve"


makeCacheMatrix <- function(x = matrix()) {
inverse <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setinverse <- function(i) inverse <- i
	getinverse <- function() inverse
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}	

## his function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'	        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getinverse()
        if(!is.null(inverse)) {
        	message("getting cached inverse")
        	return(inverse)
        }
        data <- x$get()
        inverse <- solve(data, ...)
        x$setinverse(inverse)
        inverse
       }
# x <- matrix(c(1, 1, 4, 0, 3, 1, 4, 4, 0), nrow=3, ncol=3)
