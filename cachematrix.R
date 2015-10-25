## Computing an inverse of a matrix may take long time. To avoid the time 
## cost of recomputing, the program caches the value of inverse to re-use it 
## if we need it again.

## The function makeCacheMatrix creates a list containing a function to:
## 1. set the value of matrix
## 2. get the value of matrix
## 3. set the value of inverse of that matrix
## 4. get the value of inverse of that matrix

makeCacheMatrix <- function(x = matrix()) {
	  i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


##The function cacheSolve returns the inverse of the matrix by:
## First, check if the inverse has already been computed. If yes, it prints 
## out the result without having to compute again. If not, it computes 
## the inverse with setinverse().

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data)
	x$setinverse(i)
        i
}
