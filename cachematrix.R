## Create a special "matrix" object that can cache its inverse, which is indeed list of functions
makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	## first function to set the value of Matrix
      set <- function(y) {
             x <<- y
             
             ## create the empty objects where inverse matrix cached
             inv <<- NULL
        }
        ## Second function with return is x
        get <- function() x
        ## Third function to take an argument and reassign the value of inv in the parent environment
        setinverse <- function(inverse) inv <<- inverse
        ## Forth function to get return of inv
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)


}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## Call the forth function of makeCacheMatrix to get inv in case inve is cached
	  inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        ## If not, calculate the inverse of the matrix by function solve
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}

###### Test the Code
x <- matrix(1:4,2,2)
x1 <- makeCacheMatrix(x)
x2 <- cacheSolve(x1)
x2
