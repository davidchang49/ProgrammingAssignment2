## These functions help reduce processing time by make calculus available for reuse

##This function cache a Matrix into memory
makeCacheMatrix <- function(x = matrix()) {
		m <- NULL  #initializin values
		
		#subfunction that sets matrix
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
		#retrieve matrix values
        get <- function() x
		#sub function that sets inverse matrix value
        setinverse <- function(inverse) m <<- inverse
        #sub function that gets inverse matrix value
		getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function calculate the inverse of a matrix and cache result into memory for later use

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		
		m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
		#Applying r function that makes the inverse of a matrix
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
