## This program is designed to return the inverse of a matrix
## Since this operation is pretty time consuming for large matrices
## We make use of scoping rules in this program to set values for the inverse 
## if they are available in the cache memeory, so as to avoid repitition


makeCacheMatrix <- function(x = numeric()) {
        ## Returns value of inverse of matrix already present in cache
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## Once we have created matrix x using above function, the below function finds the inverse of x

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x', if not already present in cache, then calculates it
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setinverse(m)
        m
}
