## This scripts containing functions to cache inverse matrix computations and potentially to increase efficiency 
## functions do

## makeCacheMatrix is to create a "cached" matrix, which stores the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        get <- function() x
        
        setinv <- function(invmat) inv <<- invmat
        getinv <- function() inv
        
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
        

}


## cacheSolve is to retrive cache inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inverse <- solve(data, ...)
        x$setinv(inverse)
        inverse
}
