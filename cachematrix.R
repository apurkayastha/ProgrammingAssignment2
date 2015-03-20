## The pair of functions below cache the inverse of a matrix.
## Matrix inversion is usually a costly computation and 
## there may be some benefit to caching the inverse of a matrix 
## rather than compute it repeatedly 


## This function creates and returns a special "matrix" object, which is
## essentially a list. The input argument x is a square matrix.

makeCacheMatrix <- function(x = matrix()) {
        
        # Initialize an empty object to store the inverse
        inv <- NULL
        
        # Fuction defined to store a given matrix
        set <- function(y) {
                
                # The given matrix is stored in the object x, in the 
                # defining environment of this function, and not locally
                # (because of using the <<- operator)
                x <<- y
                
                # The value of inv remains NULL
                inv <<- NULL
        }
        
        # Function defined to retrieve the stored value of the matrix
        get <- function() x
        
        # Function defined which stores a given inverse to the inv object
        # in the defining environment
        setinv <- function(inverse) inv <<- inverse
        
        # Function defined to retrieve the stored inverse
        getinv <- function() inv
        
        # A list is returned that contains all the four functions
        # as its named elements
        list(set = set, get = get, setinv = setinv, getinv = getinv)
              
}

## This function computes the inverse of the special "matrix" (actually a 
## list) returned by the function "makeCacheMatrix". So, the input argument
## x to this function is the result of the "makeCacheMatrix" function. If the
## inverse has already been calculated (and the matrix has not changed),
## then the cacheSolve function retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
       
        # First retrieve the stored inverse value of the matrix
        inv <- x$getinv()
        
        # Check if the inverse has already been calculated
        if(!is.null(inv)) {
                
                # Return the value of inverse from cache
                message("getting cached data")
                return(inv)
        }
        
        ## Inverse has not been calculated, so calculate now
        
        # First get the value of the matrix
        data <- x$get()
        
        # Compute inverse
        inv <- solve(data, ...)
        
        # Set the calculated inverse, so that it 
        # will be cached and need not be computed again
        x$setinv(inv)
        
        # Return the calculated inverse 
        inv
} 
