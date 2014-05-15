## Functions to build a matrix with cached inverse.
##
## Usage (suppose A is an invertible matrix):
##
## To create the cached object:
##    Acached  <- makeCacheMatrix(A)
##
## To retrieve the inverse:
##    Ainverse <- cacheSolve(Acached)
##
## See descriptions of each function below.
## Implementation tested on magic squares matrixes
## (see package "magic") of dimensions 3 and 5
## (4 is not invertible).


## MAKECACHEMATRIX:
## This function creates a caché object that stores
## the matrix and assigns a variable to hold the caché
## of the inverse matrix. Note that the inverse is not
## calculated by this function, since at the creation
## of the object we might still don't need the inverse,
## (we might even not need it at all).

makeCacheMatrix <- function(x = matrix()) {
    
    # This function is more than less the same as
    # the makeVector example in https://class.coursera.org/rprog-003
    
    m <- NULL
    set <- function(y) { # This function sets the value of the matrix,
        x <<- y          # and a nulled variable for the cached inverse
        m <<- NULL
    }
    
    get <- function() x  # Retrieve the matrix
    
    setinv <- function(inv) m <<- inv # Store the inverse
    
    getinv <- function() m   # retrieve cached inverse
    
    list(set = set, get = get,     # return results
         setinv = setinv,
         getinv = getinv)
}



## CACHESOLVE:
## This function is to be called when the inverse matrix
## is needed. Note that its argument is the caché object,
## not the original matrix.
## The first time it is called, the inverse matrix is 
## calculated by means of an internal call to solve(),
## and stored. On subsequent calls, the inverse is not
## calculated but retrieved from its storage.
##
cacheSolve <- function(x, ...) {
    
    # Return an inverse for the matrix stored in x
    
    m <- x$getinv()      # recover content of caché
    
    if(is.null(m)) {     # if we retrieved an empty caché...
        
        # tell the user
        message("no cached inverse found, calculating")
        
        # then calculate the inverse
        m <- solve(x$get(), ...)
        
        # store inverse in cache for subsequent calls
        x$setinv(m)
    }
    
    m   # return inverse, either calculated or retrieved
}
