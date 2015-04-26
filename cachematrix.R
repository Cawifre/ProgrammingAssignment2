## The following functions construct and use cacheMatrix objects
## to provide a mechanism for associating cached inverse matrix solutions
## with the original matrices they are calculated from

## makeCacheMatrix(x): Accepts x, a matrix, as an argument and constructs
## an object with methods allowing for setting and retrieving a source
## matrix and for setting and retrieving a cached matrix inversion solution

makeCacheMatrix <- function(x = matrix()) {
    
    inverseMatrix <- NULL ## Initialize a location to cache results
    
    set <- function(matrix) { ## Set a new value for the source matrix
        x <<- matrix
        inverseMatrix <<- NULL ## Invalidate the current cache
    }
    
    get <- function() { ## Retrieve the source matrix
        x
    }
    
    setInverse <- function(inverse) { ## Cache matrix inversion solution
        inverseMatrix <<- inverse
    }
    
    getInverse <- function() { ## Retrieve cached inversion solution
        inverseMatrix
    }
    
    list(set = set, ## Combine functions as named members of a list to
         get = get, ## construct an objectca
         setInverse = setInverse,
         getInverse = getInverse)
}


## cacheSolve(x): Accepts x, a cacheMatrix, as an argument and returns the
## inverse of the associated matrix. If the cacheMatrix has a cached
## solution, that value is used.

cacheSolve <- function(x) {
    
    ## Attempt to use a cached solution
    cached <- x$getInverse()
    if(!is.null(cached)){
        message("using cached solution")
        return(cached) ## Return the cached solution and exit the function
    }
    
    ## Calculate the inverse of the matrix from scratch
    inverse <- solve(x$get())
    x$setInverse(inverse) ## Cache the new solution
    inverse
}
