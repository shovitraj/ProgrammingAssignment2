## Put comments here that give an overall description of what your
## functions

## These pair of functions will cache the inverse of matrix

## Write a short comment describing this function

## makeCacheMatrix function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL                        ##inizialised inv as NULL
        set <- function(y) {                
                x <<- y                   ##value of matrix in parent environment
                inv <<- NULL              ## rest inv to NULL, if there is a new matrix
        }
        get <- function() x               ##returns value of the matrix argument
            
        setInverse <- function(inverse) inv <<-inverse  ##assigns value of the inv in parent env
                
        getInverse<- function() inv       ##gets the value of inv where called
        
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Write a short comment describing this function

##this function computes the inverse of the matrix returned by function makeCacheMatrix
##if the inverse has already been calculated (and the "matrix" has not changed), 
##then cacheSolve should retreive the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setInverse(inv)
        
}
