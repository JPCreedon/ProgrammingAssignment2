## Programming Assignment 2 - R Programming
## Data Science Specialization Track
## Matrix inversion is usually a costly computation 
## there may be some benefit to caching the inverse of a matrix rather than compute it repeatedly.
##  The following two functions are used to cache the inverse of a matrix.
## Description of makeCacheMatrix:
## set the matrix
## get the matrix
## set the inverse of the matrix
## get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
## Initially set inv to NULL
    inv <- NULL 
## Define set function
    set <- function(y) {
            x <<- y
            inv <<- NULL
    }
## Gets the matrix
    get <- function() x
## Sets inverse of the matrix
    setinverse <- function(inverse) inv <<- inverse
## Get the inverse of the matrix
    getinverse <- function() inv
## Return matrix with our definitions
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)	
}

## cacheSolve:
## computes the inverse and caches or returns the cached value

cacheSolve <- function(x, ...) {

## Get inverse and see if it has been assigned a value
    inv <- x$getinverse()

## If does not equal null return cached value 
    if(!is.null(inv)) {
        message("Getting cached matrix")
        return(inv)
    }

## If equals null get the matrix itself
    data <- x$get()
## Find the inverse
    inv <- solve(data, ...)

## Cache this result in the object
    x$setinverse(inv)

## Return this new result
    inv    
}
