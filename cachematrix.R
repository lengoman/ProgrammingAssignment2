## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# It sets to NULL inverse variable 
# The set and get functions work as a mechanism to store the data in "cache"
# setInverse stores the computed matrix while getInverse return the value
# everything gets store in a list of declarations for easy access from cacheSolve

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setInverse <- function(solve) inverse <<- solve
    getInverse <- function() inverse
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

## Write a short comment describing this function
# 1) In gets the getInverse function from x and if is not null it returns the value from cached data
# 2) Otherwise it inverse the matrix by using the solve function and then calls setInverse to put the data in cache

cacheSolve <- function(x, ...) {
    
    inverse<-x$getInverse()
    if(!is.null(inverse)){
        message("getting cached data")
        return(inverse)
    }
    data<-x$get()
    inverse<-solve(data,...)
    x$setInverse(inverse)
    ## Return a matrix that is the inverse of 'x'
    inverse
}

## Example Usage:
# square_matrix <- matrix(runif(160000, -1. 1), 400, 400)
# cacheSolve(makeCacheMatrix(square_matrix))
