## The following functions are used to cache the inverse of a matrix.

## Function makeCacheMatrix creates a special matrix that can cache its inverse. 
## It returns a list containing functions to 
## 1. set the matrix
## 2. get the matrix
## 3. set the inverse of the matrix
## 4. get the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
inv <- NULL
set <- function(y) {
        x <<- y
        inv <<- NULL
}
get <- function() x
setinverse <- function(inverse) inv <<- inverse
getinverse <- function() inv
list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Function cacheSolve computes the inverse of the matrix returned by the above function makeCacheMatrix(). 
## First, it will check if the inverse has already been computed. If so, it gets the result and skips the computation. 
##If not, it computes the inverse, sets the value in the cache via setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("Getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        inv
}
