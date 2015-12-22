## Matrix inversion is usually a costly computation and
## there may be some benefit to caching the  inverse of
## a matrix rather than compute it repeatedly. The below
## functions are used to cache the inverse of a matrix.

## This function, makeCacheMatrix creates a special "matrix"
## which is really a list containing a function to
## 1.set the value of the matrix
## 2.get the value of the matrix
## 3.set the value of the inverse of the matrix
## 4.get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y){
        x<<- y
        i<<- NULL
    }
    get <- function() x
    setinverse <- function(inverse)i<<- inverse
    getinverse <- function()i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function, cacheSolve calculates the inverse of the 
## special "matrix" created with the above function. If the
## inverse has been already calculated, it gets the inverse
## from the cache and skips the computation. Otherwise, it 
## calculates the inverse of the matrix and sets the value 
## of inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)){
        message("getting cached data")
        return(i)
    }
    matrix <- x$get()
    i <- solve(matrix, ...)
    x$setinverse(i)
    i
}
