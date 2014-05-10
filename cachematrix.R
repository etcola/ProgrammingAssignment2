## The functions in this file are used for caching the matrix inverse result,
## to avoid the duplicate cpu intensive computation, at the cost
## of more memory spending.

## Notice: in this example, matrices are ASSUMED to be INVERTIBLE.

## makeCacheMatrix is a list containing a functions to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse matrix
## 4. get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setinverse <- function(calculated_inverse) inverse <<- calculated_inverse
    getinverse <- function() inverse
    list(set = set,
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve calculates the inverse of the matrix created with makeCacheMatrix,
## it first checks whether the inverse has already been calculated, 
## if so, it gets the inverse from the cache and skips the computation;
## otherwise, it calculates the inverse and set the result into cache 
## via setinverse, after which it returns the result.

cacheSolve <- function(x, ...) {
    inverse <- x$getinverse()
    if (!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    data <- x$get()
    newinverse <- solve(data)
    x$setinverse(newinverse)
    newinverse
}
