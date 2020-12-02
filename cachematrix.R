## Put comments here that give an overall description of what your
## functions do
## makeCacheMatrix stores in cache a matrix and its inverse
## cacheSolve reads in the cache if a matrix is already stored to return its inverse and calculate its inverse if not

## Write a short comment describing this function
## initialises a matrix mat and its inverse and stores them in cache

makeCacheMatrix <- function(x = matrix()) {
     mat <- NULL
     set <- function(y) {
          x <<- y
          mat <<- NULL
     }
     get <- function() x
     setinverse <- function(solve) {mat <<- solve}
     getinverse <- function() mat
     list(set = set, get = get,
          setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function
## checks first if the matrix is already in cache
## if yes => reads and returns the cached data
## if not => create the cached inverse matrix and returns the inverse matrix
## in this case, x is a vector (including set, get, setinverse and getinverse)
cacheSolve <- function(x, ...) {
     ## Return a matrix that is the inverse of the matrix stored in cache
     mat <- x$getinverse()
     if (!is.null(mat)) {
          message("getting cached data")
          return (mat)
     }
     data <- x$get()
     mat <- solve(data, ...)
     x$setinverse(mat)
     mat
}

