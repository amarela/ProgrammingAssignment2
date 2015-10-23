# This pair of functions is used to  cache the inverse of a matrix

# makeCacheMatrix creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
      inverse <- NULL                      # provides default value in case cacheSolve has not been used before
      getmatrix <- function () x           #gets the value of the matrix
      setmatrix <- function (y) {
            x <<- y                        #sets a new value for the matrix
            inverse <<- NULL               #resets the value of the inverse to null, allowing new calculations to be done (or else it would maintain the previous value)
      }
      setinverse <- function(solve) inverse <<- solve   #calculates matrix's inverse and sores its value
      getinverse <- function() inverse    
      
      list (getmatrix = getmatrix, setmatrix = setmatrix, setinverse = setinverse, getinverse = getinverse)
      
}


# cacheSolve computes the inverse of the matrix returned by makeCacheMatrix.
# If the inverse of the matrix has already been calculated (and the matrix has not changed), then it retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
        inverse <- x$getinverse()               #checks if the inverse is already in cache
        if(!is.null(inverse)) {
                message("Getting cached data")  #if the inverse is not null, it gets the cached value
                return(inverse)
        }
        data <- x$getmatrix()                   #gets the matrix stored in makeCacheMatrix
        inverse <- solve(data, ...)             #computes the inverse of the matrix
        x$setinverse(inverse)
        inverse
        
}