## These functions allow us to take a matrix and get its inverse in a timely manner.

## This function allows us to create a "matrix" object from a matrix that encodes the matrix itself, as well as its inverse, once a value for that has been calculated.

makeCacheMatrix <- function(x = matrix()) {
      i <- NULL
      set <- function(y) {
            x <<- y
            i <<- NULL
      }
      get <- function() x
      setinverse <- function(inverse) i <<- inverse
      getinverse <- function() i
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}


## This function returns the inverse of the matrix encoded by the function above, either from the cache if it has already been calculated, or via the "solve" function. It also sets the value of the inverse in the "matrix" object derived by makeCacheMatrix, if this has not already been set.

cacheSolve <- function(x, ...) {
      i <- x$getinverse()
      if(!is.null(i)) {
            message("getting cached data")
            return(i)
      }
      data <- x$get()
      i <- solve(data, ...)
      x$setinverse(i)
      i
}
