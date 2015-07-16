## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  
  # Create a null vector to store the inverse
  y <- NULL
  
  # setMatrix()
  # Reset the matrix object to a new matrix
  #
  # @param matrix m
  setMatrix <- function(m) {
    x <<- m
    y <<- NULL
  }
  
  # setInverse()
  #
  # Set the inverse of the matrix
  #
  # @param matrix i
  setInverse <- function(i) {
    y <<- i
  }
  
  # getMatrix
  # Get the matrix currently stored in the object
  #
  # @return matrix
  getMatrix <- function() x
  
  # getInverse
  # Get the cached inverse of the matrix currently stored
  # in the object
  #
  # @return matrix
  getInverse <- function() y
  
  # Return a list of object methods
  list(
    setMatrix = setMatrix,
    setInverse = setInverse,
    getMatrix = getMatrix,
    getInverse = getInverse
  )
  
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  
  # Get the current inverse stored in the matrix object
  i <- x$getInverse()
  
  # If the inverse is not NULL, the matrix has not changed
  if (!is.null(i)) {
    message("returning cached result")
    return(i)
  }
  
  # If the matrix has changed, compute the inverse and cache the result
  x$setInverse(solve(x$getMatrix()))
  
  # Return a matrix that is the inverse of 'x'
  x$getInverse()
  
}
