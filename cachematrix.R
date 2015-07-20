# These functions use a cache to store the result of a potentially
# expensive operation; inverting a matrix
# A special object stores a matrix along with its inverse, once computed.
# After the inverse is cached, any future need for the matrix's inverse
# can be satisfied by the cache, and the actual computation can be skipped.

# makeCacheMatrix
#
# A function that stores a matrix along with its cached inverse.
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


# cacheSolve
#
# A function that uses the makeCacheMatrix object to store and
# retrieve a matrix's inverse
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
  
  # Return a matrix that is the inverse of the matrix stored in 'x'
  i
  
}
