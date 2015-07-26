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
  # Reset the matrix object to a new matrix and NULL the inverse.
  #
  # @param matrix m
  setMatrix <- function(m) {
    # Only null the cache if the matrix is different than
    # the one already stored.
    if(!identical(m, x)) {
      y <<- NULL
    }
    x <<- m
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
  # Get the matrix currently stored in the object.
  #
  # @return matrix
  getMatrix <- function() x
  
  # getInverse
  # Get the cached inverse of the matrix currently stored.
  # in the object
  #
  # @return matrix
  getInverse <- function() y
  
  # Return a list containing the internal functions.
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
# retrieve a matrix's inverse.
cacheSolve <- function(x, ...) {
  
  # Get the current inverse stored in the matrix object's cache.
  i <- x$getInverse()
  
  # If the inverse is not NULL, the matrix has not changed,
  # and the inverse retrieved from the cache can be used.
  if (!is.null(i)) {
    message("returning cached result")
    return(i)
  }
  
  # Is the matrix invertible? If not, the determinant will be 0.
  if (det(x$getMatrix()) == 0) {
    stop("The matrix is not invertible")
  }
  
  # If the matrix has changed, compute the inverse and cache the result.
  i <- solve(x$getMatrix(), ...)
  x$setInverse(i)
  
  # Return a matrix that is the inverse of the matrix stored in 'x'.
  i
  
}
