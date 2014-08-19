## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(base_matrix = matrix()) {
  matrix_inverse <- NULL

  Get  <- function() {
    return(base_matrix)
  }

  Set  <- function(new_matrix) {
    base_matrix <<- new_matrix
    matrix_inverse <<- NULL
  }
  
  GetInverse <- function() {
    return(matrix_inverse)
  }

  SetInverse <- function(new_matrix) {
    matrix_inverse <<- new_matrix
  }
  
  list(Get = Get, Set = Set , GetInverse = GetInverse, SetInverse = SetInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'

  # If inverse of x is already available , return inverse
  xinv <- x$GetInverse()
  if ( !is.null(xinv) ) {
    message("Inverse already availeble, returning now")
    return(xinv)
  }

  # Execute following if x does not already have an inverse
  message("Inverse NOT availeble, calculating & storing now")
  
  # Get the matrix data from object x
  xbase <- x$Get()

  # Generate the inverse 
  xinv <- solve(xbase,...)

  # Persist the inverse
  x$SetInverse(xinv)

  # Return the inverse
  return(xinv)
}
