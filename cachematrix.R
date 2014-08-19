## Put comments here that give an overall description of what your
## functions do
## This file contains 2 functions
##    makeCacheMatrix - This functions returns a cached matrix object
##    cacheSolve - This function uses the special matrix object returned by makeCacheMatrix to store/retrieve the inverted matrix
## 
## For example
## my_matrix <- matrix(runif(9),3,3) # Creates a 3 * 3 matrix
## cached_matrix <- makeCacheMatrix(my_matrix) # cached_matrix is a list object which has persisted my_matrix
## cacheSolve(cached_matrix) # Calling cacheSolve first time will generate inverted matrix for my_matrix and pesist in cached_matrix
## cacheSolve(cached_matrix) # Calling cacheSolve there after will return inverted matrix directly from cached_matrix cache

## FUNCTIOn - makeCacheMatrix
## Write a short comment describing this function
## This function accepts a matrix as parameter and returns an R list object
## This list object is used to set or get a matrix and its inverse
## Input matrix and inverted matrix are stored in objects cache 
##
## Returned list object exposes 4 function objects as its items
##    Get - Returns the matrix recieved as parameter or changed using Set
##    Set - Updates the list object cache with new base_matrix, also clears the cache for inverted matrix
##    GetInverse - Returns the inverted matrix from the objects cache
##    SetInverse - Sets the cache with inverted matrix 

makeCacheMatrix <- function(base_matrix = matrix()) {
  ## Take note of "<-" operator here vs <<- in Get & Set functions 
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

## FUNCTIOn - cacheSolve
## Write a short comment describing this function
## This function expects a special list object as an argument (returned by "makeCacheMatrix")
## and returns a inverted matrix stored in the recieved (as argument) objects cache
##
## It first checks if the list object already contains the inverted matrix
## If found the functions returns the inverted matrix by calling GetInverse function of the input object
## Else it gets the base_matrix from input object, generates its inverse 
## and sets it in the input objects cache before returning the inverted matrix 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'

  # If inverse of x is already available , return inverse
  xinv <- x$GetInverse()
  if ( !is.null(xinv) ) {
    message("Inverse already available, returning now")
    return(xinv)
  }

  # Execute following if x does not already have an inverse
  message("Inverse NOT available, calculating & storing now")
  
  # Get the matrix data from object x
  xbase <- x$Get()

  # Generate the inverse 
  xinv <- solve(xbase,...)

  # Persist the inverse
  x$SetInverse(xinv)

  # Return the inverse
  return(xinv)
}
