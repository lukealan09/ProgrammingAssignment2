## The pair of functions below Cache the inverse of a matrix.

## This function follows the same structure as the makeVector example function.
## All instaces of the word "mean" have been changed to "inverse". In addition,
## two conditional statements are added at the to prevent the user from 
## trying to calculate the inverse of a square matrix, and to prevent the user
## from trying to calculate the inverse of a non-invertible matrix.
makeCacheMatrix <- function(x = matrix()) {
  ## Returns a list "attached" to a matrix. The elements in the list are four
  ## functions which are described below.
  
  ## If the matrix is not square, exit the function without executing code.
  if (nrow(x)!=ncol(x)) {
    message("Please input a square matrix.")
    return
  ## If the matrix is square, but is not invertible, exit without executing
  ## any further code.
  }
  if (det(x) == 0) {
    message("Please input an invertible matrix.")
    return
  }
  ## This will be used to store the inverse.
  m <- NULL
  ## Create the set function. The <<- is necessary to modify x from a function 
  ## within a function. Also, setting m to null is done since a new matrix
  ## will require a new inverse computation. Otherwise, an incorrect "inverse" 
  ## may be stored.
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  ## This function retrieves the stored matrix. This allows the user to examine
  ## the contents of the matrix object.
  get <- function() x
  ## Allows cacheSolve (below) to set an inverse.
  setinverse <- function(inverse) m <<- inverse
  ## Allows cacheSolve (below) to retrieve a set inverse.
  getinverse <- function() m
  ## "Attaches" the functions to the matrix.
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## This function follows the same structure as the makeVector example function.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'.
  
  ## Determine if the inverse of a matrix was already stored. If so, 
  ## the function reports this is the case and then returns the inverse
  ## without recalculating it.
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  ## We already know the inverse has not been calculated. The code below
  ## retrieves the inverse, calcualtes the inverse, then stores the inverse
  ## using the appropriate function from within the MakeCacheMatrix
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}