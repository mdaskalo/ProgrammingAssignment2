## Put comments here that give an overall description of what your
## functions do
## Overall the makeCacheMatrix create a list with 4 function, which contain a specific enironment
## The environment contains the original matrix passed as y to the function set.
## The environment also contains the inverse matrix named "solvedM", which is set to NULL after every call to set function.
## This solvedM matrix can be externally read and written (using getSolved and setSolved)
## The function cacheSole - does exactly this. It reads the solvedM from the environment associated with the result of a makeCacheMatrix


## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  solvedM <- NULL
  set <- function(y) {
    x <<- y
    solvedM <<- NULL
  }
  get <- function() x
  setSolved <- function(solvedInvM) solvedM <<- solvedInvM
  getSolved <- function() solvedM
  list(set = set, get = get,
       setSolved = setSolved,
       getSolved = getSolved)
}


## Write a short comment describing this function
## The function expects x to be a list created with makeCacheMatrix
## It uses the last element ($getSolved) to access the value
## that is potentially stored in the environment of the function
## This is fact work in a way simlar to anonymous inner Java class
## If there is no stored value for the inverse matrix which is contained in x
## then it is computed

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  mSolved <- x$getSolved()
  if(!is.null(mSolved)) {
    message("getting cached data for Inverse Matrix")
    return(mSolved)
  }
  data <- x$get()
  message("Calculating inverse matrix using solve(matrix, ...)")
  mSolved <- solve(data, ...)
  x$setSolved(mSolved)
  mSolved  
}
