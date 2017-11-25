## Assignment: Caching the Inverse of a Matrix
## The functions below are meant to demonstrate understanding of lexical scopinng and caching functions 
## utilizing a cache and solve balance across two functions and making use of envorment level data.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  #Set the value of the matrix
    inverse <- NULL
  
    setmat <- function(y) {
        x <<- y
        inverse <<- NULL
    }
  #Get the value of the matrix
    getmat <- function() x
  #Set the value of the inverse of the matrix
    setinvmat <- function(inv) inverse <<- inv
  #Get the value of the inverse matrix
    getinvmat <- function() inverse
  #Return as a list
    list(setmat = setmat
         , getmat = getmat
         , setinvmat = setinvmat
         , getinvmat = getinvmat)
}
  


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  #Check to see if the inverse has already been computed and if so return it from cached
  inverse <- x$getinvmat()
  if(!is.null(inverse)) {
      message("Getting cached data.")
    return(inverse)
  }
  
  #If inverse is not cached figure it out and returin it to the screen
  data <- x$getmat()
  inverse <- solve(data)
  x$setinvmat(inverse)
  inverse
  
  ## Return a matrix that is the inverse of 'x'
}
