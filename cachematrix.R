## This Script creates two functions. The objective is create a routine to avoid 
## unnecessary time consuming calculations for inverse matrix.

## The first one, makeCacheMatrix, transform a regular matrix in a
## "special" vector that have four within functions:

## Function name:       Explanation:
## setnewMatrix         create new values for the matrix
## getMatrix            Return the value of the matrix
## cacheInverse         Stores the value of inverse matrix
## getInverse           Returns the stored inverse matrix


## The second function, cacheSolve, check if the value of the matrix is stored in 
## the cache memory. If not, it calculates the inverse matrix and cache it.
## chacheSolve only works if the matrix was "transformed" by makeCacheMatrix function. It
## does not work with a regular matrix.


makeCacheMatrix <- function(x = matrix()) {   
  #The command "x=matrix()" force the use of a propper input 
  #value in makeCacheMatrix function
  
  # holds the cached value or NULL if nothing is cached
  cache <- NULL
  
  # Set a new value to the matrix
  setnewMatrix <- function(newValue) {
    x <<- newValue
    # Reset the cached Inverse Matrix value
    cache <<- NULL
  }
  
  # returns the stored matrix
  getMatrix <- function() {
    x
  }
  
  # cache the Inverse Matrix value
  cacheInverse <- function(solve) {
    cache <<- solve
  }
  
  # get the cached value
  getInverse <- function() {
    cache
  }
  
  # return a list. Each named element of the list is a function
  list(setMatrix = setnewMatrix, getMatrix = getMatrix, cacheInverse = cacheInverse, getInverse = getInverse)
}

## Return a matrix that is the inverse of 'x'
## Function explanation above

cacheSolve <- function(x, ...) {
   
      
    # get the cached value
    inverse <- x$getInverse()
    
    # if a previous stored value exists return it
    if(!is.null(inverse)) {
      message("getting cached data")
      return(inverse) # The return command conclude the function.
    }
    
    # otherwise get the matrix, caclulate the inverse and store it in
    # the cache
    # get the value of the matrix
    data <- x$getMatrix()
    
    # calculate the inverse matrix
    inverse <- solve(data)
    
    # store que value of inverse matrix in the cache
    x$cacheInverse(inverse)
    
    # return the inverse
    print(inverse)
}
