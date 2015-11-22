## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse
## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## (1) makeCacheMatrix function
makeCacheMatrix <- function(x = numeric()) {
  
  # holds the cached value or NULL if nothing is cached
  cache <- NULL
  
  #the following stores a matrix
  setMatrix <- function(newValue) {
    x <<- newValue
    
    ##now we have a new value assigned, so will make the cache NULL again
    cache <<- NULL
  }
  
  #the following returns the stored matrix that we created above
  getMatrix <- function() {
    x
  }
    
  # cache the given argument 
  cacheInverse <- function(solve) {
    cache <<- solve
  }
  
  # get the cached value
  getInverse <- function() {
    cache
  }
  
  # return a list. Each named element of the list is a function
  list(setMatrix = setMatrix, getMatrix = getMatrix, cacheInverse = cacheInverse, getInverse = getInverse)
}

# (2) Cachesolve Function: calculates the inverse of a "special" matrix created with makeCacheMatrix
cacheSolve <- function(y, ...) {

# this gets the cached value
inverse <- y$getInverse()
# if a cached value exists return it
if(!is.null(inverse)) {
  message("getting cached data")
  return(inverse)
# otherwise get the matrix, caclulate the inverse and store it in the cache
data <- y$getMatrix()
inverse <- solve(data)
y$cacheInverse(inverse)
# return the inverse
inverse
}