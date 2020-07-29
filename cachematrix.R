## The aim is to see the benefit of caching the inverse
## of a matrix rather than repeated computations.
## We write two functions to cache the inverse.

## This function creates a special matrix 
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  n <- NULL    ##Initialize inverse property
  
  ## To set the matrix 
  set <- function(y){
    x <<- y
    n <<- NULL
  }
  
  ## To get the matrix
  get <- function() x
  
  ## Method to set the inverse of a matrix
  setInverse <- function(inverse) n <<- inverse 
  
  ## Method to get the inverse of a matrix 
  getInverse <- function() n 
  
  ## A list of the methods 
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)

}


## This function computes the inverse of the special matrix
## if the inverse has been calculated, then the cachesolve 
## should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  n <- x$getInverse()
  
  if(!is.null(n)){
    
    message("getting cached data")
    return(n)
  }
  
  ##Getting matrix from our object
  data <- x$get()
  
  n <- solve(data, ....)
  
  ##Setting inverse of the object
  x$setInverse(n)
  
  n
}
