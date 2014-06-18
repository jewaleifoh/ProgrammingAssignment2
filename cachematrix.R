## makeCacheMatrix function creates a special "matrix" object that can cache its inverse.
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  ##Inverse of the matrix.
  i = NULL
  
  set <- function(m){ 
    x <<- m
    inverse <<- NULL
  }
  
  get <- function() x
  
  setInverse <- function(inverse) i<<- inverse 
  
  getInverse <- function() i
  
  
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
  
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
##then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  inverse <- x$getInverse()
  
  if(!is.null(inverse)){
    message("getting cached data...")
    return(inverse)
  }
  
  data <- x$get()
  
  inverse = solve(data, ...)
  
  x$setInverse(inverse)
  
  inverse
}
