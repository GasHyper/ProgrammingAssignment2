## Function makeCacheMatrix() produces list of functions for setting and getting
## back to the environment values of matrix and inverse matrix. Function makeCacheMatrix()
## should be called on your initial matrix before cacheSolve() to set initial values!

## Function cacheSolve() checks if inverse matrix is already solved in the environment and  
## returns inverse matrix. Otherwise performs matrix inverse solving, save inverse matrix 
## and returns result. There is no checking for square matrix and det()!=0 
## because for this assignment we assume that matrix is always invertible

makeCacheMatrix <- function(x = matrix()) {
  j <- NULL
  set <- function(y){
    x <<- y
    j <<- NULL
  }
  get <- function()x
  setInverse <- function(inverse) j <<- inverse
  getInverse <- function() j 
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  j <- x$getInverse()
  if(!is.null(j)){
    message("getting cached data")
    return(j)
  }
  mat <- x$get()
  j <- solve(mat,...)
  x$setInverse(j)
  j
}
