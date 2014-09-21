## Put comments here that give an overall description of what your
## functions do

## The makeCacheMatrix function creates a list of 4 functions for getting and setting the matrix and 
## inverse of matrix in cache

makeCacheMatrix <- function(x = matrix()) {
  #set the inverseMatrix parameter as null
  inverseMatrix <- NULL
  
  set <- function(y) {
    x <<- y
    
    #when setting the new matrix in cache, set the inverseMatrix cached variable as null
    inverseMatrix <<- NULL 
  }
  
  get <- function() x 
  
  setInverseMatrix <- function(matrix) inverseMatrix <<- matrix
  
  getInverseMatrix <- function() inverseMatrix
  
  list(set = set, get = get,
       setInverseMatrix = setInverseMatrix,
       getInverseMatrix = getInverseMatrix)
  
}


## The cacheSolve function returns the inverse matrix from the cache, if it is not there in cache, it gets the matrix 
## from cache then calculate the inverse and then sets the calculated inversed matrix in cache and return the same.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

  inverseMatrix <- x$getInverseMatrix()
  
  if(!is.null(inverseMatrix)) {
    message("getting cached data")
    return(inverseMatrix)
  }
  
  data <- x$get()
  
  inverseMatrix <- solve(data) #solve() with one parameter as matrix returns the inverse of that matrix.
  
  x$setInverseMatrix(inverseMatrix)
  
  return(inverseMatrix)
  
  
}
