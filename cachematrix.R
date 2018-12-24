## Put comments here that give an overall description of what your
## functions do
## My functions, "makeCacheMatrix" and "cacheSolve", cache the inverse of a matrix
## Write a short comment describing this function
## "makeCacheMatrix"gets a matrix as an input, set the value of the matrix, get the value of the matrix, set the inverse Matrix and get the inverse Matrix. The matrix object
#can cache its own object. 
makeCacheMatrix <- function(x = matrix()) {
  theMatrix <- NULL
  set.the.Matrix <- function(y) {
    x <<- y
    theMatrix <<- NULL
}

  getMatrix <- function() x                              
  setInverse <- function(inverse) theMatrix <<- inverse  
  getInverse <- function() theMatrix                     
  list(set.the.Matrix = set.the.Matrix, getMatrix = getMatrix,
       setInverse = setInverse, getInverse = getInverse)
  
}


## Write a short comment describing this function
## cacheSolve computes the inverse of the special "matrix" , the result of makeCacheMatrix above. If the inverse has already been calculated, then the cachesolve should retrieve the inverse from the cache
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached result")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
  }
