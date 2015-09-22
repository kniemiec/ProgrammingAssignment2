## create 'object' that contains matrix and matrixInverse values - 'cache'

makeCacheMatrix <- function(x = matrix()) {
  matrixInverse <- NULL
  
  setMatrix <- function(matrixValue){
    x <<- matrixValue
    matrixInverse <- NULL
  }
  getMatrix <- function() x
  
  setMatrixInverse <- function(matrixInverseValue) matrixInverse <<- matrixInverseValue
  getMatrixInverse <- function() matrixInverse
  
  list(setMatrix = setMatrix, getMatrix = getMatrix,
       setMatrixInverse = setMatrixInverse,
       getMatrixInverse = getMatrixInverse)
}

## cache matrix inverse - calculate when needed for the first time

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
  inverse <- x$getMatrixInverse()
  if(!is.null(inverse)){
    return(inverse)
  }
  matrix <- x$getMatrix()
  inverse <- solve(matrix)
  x$setMatrixInverse(inverse)
  inverse
}
