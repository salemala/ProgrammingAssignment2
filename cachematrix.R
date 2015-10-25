## The below functions leverage caching when calculating the
## inverse of a matrix (matrix assumed to be invertible). To
## avoid costly repetitive calculations of the inverse of the
## same matrix.



## makeCacheMatrix is a function that produces a list containing
## functions to
## 1. set the value of a matrix
## 2. get the value of a matrix
## 3. set the value of the inverse of th matrix
## 4. get the value of the inverse of the matrix
## The inputed matrix is assumed to be invertible.

makeCacheMatrix <- function(x = matrix())
{
  ## intialize the inverse of the matrix to null
  matrix_inverse <- NULL
  
  ## set the inputed matrix
  set <- function(y)
  {
    x <<- y
    
    matrix_inverse <<- NULL
  }
  
  ## obtain inputed matrix
  get <- function() x
  
  ## set the inverse of the inputed matrix
  setinverse <- function(inverse) matrix_inverse <<- inverse
  
  ## get the inverse of the inputed matrix
  getinverse <- function() matrix_inverse
  
  ## place all calculated values into a list
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve is a function that returns the inverse of
## a matrix. If the inverse of the matrix is available
## in cache it will return the result from there along
## with a message that the result was obtained from cache.

cacheSolve <- function(x, ...)
{
  ## get cached matrix inverse - if inverse is not in
  ## cache matrix_inverse will be set to null
  matrix_inverse <- x$getinverse()
  
  ## check if cached value of the matrix inverse
  ## is valid. If so return the matrix from cache
  ## along with a message stating the inverse was
  ## obtained from cache.
  if(!is.null(matrix_inverse))
  {
    message("getting cached data")
    
    return(matrix_inverse)
  }
  
  ## if the inverse of the matrix is not in cache
  ## then it will be calculated and stored in cache
  
  ## obtain the matrix
  data <- x$get()
  
  ## invert the matrix
  matrix_inverse <- solve(data)
  
  ## cache the inverse of the matrix
  x$setinverse(matrix_inverse)
  
  ## return the inverse of the matrix
  matrix_inverse
}