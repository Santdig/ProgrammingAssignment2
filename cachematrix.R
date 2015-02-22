## This R program source code is for Coursera R programming course programming assignment 2 solution submission.
## Matrix inversion is usually a costly computation and there may be some benefit to caching 
## the inverse of a matrix rather than compute it repeatedly 
## The below two functions are written that cache the inverse of a matrix.

## The makeCacheMatrix function creates a special "matrix" object that can cache its inverse.
## This function is a list containing a function to:
## 1  set the value of the matrix
## 2	get the value of the matrix
## 3	set the value of the inverse
## 4	get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  iv <- NULL
  set <- function(y) {  ## set the value of the matrix
    x <<- y
    iv <<- NULL
  }
  get <- function() x  ## get the value of the matrix
  setinvrs <- function(invrs) iv <<- invrs  ## set the value of the inverse
  getinvrs <- function() iv  ## get the value of the inverse
  list(set = set, get = get,
       setinvrs = setinvrs,
       getinvrs = getinvrs)
}

## The cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache.
## Otherwise, it calculates the inverse of the matrix 
## and sets the value of the mean in the cache via the setinvrs function.
## This function also checks whether the matrix provided is invertible or singular.
## If singular then throws warning message. This is an additional functinality.

cacheSolve <- function(x, ...) {
  iv <- x$getinvrs()  ## gets the inverse from the cache
  if(!is.null(iv)) {
    message("getting cached matrix inverse data...")
    return(iv)
  }
  data <- x$get()
  if(det(data)==0) {  ## checks if the matrix is invertible or singular
    message("Error!! Sigular matrix provided...")  ## if determinant is 0 then singular
  }
  else {
    iv <- solve(data, ...)
    x$setinvrs(iv)
    iv
  }
}