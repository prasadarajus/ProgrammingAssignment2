# R Programming Assignment
# Caching the Inverse of a Matrix
# Usage:
# Create a matrix x
# ex: x <- matrix(1:4, 2)
# Create a special matrix
# ex: sm <- makeCacheMatrix(x)
# Return Matrix Inverse
# ex: cacheSolve(sm)
# makeCacheMatrix function
# This function creates a special "matrix" object 
# that can cache its inverse.
# 1. Set the value of the matrix
# 2. Get the value of the matrix
# 3. Set the value of the inverse
# 4. Get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,  setinv = setinv, getinv = getinv)
}

# cacheSolve: This function computes the inverse of the special "matrix" 
# returned by makeCacheMatrix above. If the inverse has already been calculated 
# then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  
  if (!is.null(inv)) {
    message("inverse is already calculated")
    message("cashed inverse is as below:")
    return(inv)
  }
  
  # when inverse not calculated...
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}

