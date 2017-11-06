## Peer-graded Assignment: Programming Assignment 2: Lexical Scoping
## Matrix inversion is usually a costly computation and there may be 
## some benefit to caching the inverse of a matrix rather than compute 
## it repeatedly (there are also alternatives to matrix inversion that 
## we will not discuss here). Your assignment is to write a pair of 
## functions that cache the inverse of a matrix.

## This function creates a "matrix" object that caches the inverse of a matrix
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  matrix <- x
  setMatrix <- function(y) {
    matrix <<- y
    inverse <<- NULL
  }
  getMatrix <- function() matrix
  setInverse <- function(im) {
    inverse <<- im
  }
  getInverse <- function() inverse
  list(setMatrix = setMatrix, getMatrix = getMatrix,
       setInverse = setInverse,
       getInverse = getInverse)
}

## This function computes the inverse of the special "matrix" returned by the makeCacheMatrix function.
## If the inverse has already been calculated (and the matrix has not changed), then cachesolve retrieves
## the inverse from the cache.
cacheSolve <- function(x, ...) {
  # Check if the matrix has not changed and also if the inverse has not been calculated already
  if (exists("cacheMatrix") && x == cacheMatrix$getMatrix() && !is.null(cacheMatrix$getInverse())) {
      message("getting cached inverse matrix")
      return(cacheMatrix$getInverse())
  }
  # Cache matrix and its inverse
  message("caching inverse matrix")
  cacheMatrix <<- makeCacheMatrix(x)
  cacheMatrix$setInverse(solve(x))
  cacheMatrix$getInverse()
}

## Sample executions:

## First execution, the cache is empty so the inverse is cached
# > m = matrix(c(2,5,1,3), nrow=2, ncol=2)
# > cacheSolve(m)
# caching inverse matrix
# [,1] [,2]
# [1,]    3   -1
# [2,]   -5    2

## Second execution, retrieve the inverse from the cache
# > cacheSolve(m)
# getting cached inverse matrix
# [,1] [,2]
# [1,]    3   -1
# [2,]   -5    2

## Third execution, we pass a different matrix. The cache is regenerated
# > m2 = matrix(c(4,1,2,5), nrow=2, ncol=2)
# > cacheSolve(m2)
# caching inverse matrix
# [,1]       [,2]
# [1,]  0.27777778 -0.1111111
# [2,] -0.05555556  0.2222222

