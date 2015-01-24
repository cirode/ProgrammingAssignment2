## The functions below create a "CacheMatrix" object that allows caching of
## the creation of its inverse. The original matrix can be retrieved
## by calling the x$get() function on the "CacheMatrix" that is returned
## both functions are intended to be used together

## makeCacheMatrix takes a matrix object ( defaulting to a 1x1 matrix with NA )
## and returns a "CacheMatrix" object
## with the following functions:
##
## $get(): returns the original matrix
## $set(y): sets the matrix contained within the CacheMatrix object
##          setting the contained matrix also resets the cached inverse
##          it returns the parameter given
## $getSolve(): returns either the previously cached inverse of the matrix
##          or NULL
## $setSolve(y): sets a calculated inverse of the matrix
##          it returns the paremeter given

makeCacheMatrix <- function(x = matrix()) {

  solveCache <- NULL

  get <- function() x
  set <- function(y){
    x <<- y
    solveCache <<- NULL
    y
  }

  getSolve <- function() solveCache
  setSolve <- function(y){
    solveCache <<- y
    solveCache
  }

  list(get= get, set=set, getSolve=getSolve, setSolve=setSolve)
}


## cacheSolve is a replacement to the standard solve() function
## that can be used with a "CacheMatrix" object. It calculates
## the inverse of the provided "CacheMatrix" and caches that inverse
## so that the inverse need not be solved again.
## cacheSolve has the same arity as the standard solve function

cacheSolve <- function(x, ...) {

  solveAndCache <- function(x, ...){
    inverse <- solve(x$get(), ...)
    x$setSolve(inverse)
  }

  cachedSolve <- x$getSolve()

  if( !is.null(cachedSolve) ){
    cachedSolve
  } else {
    solveAndCache(x, ...)
  }
}