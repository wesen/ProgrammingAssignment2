#' Create a caching matrix object
#' 
#' This creates an object that stores a matrix and can cache its inverse.
#' This works in conjunction with `cacheSolve`
#' 
#' @example 
#' m = makeCacheMatrix()
#' inv_m = cacheSolve(m)
#' 
#' @return an object that stores a matrix and potentially its inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inv_) inv <<- inv_
  getinv <- function() inv
  list(set = set,
       get = get,
       setinv = setinv,
       getinv = getinv)
}

#' Invert a cached matrix
#' 
#' If the matrix has been inverted before, the cached result is returned. Else,
#' the computed result is cached in `x`
cacheSolve <- function (x, ...) {
  m <- x$getinv()
  if (!is.null(m)) {
    return(m)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinv(inv)
  inv
}