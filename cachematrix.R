## These functions create a special object that stores a matrix
## and caches its inverse.


## makeCacheMatrix sets value of matrix, gets value of matrix, 
## sets value of inverse, gets value of inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set_mat <- function(A) {    
    x <<- A
  # inv <<- NULL
  }
  get_mat <- function() x
  set_inv <- function(A_inverse) {
    inv <<- A_inverse
  }
  get_inv <- function() inv
  list(set_mat = set_mat, get_mat = get_mat, set_inv = set_inv, get_inv = get_inv)
}


## cacheSolve Calculates inverse of special matrix, unless already 
## known, whence it is retrieved from cache.  If cacheSolve 
## has to compute A inverse, the result is cached.

cacheSolve <- function(x, ...) {
  inv <- x$get_inv()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get_mat()
  inv <- solve(data, ...)
  x$set_inv(inv)
  inv
}