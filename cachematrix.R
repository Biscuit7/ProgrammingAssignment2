## This program has two functions
## makeCacheMatrix:
##    Creates a special object from input square matrix and caches its inverse.
## cacheSolve:
##    Computes the inverse of the matrix from the object returned by makeCacheMatrix. If the inverse 
##    has already been calculated (matrix is not changed), it retrieves the inverse from the cache.

## The following function takes on a matrix argument, creates a special object that stores a matrix
## and caches its inverse in an environment otger than the current working environment.

makeCacheMatrix <- function(x = matrix()) {
      sq_mat <- NULL
      set <- function(y) {
            x <<- y
            sq_mat <<- NULL
      }
      get <- function() x
      set_inv <- function(inv) sq_mat <<- inv
      get_inv <- function() sq_mat
      list(set = set, get = get,
           set_inv = set_inv,
           get_inv = get_inv)
}


## The following function takes on the object returned by the previous function and calculates
## the inverse of the matrix. If this has already been calculated, it retrieves it from the cache 
## before doing the calculation.

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      sq_mat <- x$get_inv()
      if(!is.null(sq_mat)) {
            message("Already calculated, retrieving cached inverse matrix!")
            return(sq_mat)
      }
      else message("Recalculating inverse matrix...")
      mat <- x$get()
      sq_mat <- solve(mat, ...) 
      x$set_inv(sq_mat)
      sq_mat
}