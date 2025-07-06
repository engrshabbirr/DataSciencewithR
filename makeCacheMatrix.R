makeCacheMatrix <- function(x = matrix()) {
  # 'inv' will store the cached inverse of the matrix.
  # It is initialized to NULL.
  inv <- NULL
  
  # 'set' function:
  # This function allows you to set/reset the matrix.
  # When a new matrix is set, the cached inverse 'inv' is reset to NULL.
  # This is crucial because the old inverse is no longer valid.
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  # 'get' function:
  # This simply returns the matrix 'x'.
  get <- function() x
  
  # 'setinverse' function:
  # This function caches the calculated inverse.
  # It is intended to be called by cacheSolve after computing the inverse.
  setinverse <- function(inverse) inv <<- inverse
  
  # 'getinverse' function:
  # This function returns the cached inverse 'inv'.
  getinverse <- function() inv
  
  # Return a list of the four functions defined above.
  # This list is the "special matrix" object.
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


# -----------------------------------------------------------------------------
# cacheSolve:
# This function computes the inverse of the special "matrix" created by
# makeCacheMatrix. It first checks if the inverse has already been calculated.
# If so, it gets the inverse from the cache and skips the computation.
# Otherwise, it calculates the inverse, caches it, and returns it.
# -----------------------------------------------------------------------------

cacheSolve <- function(x, ...) {
  # Attempt to retrieve the inverse from the cache using the getinverse function
  # of the special matrix object 'x'.
  inv <- x$getinverse()
  
  # If the cached inverse is not NULL, it means we have a valid, cached inverse.
  if (!is.null(inv)) {
    # Print a message indicating the data is from the cache and return it.
    message("getting cached data")
    return(inv)
  }
  
  # If the cached inverse was NULL, we need to compute it.
  # First, get the matrix from the object 'x'.
  data <- x$get()
  
  # Calculate the inverse of the matrix using the solve() function.
  # The '...' argument allows for passing additional arguments to solve().
  inv <- solve(data, ...)
  
  # Cache the newly computed inverse by calling the setinverse function
  # of the object 'x'.
  x$setinverse(inv)
  
  # Return the newly computed inverse.
  inv
}