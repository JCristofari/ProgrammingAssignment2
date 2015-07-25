## The goal is to write a pair of functions that cache the inverse of a matrix.

## creates a special "matrix" that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
      ## x is a square invertible matrix
      ## The function creates a list containing a function to
      ##    set the matrix
      ##    get the matrix
      ##    set the inverse
      ##    get the inverse
      
      Inv <- NULL
      set <- function(y) {
            x <<- y
            Inv <<- NULL
      }
      get <- function() x
      setinv <- function(inverse) Inv <<- inverse
      getinv <- function() Inv
      list(set = set, get = get,
           setinv = setinv,
           getinv = getinv)
}


## The next functioncomputes the inverse of the "matrix" returned by 
## makeCacheMatrix(). 
## If the inverse has already been calculated and the matrix has not changed,
## it'll retrieves the inverse from the cache directly.

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      
      inv = x$getinv()
      
      ## First check to see if the mean has already been calculated.
      
      if (!is.null(inv)){
            ##If so, it gets the mean from the cache and skips the computation
            message("getting cached data")
            return(inv)
      }
      
      ## otherwise, calculates the inverse 
      mat.data = x$get()
      inv = solve(mat.data, ...)
      
      ## sets the value of the inverse in the cache via the setinv function.
      x$setinv(inv)
      
      return(inv)
}