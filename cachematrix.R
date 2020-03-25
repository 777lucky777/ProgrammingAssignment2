 
## Together these functions use solve() to invert a matrix (given as input),cache the result in the parent
## ... environment and outputs the result.  If the input matrix does not change on the following iteration
## ... then the cached result is retrieved and returned without recalculating the result.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmean <- function(solve) m <<- solve
  getmean <- function() m
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
  
}


## Computes the inverse of the matrix returned by the above function. If the same matrix is input again for a
## ... following iteration, it will retrieve the cached result.

cacheSolve <- function(x, ...) {
        
  m <- x$getmean()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setmean(m)
  m
}
