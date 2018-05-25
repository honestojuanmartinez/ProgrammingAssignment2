

## Store the original matrix into a list structure that also wil contain the inverse 
## of the original matrix after the calculation

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setSolve <- function(solve) m <<- solve
  getSolve <- function() m
  list(set = set, get = get,
       setSolve = setSolve,
       getSolve = getSolve)

}


## This function test if the inverse of the matrix is stored in cache, in thios case return the stored inverse
## In the other case calculate the inverse, print the result and store the inverse in cache

cacheSolve <- function(x, ...) {
          ## Return a matrix that is the inverse of 'x'
  m <- x$getSolve()
  if(!is.null(m)) {
    message("getting cached inverse")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setSolve(m)
  m
}
