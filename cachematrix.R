makeCacheMatrix <- function(x = matrix()) {
  # Initializing
  i <- NULL
  
  ## Setting the Matrix
  set <- function( matrix ) {
    m <<- matrix
    i <<- NULL
  }
  
  # Getting the Matrix
  get <- function() {
    m
  }
  
  # Setting inverse of matrix
  setInverse <- function(inverse) {
    i <<- inverse
  }
  
  # Getting inverse 
  getInverse <- function() {
    i
  }
  
  # Return a list of the methods
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}




cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  
  ##Return the inverse if its already set
  if( !is.null(m) ) {
    message("getting cached data")
    return(m)
  }
  
  
  data <- x$get()
  
  # Calculating Inverse
  m <- solve(data) %*% data
  
  ##Setting Inverse
  x$setInverse(m)
  
  ## Return the matrix
  m
}
