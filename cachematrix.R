## Create a special matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  ## Initializing the inverse
  m <- NULL
  ## setting the matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
        
  ## getting the matrix
  get <- function() {
       x
  }
  ## setting the inverse for the matrix
  setmatinv <- function(matinv) m <<- matinv
  ## getting the inverse for the matrix
  getmatinv <- function() m
  list(set = set, get = get, setmatinv = setmatinv, getmatinv = getmatinv)
}


## Computing the inverse of the special matrix returned by makeCacheMatrix function
cacheSolve <- function(x, ...) {
  ## returning matrix if inverse of x
  m <- x$getmatinv()
  ## returning inverse if already in place
  if(!is.null(m)){
    message("getting chached matrix data")
    return(m)
  }
        
  ## get matrix
  data <- x$get()
  ## calculating inverse using solve function
  m <- solve(data, ...)
  ## set inverse
  x$setmatinv(m)
  ## Return a matrix that is the inverse of 'x'
  m
}
