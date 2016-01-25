## This function is for calculating the inverse of a matrix
## functions do take a Matrix and return (set) and can
## Return a inverse (getinv) or return matrix (get)

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  #m <- matrix(c(-1, -2, 1, 1), 2,2)
  #x <- makeCacheMatrix(m)
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv) 
}


## This function return a inverse matrix
## if Matrix is null return "getting cached data"

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
