## Put comments here that give an overall description of what your
## functions do

##you can test the functions from the following code:
#m1 <- matrix(c(1,2,3,4), nrow=2, ncol=2)
#m1
#r1 <- makeCacheMatrix(m1)
#cacheSolve(r1)

## Write a short comment describing this function
## This function creates a special "matrix" object 
## that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(calcInv) m <<- calcInv
  getinv <- function() m
  list(set = set, get = get, 
       setinv = setinv, 
       getinv = getinv)
}


## Write a short comment describing this function
## This function computes the inverse of the special "matrix" returned by the makeCacheMatrix. 
## If the inverse has already been calculated (and the matrix has not changed), 
## the cachesolve retrieves the inverse from the cache.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinv(m)
  m      
}
