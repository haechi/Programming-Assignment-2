## "makeCacheMatrix" creates a list of functions which return a stored 
## matrix and the stored inverse of that matrix. 

## "cacheSolve" checks is the inverse is cached in the supplied object 
## and returns the cached inverese if the object is not changed, 
## otherwise its computes the inverse using the solve() function 

## Functionallity can be checked by using the following example inputs

## > x <- makeCacheMatrix()
## > x$set(matrix(1:4, nrow = 2, ncol = 2))
## > x$get()
## > cacheSolve(x)
## > cacheSolve(x)

## > x$set(matrix(5:8, nrow = 2, ncol = 2))
## > x$get()
## > cacheSolve(x)
## > cacheSolve(x)

makeCacheMatrix <- function(x = matrix()) {
  ## creates a special "matrix" object that can cache its inverse.
  ## it is assumed that the matrix supplied is always invertible.
  
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

cacheSolve <- function(x, ...) {
  ## computes the inverse of the special "matrix" returned by makeCacheMatrix.
  ## if inverse has already been calculated (and the matrix has not changed), 
  ## then cacheSolve retrieves the inverse from the cache.
  
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached inverse")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
