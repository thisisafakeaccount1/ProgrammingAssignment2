## Below are two functions that are used to create a special object that stores a matrix and cache's its inverse.

## The first function makeCacheMatrix creates a special "matrix" object
## that can cache its inverse. 

makeCacheMatrix <- function(x = matrix()) {
  inv_mat <<- NULL
  set <- function(y){
    x <<- y
    inv_mat <<- NULL
  }
  get <- function()x
  setinv <- function(inv) inv_mat <<- inv
  getinv <- function() inv_mat
  
  list(set = set, get = get, setinv = setinv, getinv=getinv)
}


## The second function cacheSolve computes the inverse of the "matrix" 
## returned by makeCacheMatrix above. 
## If the inverse has already been calculated and the matrix hasn't changed, 
## then the inverse will be retrieved from the cache. 
## Otherwise, the inverse will be recalculated

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv_mat <- x$getinv()
  if (!is.null(inv_mat)){
    message("getting cached data")
    return(inv_mat)
  }
  data <- x$get()
  inv_mat <- solve(data,...)
  x$setinv(inv_mat)
  inv_mat
}
