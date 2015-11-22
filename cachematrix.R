## x is a square invertible matrix. Here is a description on what 
##    each of the functions do:
## makeCacheMatrix: sets the matrix and gets the matrix
## cacheSolve: sets the inverse and gets the inverse

makeCacheMatrix <- function(x = matrix()) {

 inv = NULL
  set = function(b) { ## function sets inverse
    x <<- b ## sets x equal to matrix b
    inv <<- NULL
  }
  get = function() x ## function gets inverse
  setinverse = function(inverse) inv <<- inverse
  getinverse = function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)

}

## cachSolve returns the inverse of the matrix from makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
         inv = x$getinverse()
  
  if (!is.null(inv)){ ## if the inverse has already been calculated
    message("getting cached data") ## skips recomputation and retrieves inverse from cache
    return(inv)
  }
  inv_mat.data = x$get ## otherwise inverse is calculated
  inv = solve(inv_mat.data, ...)
  
  x$setinverse(inv) ##sets the value of the inverse in the cache
  
  return(inv)
}
