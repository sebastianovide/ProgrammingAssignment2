## makeCacheMatrix creates an object thant contains a matrix and a couple
## of getters ans setters.  cacheSolve read the inverse from the matrix 
## passed as parameters and if it NULL it calcuates it, otherwise it just
## returns it.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  im <- NULL
  set <- function(m) {
    x <<- m
    im <<- NULL
  }
  get <- function() x
  setInverse <- function(imt) im <<- imt
  getInverse <- function() im
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then cacheSolve should retrieve the inverse from the 
## cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  im <- x$getInverse()
  if(!is.null(im)) {
    message("getting cached data")
    return(im)
  }
  data <- x$get()
  im <- solve(data, ...)
  x$setInverse(im)
  im  
}
