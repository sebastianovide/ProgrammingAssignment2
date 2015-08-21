## makeCacheMatrix creates an object thant contains a matrix, its inverse and a 
## couple of getters ans setters.  cacheSolve read the inverse from the matrix 
## passed as parameters and if it NULL it calcuates it, otherwise it just
## returns it.
## Example :
## m <- matrix(rnorm(16), ncol=4)
## x <- makeCacheMatrix(m)
## i <- cacheSolve(x)
## all(solve(m) == i)

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  im <- NULL
  
  ## every time the matrix is set, the cached value is deleted (NULL)
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
  
  ## NULL means that the inverse need to be calculated
  if(is.null(im)) {
    data <- x$get()
    im <- solve(data, ...)
    
    ## cache the inverse 
    x$setInverse(im)
  } else {
    ## no need to calculate it
    message("getting cached data")
  }
  
  im  
}
