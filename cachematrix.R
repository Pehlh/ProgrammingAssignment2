## This pair of functions allows you to calculate and cache the 
## inverse of a matrix so as to reduce runtime for recalculations

## The first function makeCacheMatrix creates a special "matrix" 
## object that can cache its inverse.
## Enter "x <- makeCacheMatrix (matrix(elements, nrow=#,ncol=#))" 
## to generate the special matrix
## The following functions will be stored withing the first function:
## x$get() will return the matrix created
## X$set() will allow you to set the value of the matrix to something new
## x$getinverse() will return the inverse of the matrix
## X$set inverse() will allow you to set the value of the inverse matrix 
## to something new

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <-function(y){
    x <<-y
    i <<- NULL
  }
  get <-function()x
  setinverse <- function(inverse) i<<- inverse
  getinverse <-function()i
  list (set = set, get = get, 
        setinverse = setinverse, 
        getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has 
## already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
## Enter cacheSolve(a) to retrieve the inverse matrix from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)){
    message("getting cached data")
    return (i)
  }
  data <-x$get()
  i <-solve(data,...)
  x$setinverse(i)
  i
}
