## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  #test if the matrix is square
  dimtest <- function(d){
    if(length(d)!=2 | d[1]!=d[2]){
      stop("matrix is not square")
    }
  }
  #set values function (based on the example)
  set <- function (y){
    dimtest(dim(y))
    x <<- y
    m <<- NULL
  }
  #get values function (based on the example)
  get <- function() {
    dimtest(dim(x))
    x
  }
  # get and set functions that already had on the example function
  setinv <- function(inv) m <<- inv
  getinv <- function () m
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  #part that tests if the values of the matrix are already "cached"
  m <- x$getinv()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  # finally proceeds with the inverse matrix function.
  data <- x$get()
  m <- solve(data, ...)
  x$set(m)
  m
}
