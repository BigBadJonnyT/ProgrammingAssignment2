## makeCacheMatrix initially creates an empty matrix, within the function are stored 4 sub functions that allow you
## to you to set and retrieve a cached matrix, and set and retrieve a cached inverse of that matrix
## If testing you must ensure the matrix can be inversed, meaning for this function the matrix must be square and 
## must not be sequential numbers. For example:
## myMatrix <- matrix(c(1,5,9,23,50,107,98,25,7), nrow = 3, ncol = 3)

## $Set = Allows you to set or reset the matrix held in the cache. Upon setting a new matrix, any cached inverse of a 
## previous matrix is removed
## $get = Retrieves the current cached matrix
## $setInverse = Allows you to cache the inverse of the currently cached matrix. The arguement "inverse" represents the
## cacheSolve function.
## $getInverse = Retrieves the cached inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function(){
    x
  }
  setInverse <- function(z){
    m <<- z
  }
  getInverse <- function(){
    m
  }
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function is used in conjunction with the $setInverse function and checks if there is a cached inverse matrix
## If an inverse matrix has been cached it returns the cached value, otherwise it calculates the 
## inverse of the currently stored matrix, caches it and returns the inversed matrix

cacheSolve <- function(x, ...) {
  z <- x$getInverse()
  if(!is.null(z)){
    print("Retrieving Cache")
    print(z)
    return(z)
  }else{
    data <- solve(x$get())
    x$setInverse(data)
    final <- (x$getInverse())
    return(final)
  }
}
