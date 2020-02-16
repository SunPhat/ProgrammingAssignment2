#In my code, "matlib" package is used to calculate inverse matrix by calling Inv().
#In makeCacheMatrix function, I try to set the value of the matrix and get the value of the matrix
#after that, I set the value of inverse matrix and get the inverse matrix value.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(Inv) m <<- Inv
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

#cacheSolve function is used to check that if the inverse matrix has already been calculated.
#If so, the function will return the value from catched data. 
#Otherwise, Inverse matrix need to be computed and the new value will be returned.

cacheSolve <- function(x, ...) {
  m <- x$getInverse()
  if(!is.null(m)) {
    message("Yeah! there are the value in cached data so the inverse matrix don't need to be computed")
    return(m)  #return the value of the inverse of 'x' from catched data
  }
  data <- x$get()
  m <- inv(data, ...)
  x$setInverse(m)
  m #return new value of the inverse of 'x' from computation
}