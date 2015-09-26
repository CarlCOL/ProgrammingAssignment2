## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Set functions and a matrix which will be inversed with cacheSolve function
makeCacheMatrix <- function(x = matrix()) {
  ##creates and initializes to NULL a local variable "inv" which will store a matrix object locally
  inv <- NULL
  ##This functions is used to change matrix; and reset inv (in another environment) to NULL value
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  ##This function returns value of x matrix
  get <- function(){
    x
  }
  ##this function stores inverse of x in (another environment) "inv" variable
  setinverse <- function(inverse){
    inv <<- inverse
  }
  ##this function gets the inverse of x from the "inv" variable
  getinverse <- function() {
    inv
  }
  ##This returns (to upper-level environment) a list of methods defined in this environment
  return(list(
    set = set, 
    get = get,
    setinverse = setinverse,
    getinverse = getinverse
  ))  
}


## Write a short comment describing this function
## Inverses a matrix stored in a makeCacheMatrix object
cacheSolve <- function(x, ...) {
  ##'x' is an object created with createCacheMatrix() function
  ##Checking for a square matrix
  dims<-dim(x$get())
  if(dims[1]!=dims[2]){
    ##NonSquare Matrix are not inversible, returning NaN
    warning("NonSquare Matrix are not inversible, returning NaN")
    return (NaN)
  }
  ## Getting matrix inverse value
  inv <- x$getinverse()
  ##If inv is not null, return to the caller
  if(!is.null(inv)) {
    message("getting cached data")
  }
  else{
    message("New matrix value, calculating its inverse...")
    ##Getting new matrix value and storing into "data" variable
    data <- x$get()
    if(det(data)==0){
      ##If matrix determinant is zero, this matrix is not inversible
      warning("Singular matrix error, returning NaN")
      return(NaN)
    }      
    ##Getting inverse matrix value through "solve" function
    inv <- solve(data, ...)
    ##Setting inverse matrix value into "inv" value
    x$setinverse(inv)    
  }
  ##Returning inverse matrix value
  print(inv)
  return(inv)  
}
