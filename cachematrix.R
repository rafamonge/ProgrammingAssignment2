#SampleMatrix A & B
A = matrix( 
     c(1,0,1,1), # the data elements 
     nrow=2,              # number of rows 
     ncol=2,              # number of columns 
     byrow = TRUE)        # fill matrix by rows 

B = matrix( 
  c(1,0,0,0,1,0,0,1,1), # the data elements 
  nrow=3,              # number of rows 
  ncol=3,              # number of columns 
  byrow = TRUE)        # fill matrix by rows 


makeCacheMatrix <- function(x = matrix()) {
  inverseMatrix <- NULL
  set <- function(newMatrix){
    x<<-newMatrix
    inverseMatrix <<- NULL
  }
  get <- function() x
  setInverse <- function(newInverse) inverseMatrix <<- newInverse
  getInverse <- function() inverseMatrix
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)

}


cacheSolve <- function(x, ...) {
  inverse <- x$getInverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data,...)
  x$setInverse(inverse)
  inverse
}