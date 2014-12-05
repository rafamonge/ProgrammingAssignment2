#Returns: List with the following items
#set: set the value of the matrix
#get: get the value of the matrix
#setInverse: set the value of the inverse of the matrix
#getInverse: get the value of the inverse of the matrix

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

#Description: Calculates the inverseMatrix for a makeCacheMatrix object. If the object already has the inverse in a cache, it returns the cached inverse.
#Input:
#x: An object of type makeCacheMatrix
#Returns: the inverse of the matrix

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


# 2 sample invertible matrixes so you can test the code below
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
