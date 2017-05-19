## makeCacheMatrix functon takes a matrix variable as an argument
## It contains set, get, setInverse and getInverse functions
## Set assigns the passed matrix to the internal variable and clears the cached inverse matrix variable
## Get returns the stored matrix
## SetInverse assigns the passed matrix to the inverse matrix variable
## GetInverse returns the stored inverse matrix

makeCacheMatrix <- function(m = matrix())
{
  inverseMatrix <- NULL
  
  set <- function(x)
  {
    m <<- x
    inverseMatrix <<- NULL
  }
  
  get <- function()
  {
    m
  }
  
  setInverse <- function(inverse)
  {
    inverseMatrix <<- inverse
  }
  
  getInverse <- function()
  {
    inverseMatrix
  }
  
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
  
}


## cacheSolve function takes a matrix argument and returns the inverse of that matrix
## It assumes that the matrix is convertable
## It checks if an inverse matrix is cached and if it is returns this value
## If no inverse matrix is cached it  uses solve to inverted the matrix and stores this value
cacheSolve <- function(m,...)
{
  inverseMatric <- m$getInverse()
  
  if(is.null(inverseMatric))
  {
    message("No cached data to retrieve")
  }
  else
  {
    message("Returning retrieved cached data")
    return(inverseMatric)
  }
  
  data <- m$get()
  inverseMatric <- solve(data)
  
  m$setInverse(inverseMatric)
  inverseMatric
  
}
