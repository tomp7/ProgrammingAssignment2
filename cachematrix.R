## Programming Week 3 Assignment 

## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## Creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) 
{
  ## 'i' is the inverse property 
  i <- NULL
  
  ## Setting the matrix 
  set <- function(y) 
  {
    x <<- y
    i <<- NULL
  }
  
  ## Getting the matrix 
  get <- function() x
  
  ## Setting inverse of matrix
  setinverse <- function(inverse) i <<- inverse
  
  ## Actually getting the inverse of the matrix 
  getinverse <- function() i
  
  ## List of methods returned 
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above unless the
## inverse has already been calculated and the matrix has not been changed 
cacheSolve <- function(x, ...) 
{
  ## Getting a matrix which is the inverse 
  i <- x$getinverse()
  
  ## If already set, returning cached inverse with a message 
  if (!is.null(i)) 
  {
    message("getting cached data")
    return(i)
  }
  
  ## Get the matrix 
  data <- x$get()
  i <- solve(data, ...)
  
  ## Setting the inverse of 'x' to an object 
  x$setinverse(i)
  
  ## Return a matrix that is the inverse of 'x'
  i
}
