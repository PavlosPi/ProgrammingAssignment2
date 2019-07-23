## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##  In this function we get a matrix as an input, set a value of the matrix,
#get the value of the matrix, set the inverse Matrix and get the inverse Matrix.

#<<- operator is used to assign a value to an object in an environment that is different 
#from the current environment 

makeCacheMatrix <- function(x = matrix()) {
  inve <- NULL                            
  set <- function(y) {                
    x <<- y                 
    inve <<- NULL     
  }
  get <- function() x                
  
  setinverse <- function(inverse) inve <<- inverse 
  getinverse <- function() inve                     
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}


## Write a short comment describing this function
## In this function we take the output of the previous matrix makeCacheMatrix(matrix) as an 
# input and checks inverse matrix from makeCacheMatrix(matrix) has any value in it or not.
# If the inverse matrix from makeCacheMatrix((matrix) has not a value, it gets the original matrix data from 
# and set the invertible  matrix by using the solve function.
# But if the inverse matrix from makeCacheMatrix((matrix) has some value , 
#it returns a message and the cached object

cacheSolve <- function(x, ...) {
  
  inve <- x$getinverse()
  if(!is.null(inve)) {
    message("getting cached data")
    return(inve)
  }
  data <- x$get()
  inve <- solve(data, ...) ## Return a matrix that is the inverse of 'x'
  x$setinverse(inve)
  return(inve)
        
}
