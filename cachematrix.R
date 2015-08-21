## Put comments here that give an overall description of what your
## functions do

## this functions receives matrix as input argument and produces list of four functions 
## as output

makeCacheMatrix <- function(x = matrix()) {

  inversematrix <- NULL
  set <- function(y) {
    x <<- y
    inversematrix <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inversematrix <<- inverse
  getinverse <- function() inversematrix
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

  
  }

## this function uses output of above function as arguement and produces an inverse matrix
## the inverse matrix is calculated for the first time only and any subsequent execution bring
## out the value from the cached data only

cacheSolve <- function(x, ...) {
           ## Return a matrix that is the inverse of 'x'
      
  inversematrix <- x$getinverse()
  if(!is.null(inversematrix)) {
    message("getting cached data")
    return(inversematrix)
  }
  data <- x$get()
  inversematrix <- solve(data, ...)
  x$setinverse(inversematrix)
  inversematrix
       
         
       }

## please use the following commands for the test of above two functions
##  p1<- makeCacheMatrix(matrix(1:4,2,2))
## cacheSolve(p1) - this will compute for the first time only
## cacheSolve(p1) - this will fetch the cached data




