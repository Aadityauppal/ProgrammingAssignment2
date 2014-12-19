## makeCacheMatrix creates a matrix object that can cache its inverse
## cacheSolve computes the inverse of the matrix returned by makeCacheMatrix
## If the inverse has already been calculated then the cacheSolve retrieves
## the inverse from the cache.

## Takes values and size of a matrix to return a special
## "matrix" object that can cache its inverse

makeCacheMatrix<-function(x=matrix()){##Defines x as matrix
  n<-NULL                             ##Sets initial value of n as NULL 
  set<-function(y){                   ##In 2nd environment, assigns 
    x<<-y                             ##matrix x to set
    n<<-NULL                          ##default value of n as NULL
  }
  get<-function() x                     ##gets value of x from 1st environment
  setinverse<-function(solve) n<<-solve ##calculates and sets inverse 
  ##as m in 2nd environment
  getinverse<-function() n            ##gets inverse from n
  list(set=set,get=get,               ##creates list vector with 4 variables
       setinverse=setinverse,getinverse=getinverse)
}

## This function computes the inverse of the matrix returned by makeCacheMatrix
## If the inverse has already been calculated then the cacheSolve retrieves
## the inverse from the cache.

cacheSolve <- function(x, ...) {
  n <- x$getinverse()               ## Gets value of n from getinverse
  if(!is.null(n)) {                 
    message("getting cached data")
    return(n)
  }
  data <- x$get()                   ## If NULL, gets matrix and solves for inverse
  n <- solve(data, ...)
  x$setinverse(n)                   ## Inverse fed into setinverse 
  n                                 ## Returns a matrix that is the inverse of 'x'
}
