## makeCacheMatrix() function receives a matrix that is assumed to be invertible and returns a matrix object that is a list of four functions that provide access to the matrix and its inverse and enable the user to modify them.

## Given a matrix object, if its inverse is not already calculated, the cacheSolve() function can be applied to it to find the inverse. In order to avoid extra computations, whenever the cacheSolve() function is applied to a matrix object, it first calls the getinverse() function to check if there exists any matrix as the inverse of the matrix object. If nothing is found, it then proceeds to calculate the inverse function by calling the function solve() and stores the returned matrix in the inverse vector associated with the matrix object.

## This function creates a matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(m){
    x <<- m
    inverse <<- NULL
  }
  setinverse <- function(inv) inverse <<- inv
  get <- function() x
  getinverse <- function() inverse
  
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function computes the inverse of the matrix object returned by makeCacheMatrix

cacheSolve <- function(x) {
  inv <- x$getinverse()
  if(!is.null(inv)){
    print("getting cached inverse of the matrix")
    return(inv)
  }
  
  matrix <- x$get()
  inv <- solve(matrix)
  x$setinverse(inv)
  inv
}
