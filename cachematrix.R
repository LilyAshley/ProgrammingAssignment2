## Put comments here that give an overall description of what your
## functions do

## Caching the Inverse of a Matrix ##

## makeCacheMatrix creates a special "matrix" object that returns the inverse of a matrix.
## Because repeated matrix inversions can become a costly computation, 
## it may be beneficial to cache the inverse of a matrix. 
## Thus cacheSolve finds the inverse matrix from the previous function and retrieve it,
## if not, it calculates it again.

## Write a short comment describing this function

## makeCacheMatrix creates a "matrix" object 
## which is a list of functions that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  InvMatrix <- NULL
  
  set <- function(y) {
    
    x <<- y
    InvMatrix <<- NULL
    
  }
  
  get <- function()x
  
  setInverseMatrix <- function(solve)InvMatrix <<- solve
  getInverseMatrix <- function()InvMatrix
  
  list (set = set, get = get, setInverseMatrix = setInverseMatrix,
        getInverseMatrix = getInverseMatrix)

}


## Write a short comment describing this function

## cacheSolve returns the inverse of a matrix created witht makeCacheMatrix. 
## First cacheSolve checks if the inverse of the matrix has already been calculated 
## and the matrix has not changed. 
## If so, it retrieves the inverse of the matrix.
## Otherwise, cacheSolve calculates the inverse of the matrix and sets the value in the cache. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  InvMatrix <- x$getInverseMatrix()
  
  if (!is.null(InvMatrix)) {
    
    print("Retrieving cached inverse matrix")
    return(InvMatrix)
  }
  
  else {
    
    InvMatrix <- solve(x$get())
    x$setInverseMatrix(InvMatrix)
    return(InvMatrix)
  }
  
}
