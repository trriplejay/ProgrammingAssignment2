## makeCacheMatrix returns a list of callable functions to modify the state
## of the makeCacheMatrix object.  These functions include:
##  set: sets the value of the original matrix
##  get: returns the original matrix
##  setinv: sets the value of the inverted version of the original matrix
##  getinv: returns the inverted value of the original matrix
makeCacheMatrix <- function(theMat = matrix()) {
  
  inverseMatrix <- NULL
  set <- function(newMat) {
    theMat <<- newMat
    inverseMatrix <<- NULL
  }
  get <- function() theMat
  setinv <- function(inverse) inverseMatrix <<- inverse
  getinv <- function() inverseMatrix
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## cacheSolve takes in a "makeCacheMatrix" object and attempts to calculate
## the inverse of the matrix that it contains.  First it checks to see if
## the inverse value already exists.  If it exists, return it.  If the inverse
## is not already stored, then calculate it using the "solve" function, then
## store the result, and return it.

cacheSolve <- function(mcm, ...) {

  inverseMatrix <- mcm$getinv()
  if(!is.null(inverseMatrix)) {
    message("getting cached data")
    return(inverseMatrix)
  }
  else{
    matrix <- mcm$get()                        #get the current value of x (the original matrix) and store it
    inverseMatrix <- solve(matrix, ...)      #solve the matrix
    message("setting cached data")
    mcm$setinv(inverseMatrix)                  #set the cached inverse version of the matrix
    return (inverseMatrix)                   #return the inverted matrix
  }
  
}
