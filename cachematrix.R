## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  # Initialize NULL matrix
  inverseMat <- NULL
  
  # Function t set value
  setMat <- function(y){
    x <<- y
    inverseMat <<- NULL
  }
  
  # This function returns the matrix itself
  getMat <- function() x
  # This function stores the calculated inverse
  setInv <- function(inverse) inverseMat <<- inverse
  # This function retrievs the cached inverse
  getInv <- function() inverseMat
  # Return this "special" matrix
  list(setMat = setMat, getMat = getMat, setInv = setInv, getInv = getInv)
  
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  
  # Get the stored value from the special matrix.
  inverseMat <- x$getinv()
  # The value is not NULL indicating that the value have already calculated. 
  if (!is.null(inverseMat)){
    message("getting cached data")
    return(inverseMat)
  }
  # The value is NULL, calculate it and store it in the special matrix
  data <- x$getMat()
  inverseMat <- solve(data)
  x$setInv(inverseMat)
  inverseMat
  ## Return a matrix that is the inverse of 'x'
}
