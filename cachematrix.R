## Put comments here that give an overall description of what your
## functions do

## This Function will define the functions for storing/retreiving a matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
  #Define the Inverse field and set to NULL
  i <- NULL
  
  #Define the Set Function for Matrix
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  #Define the Get Function for Matrix
  get <- function() x
  
  #Define the Set Function for Inverse
  setinv <- function(inv) i <<- inv
  
  #Define the Get Function for Inverse
  getinv <- function() i
  
  #Return a list with all the set and get functions
  list(set = set, get = get, setinv = setinv, getinv = getinv)
  
}


## This Function will calculate the inverse of a matrix and returns it if it not is already calcualted and cached.

cacheSolve <- function(x, ...) {
  #Get the Inverse Matrix 
  i <- x$getinv()
  
  #If the inverse was already present, return the cahed result 
  if(!is.null(i)) {
    message("getting cached inverse matrix")
    return(i)
  }
  
  #Get the Matrix if the inverse was not present
  data <- x$get()
  
  #Derive the Inverse of Matrix
  i <- solve(data, ...)
  
  #Store the Inverse in Cache
  x$setinv(i)
  
  #Print the Inverse
  i
}
