makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL #Calculate the inverse of the matrix
  
  #Set the Matrix as the original vector function
  
  set <- function(y){
    x <<- y 
    inv <- NULL
    
  }
  
  #Get function to handle the matrix

  get <- function() x 
  
  setInv <- function(matinv) inv <<- matinv
  
  ## Get cached matrix
  getCache <- function() inv
  
  ## Return cachedmatrix type
  list(set = set, get = get, setInv = setInv, getCache = getCache)
  
}

cacheSolve <- function(x, ...) {
  inv <- x$getCache()
  
  ## Check if matrix is cached
  if(!is.null(inv)) {
    message("get cached inverse matrix data.")
    return(inv)
  }
  ## Get matrix
  data <- x$get()
  
  # Get the inverse of our matrix
  inv <- solve(data, ...)
  
  # Set the inverse in our cache method
  x$setInv(inv)
  
  inv
  
}

