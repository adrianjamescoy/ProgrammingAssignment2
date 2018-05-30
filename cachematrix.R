# Calculating the inverse of a matrix, whereby code stores values of inverted matrix to avoid unnecessary reprocessing. 

# Create inverse of the matrix 

makeCacheMatrix <- function(x = matrix()) {
  # Create inverse of the matrix 
  
  inv <- NULL           ## ensure inverse empty
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, 
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}

#Create the inverse as a prior calculation

cacheSolve <- function(x, ...) {    #create matrix that is the inverse of x
  inv <- x$getinverse()
  if(!is.null(inv)) {               #does inverse contains any value already?
    message("getting cached data")
    return(inv)
  }
  data <- x$get()                   
  inv <- solve(data, ...)           #creates data of the inverse matrix
  x$setinverse(inv)
  return(inv)                       #returns the inverse matrix value
}

#Create the inverse as a prior calculation

cacheSolve <- function(x, ...) {    #create matrix that is the inverse of x
  inv <- x$getinverse()
  if(!is.null(inv)) {               #does inverse contains any value already?
    message("getting cached data")
    return(inv)
  }
  data <- x$get()                   
  inv <- solve(data, ...)           #creates data of the inverse matrix
  x$setinverse(inv)
  return(inv)                       #returns the inverse matrix value
}
