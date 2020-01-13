add2 <- function (x,y) {
    x + y
}

above10 <- function(x) {
  use <- x > 10
  x[use]
}

above <- function(x,n) {
  use <- x > n
  x[use]
}

above <- function(x,n = 10) {
  use <- x > n
  x[use]
}

col_mean <- function (x, removeNA = TRUE) {
  nc <- ncol(x)
  means <- numeric(nc)
  for (i in 1:nc ){
    means[i] <- mean(x[,i], na.rm = removeNA)
  }
  means
}

#make an object that contains a matrix and can perform operations on that matrix
makeCacheMatrix <- function(inputMatrix = matrix()) {
  
  # This is where we store the cached value once the computation is done
  cachedMatrix <- NULL
  
  # set the value - ie perform the 'set' function
  set <- function(y) {
    inputMatrix <<- y
    cachedMatrix <<- NULL
  }
  
  # return the original
  get <- function() inputMatrix
  
  # set the inverse matrix
  setsolve <- function(solve) cachedMatrix <<- solve
  
  # get the inverse matrix
  getsolve <- function() cachedMatrix
  
  # create a list of things we can to with the variable
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

cacheSolve <- function(inputMatrix, ...) {
  
  #set the cached matrix to be the soluion for solving the inverse of the matrix 
  #   (which may not have happend yet, and so could be NULL)
  cachedMatrix <- inputMatrix$getsolve()
  
  #if the solution is already there, tell the user and return the solution
  if(!is.null(cachedMatrix)) {
    
    message("getting cached data")
    return(cachedMatrix)
    
  }
  
  #otherwise, set data to be the input matrix, create the inverse and return it
  else {
    #doing both of the next two steps in is not needed but makes it clearer
    data <- inputMatrix$get()
    cachedMatrix <- solve(data, ...)
    
    #could have been just:
    #cachedMatrix <- solve(inputMatrix$get(), ...)
    
    inputMatrix$setsolve(cachedMatrix)
    
    #again, could just be:
    #inputMatrix$setsolve(solve(inputMatrix$get(), ...))
    
    return(cachedMatrix)
    
  }
}