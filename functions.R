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


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
    }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
             setsolve = setsolve,
getsolve = getsolve)
}


cacheSolve <- function(x, ...) {
    +x$getsolve()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
      }
    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m)
    m
}
