## the functions makecacheMatrix and cacheSolve calculate the inverse of a 
## matrix without calculating it more than once as the result is stored in
## the cache. This exercise is to practice the scoping rules in R
## the functions would be called as below: (with the results)
## > aMatrix<-makecacheMatrix(matrix(c(2,0,0,2), nrow=2,ncol=2))
## > cacheSolve(aMatrix)
##       [,1] [,2]
## [1,]  0.5  0.0
## [2,]  0.0  0.5
## > cacheSolve(aMatrix)
## getting cached data
##       [,1] [,2]
## [1,]  0.5  0.0
## [2,]  0.0  0.5
##

## makecacheMatrix puts the input matrix x in the list format, 
## but does not calculate the inverse 


makecacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}



cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  ## x has to be in format makecacheMatrix
  ## it will give the message "getting cache data" if it has already been calculated
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}