## These pair of functions both calculate and cache 
## the inverse of an invertible matrix

#  Example:
#   
# > mx <- matrix(c(1,2,3,4), nrow=2, ncol=2)
# > fmx <- makeCacheMatrix(mx)
# > cacheSolve(fmx)
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
# > cacheSolve(fmx)
# getting cached data
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
# > 


## Creates an encapsulted matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  
  # init local member
  m <- NULL
  
  # init local cached values
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  # get the value of the original matrix passed in
  get <- function() x
  
  # obtain the inverse of the matrix
  setsolve <- function(solve) m <<- solve
  
  # return the inversed matrix
  getsolve <- function() m
  
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## Computes the inverse of a makeCacheMatrix object and 
## sets the result within the makeCacheMatrix object

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  #  attempt to get a chached copy of the result
  m <- x$getsolve()
  
  # check for existance of data, if present, return the cached result
  if(!is.null(m)) {   
    message("getting cached data")
    return(m)         
  }
  
  # get original data from encapsulation
  data <- x$get()
  
  # preform inversion
  m <- solve(data, ...)
  
  # encapsulte result back into makeCacheMatrix function
  x$setsolve(m)
  
  # return result
  m
}
