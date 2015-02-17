## Michael Bebjak
## cachematrix.r
## Feb 2015
## Function to cache a matrix and then compute its inverse

## makeCacheMatrix is a function that takes a matrix and caches it

makeCacheMatrix <- function(x = matrix()) {
  # initialize a variable that will hold the matrix's inverse
  i <- NULL
  
  ## function to place the matrix into a cached object 
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  ## function to retrive the cached matrix
  get <- function() x
  
  ## function to set the inverse of cached matrix x 
  ## by solving it and assigning it to i
  setInverse <- function(solve) i <<- solve
  
  ## function to retrieve the inverse of cached object x
  getInverse <- function() i
  
  ## return a list with the values of set, get, setInverse, 
  ## and getInverse functions
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)  
}

## cacheSolve takes a matrix x that has been cached 
## (ie. it takes a cached object made using the makeCacheMatrix
## and computes its inverse if it has not already been computed

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  # assign the inverse of cached object x to i
  i <- x$getInverse()
  
  ## then check if the inverse in the cache exists
  ## and if it does, return the cached inverse
  if(!is.null(i)) {
    message("retrieving cached matrix inverse...")
    return(i)
  }
  
  ## otherwise, assign the cached x to a temp variable
  ## data, solve the matrix in data and assign to i
  data <- x$get()
  i <- solve(data, ...)
  
  ## set i to be the inverse of cached object x
  ## and return it
  x$setInverse(i)
  return(i)
}
