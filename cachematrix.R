## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
m1 <- matrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol = 2) #test matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
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


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}

myMatrix <- makeCacheMatrix(m1) #passes test matrix to makeCacheMatrix function to create
                                #myMatrix as an makeCacheMatrix() object with x set to m1
myMatrix$get()                  #this returns the matrix that has been passed 
myMatrix$getsolve()             #this returns an error until myMatrix is passed to cacheSolve
                                #after cacheSolve has been run, this returns the inverse of m1
cacheSolve(myMatrix)            #returns the inverse of the matrix m1
