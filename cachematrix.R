## this is a test matrix that is being created to test the two functions later on. 
## This test matrix was taken from the discussion forum for week 3 where we were
## encouraged to use a test case and also as linear algebra is not an official
## pre-requisite for the class it is not assumed that we know how to construct a
## squared matrix that can be inverted

m1 <- matrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol = 2)

## These two functions work together to first cache the value of a matrix and then
## calculate the inverse of that matrix.  The first function sets both of the necessary
## objects as well as their accessor methods and then creates a named list of the
## four functions to make it possible to use the $ extractor command later on.

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


## The second function takes the object created by the first function and returns the
## inverse of the matrix after testing to see if an inverse matrix has already been
## cached.

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
myMatrix$set()                  #if you wanted to, you could set a new matrix to the environment
                                #here
cacheSolve(myMatrix)            #returns the inverse of the matrix m1 (in this case)
