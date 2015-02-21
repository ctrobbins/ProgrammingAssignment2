## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix creates a list containing functions to store a matrix and it's inverse
## to use assign the makeCacheMatrix list to a variable (eg. "a <- makeCacheMatrix()")
## then you can save a square matrix into the new function (eg. "a$set(matrix(1:4,2,2))")

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y) {
    x<<-y
    m<<-NULL
  }
  get <- function() x
  setinverse<- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get, 
      setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve returns the inverse of a matrix stored using makeCacheMatrix above (eg. "y<-cacheSolve(a)")
## cacheSolve first checks if the inverse has already been calculated and stored in the special "a" list created by makeCacheMatrix
## if inverse already exists then answer is returned from cached inverse, otherwise it calculates the inverse, stores it for future, and returns inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}