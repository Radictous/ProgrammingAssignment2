## a<-makeCacheMatrix() this command creates an object named "a" and calls the
## makeCacheMatrix() function.This function creates the matrix "x" inside the object ##"a".If we included an argument when we called the function (i.e.: ## a <- makeCacheMatrix(1:4,2,2), the object "a" would get a matrix "x" that contained ## numbers 1,2,3,4 ; since the argument in the function defaults to matrix(), if we 
## don't pass an argument then x is created as an empty matrix.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

# When you call the function and don't reference any of the subfunctions , it executes # the code that is not in any of the subfunctions. In this case it sets m to NULL: m is # cache for this particular object. So the result is that at the end of this command,
# "a" is an object that contains an empty matrix, has an internal variable m which is
# set to NULL, and has four subfunctions that can be called to work on it (set(), get# (), setinverse(), and getinverse())

cacheSolve <- function(x, ...) {

## Return a matrix that is the inverse of 'x'
# functions
# a$set(matrix(1:4,2,2))  #set the matrix
# a$get()                 #get the matrix 
# cachesolve(a)           #calculate the inverse of the matrix 
# cachesolve(a)           #when is called back use the cached inverse of the matrix  

  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m}
# Part of the information used to explain this function is based on a post writted by
# James A.  Stephenson https://class.coursera.org/rprog-003/forum/profile?
# user_id=283808