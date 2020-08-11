## Programming Assignment 2 by Nikitha 

## The function makeCacheMatrix, takes a matrix x and creates a matrix that can
## cache it's inverse. 
## The code was simple enough to write using the example of cache mean, and editing 
## mean to inverse and changing some of the variable names.

makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set<-function(y){
      x<<-y
      inv<<-NULL
  }
  get<-function()x
  setinv<-function(solve)inv<<-solve
  getinv<-function()inv
  list(set=set,get=get,
       setinv=setinv,
       getinv=getinv)
}

## The function cacheSolve, takes a matrix x and returns the inverse of the matrix.
## It checks if there's cached inverse and if there is, it prints out "Using cached data". 
## If there isn't, it prints out "Computing inverse for the first time"
## The code was simple enough to write using the example of cache mean, and editing 
## mean to inverse and changing some of the variable names.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv<-x$getinv()
  if(!is.null(inv)){
      message("Using cached data")
      return(inv)
  }
  message("Computing Inverse for the first time")
  data<-x$get()
  inv<-solve(data, ...)
  x$setinv(inv)
  inv
}
