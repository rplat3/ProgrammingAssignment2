## The makeCacheMatrix function creates a matrix object capable of caching its inverse.
## The cacheSolve function coputes the inverse of the matrix if it hasn't already been 
## calculated and the matrix hasn't changed

## combination of 4 functions: set,get,setinverse,getinverse

makeCacheMatrix <- function(x = matrix()) {
m=NULL
set=function(y){
  x<<- y
  m<<- NULL
  }
get=function() x
setinverse=function(solve) m<<-solve
getinverse=function() m
list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## if the inverse has already been calculated it retrives it. If not "data" gets the matrix
## "m" calculates the inverse, and "x$setinverse(m)" stores it. 

cacheSolve <- function(x, ...) {
m=x$getinverse()
if(!is.null(m)){
  message("getting cached data")
  return(m)
}
data=x$get()
m=solve(data,...)
x$setinverse(m)
m
}
