##The objectives of these functions are to calculate the 
##inverse of a matrix and catch it. It would make it possible
##to access the inverse of the matrix without the necessity to 
##compute it again. 


## This function creates a list of functions that: 
##sets the matrix (l$set(y)) "y" needs to have a length that can create a square matrix
##gets the matrix (l$get())
##sets the inverse of the matrix (l$setinverse())
##gets the inverse of the matrix (l$getinverse())

makeCacheMatrix <- function(x = matrix()) {
  i<-NULL
  
  set <- function(y) {
    l<-length(y)
    x<<-matrix(y,sqrt(l))
    i<<-NULL
    
  }
  get<-function(){
    x
  }
  setinverse<-function(){ 
    i<<-solve(x)
  }
  getinverse<<-function() {i}
  l<<-list(set = set, get = get,
            setinverse = setinverse,
            getinverse = getinverse)
}


## This function gets the inverse matrix caught or calculates it from the matrix
## inputted on the first function.

cacheSolve <- function() {
  i <- l$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- l$get()
  i <- solve(data)
  l$setinverse()
  i
}
