## Put comments here that give an overall description of what your
## functions do
##The functions are designed to find the inverse of a matrix 
##and cache it so that it can be used again if matrix is unaltered

## Write a short comment describing this function
##makeCacheMatrix is used to create a matrix object to 
##cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set<-function(y){
    x<<-y
    inv<<-NULL
  }
  get<-function() x
  setinverse<-function(inver) inv<<-inver
  getinverse<-function() inv
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)

}


## Write a short comment describing this function
##Computes the inverse of matrix returned by the makeCacheMatrix function
##If inverse has already been calculated and
##matrix is unchanged it returns inverse from cache
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv<-x$getinverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data<-x$get()
  inv<-solve(data,...)
  x$setinverse(inv)
  inv
}
