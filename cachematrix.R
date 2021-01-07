## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#pair of functions that cache the inverse of a matrix

library(MASS) #to solve for inverse of both squared and non squared matrices 
makeCacheMatrix <- function(x = matrix()) { #makeCacheMatrix consists of set, inv, setinv, and get inv
  inv<-NULL #to set inverse as NULL
  set<-function(y){
    x<<-y
    inv<<-NULL
  }
  get<-function()x #to get matrix x
  setinv <- function(inverse) {inv <<- inverse}
  getinv <- function() {
    inver<-ginv(x)
    inver%*%x #to get inverse of matrix
  }
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  inv<-x$getinv()
  if(!is.null(inv)){ #to check if inverse is NULL
    message("getting cached data.")
    return(inv) #to return inverse value
  }
  data<-x$get()
  inv<-solve(data, ...) #to solve inverse value
  x$setinv(inv)
  inv
        ## Return a matrix that is the inverse of 'x'
}
