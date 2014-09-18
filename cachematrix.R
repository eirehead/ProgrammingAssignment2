## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  ## get the value of the matrix
  get<-function() x
  
  ## set the inverse of the matrix
  setmatrix<-function(solve) m<<- solve
  getmatrix<-function() m
  
  ## get the inverse of the matrix
  list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)
}

cacheSolve <- function(x=matrix(), ...) {
  ## Return a matrix that is the inverse of 'x'
  
  ## get the inverse of the matrix
  m<-x$getmatrix()
  
  ## check if there is a matrix
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  ## if not: get the inverse of the matrix
  matrix<-x$get()
  m<-solve(matrix, ...)
  ## set the inverse of the matrix
  x$setmatrix(m)
  ## return the matrix
  m
}

#for checking
mat <- matrix(data = c(4,2,7,6), nrow = 2, ncol = 2)
mat2 <- makeCacheMatrix(mat)
cacheSolve(mat2)