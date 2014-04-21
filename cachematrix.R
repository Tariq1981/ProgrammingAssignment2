## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  invm<-NULL
  dd<-dim(x)
  if(dd[1]!=dd[2] ||  sum(dd)%%2!=0){
    stop("The Matrix must be square matrix")
  }
  
  set<-function(y){
    dd<-dim(y)
    if(dd[1]!=dd[2] ||  sum(dd)%%2!=0){
      stop("The Matrix must be square matrix")
    }
      
    if(is.null(x) ||
         sum(abs(dim(x)-dim(y)))!=0 ||
               sum(abs(x - y)) != 0){
           x<<-y
           invm<<-NULL
        
    }
    
  }
  get<-function() x
  setinvm<-function(invmat){
    invm<<-invmat
  }
  getinvm<-function() invm
  list(set = set, get = get,setinvm = setinvm,getinvm = getinvm)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m<-x$getinvm()
  if(!is.null(m)){
    message("getting cashed data")
    return(m)
  } 
  data<-x$get()
  m<-solve(data,...)
  x$setinvm(m)
  m		
}
