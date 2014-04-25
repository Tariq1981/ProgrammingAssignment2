## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  invm<-NULL
  dd<-dim(x)
  if(dd[1]!=dd[2] ||  sum(dd)%%2!=0){  ##Check is the Matrix is Square
    stop("The Matrix must be square matrix")
  }
  
  set<-function(y){
    dd<-dim(y)
    if(dd[1]!=dd[2] ||  sum(dd)%%2!=0){  ##Check is the Matrix is Square
      stop("The Matrix must be square matrix")
    }
      
    if(is.null(x) ||
         sum(abs(dim(x)-dim(y)))!=0 ||
               sum(abs(x - y)) != 0){  ##Check if there is a difference between the dimensions of the new and the old matrix or 
			                           ##a difference between the values between the old and new matrix
           x<<-y
           invm<<-NULL
        
    }
    
  }
  get<-function() x  ## Return the saved matrix
  setinvm<-function(invmat){  ## saving the inverse matrix
    invm<<-invmat
  }
  getinvm<-function() invm  ## Return the saved inverse matrix
  list(set = set, get = get,setinvm = setinvm,getinvm = getinvm)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		## Return the inverse matrix if exists or calculate it and save it.
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
