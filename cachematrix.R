## This function creates object-matrix, that can cache its inverse  
## it calculates the inverse of the matrix and if it finds out it has already been computed it 
## retrieves the inverse from the cache,
##which saves time in comparison to repeated canculationsof matrix inversion

## this function list sets the value of the matrix, gets the value of the matrix, 
## and it sets and gets the inverse of the matrix that can be cached


makeCacheMatrix <- function(x = matrix()) {
m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setinverse<-function(solve) m<<- solve
  getinverse<-function() m
  list(set=set, get=get,
       setinverse=setinverse,
       getinverse=getinverse)
}


## this function checks if the inverse of the matrix above has already been calculated
## if so it retrieves the inverse from the cache, ig not it computes the matrix inverse 

cacheSolve <- function(x=matrix(), ...) {
  m<-x$getinverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data<-x$get
  m<-solve(data, ...)
  x$setinverse(m)
  m
}
