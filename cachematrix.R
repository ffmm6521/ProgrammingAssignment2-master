## makeCacheMatrix takes a matrix as input and outputs a list of functions
##cacheSolve takes a matrix as input and checks if the inverse exists. if not, cacheSolve will calculate its inverse
## Write a short comment describing this function

makeCacheMatrix<-function(x=matrix()){
  inver<-NULL
  set<-function(u) {
    x<<-u
    inver<<-NULL        
  }
  get<-function() x
  
  setInver<-function(inverse) inver<<-inverse
  getinver<-function() inver
  list(get=get,set=set,getinver=getinver,setInver=setInver)
}



## Write a short comment describing this function

cacheSolve<-function(x,...){
  innv<-x$getinver()
  if (!is.null(innv)){
    message('getting cached data')
    return(innv)
  }
  data<-x$get()
  print('data')
  innv<-solve(data)
  print('innv')
  x$setInver(innv)
  innv
}

