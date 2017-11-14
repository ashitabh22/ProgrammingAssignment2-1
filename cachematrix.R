library(MASS)
#Making Cache File 
makeCachematrix<-function(x=matrix())
{
  i<-NULL;
  set<-function(y) # setting the matrix for getters in the future.
  {
    x<<-y
    i<<-NULL
  }
  get<-function() x # Simply returns existing matrix
  setinverse<-function(inverse)
  {
    i<<-inverse
  }
  getinverse<-function() i
  
  list(set = set , get = get,setinverse=setinverse,getinverse=getinverse) # Names the functions so that they can be called vie '$'
  
}

cacheSolve<-function(x){
  m<-x$getinverse() #Getting Cached Data 
  if(!is.null(m)){. #Checking whether cache data exists ,if so we enter the if statement and return cache data.
    message("getting cached data ")
    return(m)
  }
  data<-x$get()# if there was no cahce data to be found , we compute the inverse
  m<-ginv(data)
  x$setinverse(m)
  m
}

#To run code you can use the following 
#mymatrix<-matrix(1:25,5,5)
#myinverse<-makeCachematrix(mymatrix)
#myinverse$set(mymatrix) 
#and so on...
