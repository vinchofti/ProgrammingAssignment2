#with the first function you can create for example atze<-makeCacheMatrix()
#and set and get a matrix as well as the inverse of the matrix with atze$set(matrix(...))
# and atze$setinverse(solve(atze$get()))

makeCacheMatrix <- function(x = matrix()) {
        m<-NULL
        
        set<-function(y){
                x<<-y
                m<<-NULL
        }
        get<-function()
                x
        
        setinverse<-function(solv) 
                m<<-solv
        
        getinverse<-function()
                m
        
      list(set=set,
           get=get,
           setinverse=setinverse,
           getinverse=getinverse)
}

#this function checks if there already exists an inverse for the input function x 
#if it does it returns that value, if not it calculates and returns it 

cacheSolve <- function(x, ...) {
      v<-x$getinverse
      if(!is.null(v())) {
              return(v())
      }
      
      x$setinverse(solve(x$get(),...))
      x$getinverse()
}


