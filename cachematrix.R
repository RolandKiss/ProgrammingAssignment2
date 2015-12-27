# makeCacheMatrix creates a list including the following functions:
# 1. set the value of matrix
# 2. get the value of matrix
# 3. set the value of the inverse matrix
# 4. get the value of the inverse matrix

makeCacheMatrix<-function(x=matrix()) {
    Inverse<-NULL      #the inverse results are stored at Inverse, which is empty in the beginning
    set<-function(y) {
      x<<-y            #setting the initial matrix to the cache 
      Inv<<-NULL       #setting Inv to empty in the cache
    }

    get<-function() x  #returning the input matrix
    
    setinv<-function(Inverse) Inv<<-Inverse      #setting inverse

    getinv<-function() Inv                       #returning the inverse of the matrix if stored

list(set=set,get=get,setinv=setinv,getinv=getinv)  #creating the list object
}


#cacheSolve function includes the followings:
# 1. retrieve the value of the getinv() function and store it as 'Inv'
# 2. if the value of 'Inv' is not NULL then retrieves the value of 'InV'
# 3. if the value of 'Inv' is NULL then then the function calculates it and retrieves it

cacheSolve<-function(x,...) {
    Inverse<-x$getinv()
    
    if (!is.null(Inverse)) {                 #if the value of Inv is not null
    message("retreiving inverse from cache") #display message 
    return(Inverse)                          #return the value of Inv
    }
    matrix.original<-x$get()                 #retrieve the original matrix
    Inv<-solve(matrix.original)              #calculate the inverse of the original matrix
    x$setinv(Inv)                            #store the value of Inv in the cache
    return(Inv)                              #return the value of Inv
}