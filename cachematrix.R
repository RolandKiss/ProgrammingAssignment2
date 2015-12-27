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
    
    setinv<-function(Inverse) Inv<<-Inverse       #setting Inv equal to Inverse

    getinv<-function() Inv                        #returning the inverse of the matrix if stored as Inv

list(set=set,get=get,setinv=setinv,getinv=getinv) #creating the list object
}


#cacheSolve function includes the followings:
# 1. retrieve the value of the getinv() function and store it as 'Inverse'
# 2. if the value of 'Inverse' is not NULL then retrieves the value of 'InVerse'
# 3. if the value of 'Inverse' is NULL then then the function calculates it and retrieves it from 'Inv'

cacheSolve<-function(x,...) {
    
    Inverse<-x$getinv()                      #retrieve the Inverse of the matrix stored
    
    if (!is.null(Inverse)) {                 #if the value of Inv is not null
    message("retreiving inverse from cache") #display message 
    return(Inverse)                          #return the value of Inv
    }
    
    matrix.original<-x$get()                 #retrieve the original matrix
    Inv<-solve(matrix.original)              #calculate the inverse of the original matrix and store as 'Inv'
    x$setinv(Inv)                            #store the value of Inv in the cache
    return(Inv)                              #return the value of Inv
}


#Sources visited:
#    Coursera Forums: https://class.coursera.org/rprog-035/forum/thread?thread_id=448
#    Coursera Forums: https://class.coursera.org/rprog-035/forum/thread?thread_id=433
#    Coursera Clarifying Instructions: https://class.coursera.org/rprog-032/forum/thread?thread_id=44#comment-1345
#    Stackoverflow Forum: http://stackoverflow.com/questions/11995832/inverse-of-matrix-in-r
