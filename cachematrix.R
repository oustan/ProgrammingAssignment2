## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix function takes as only imput an invertible matrix
## creates a list of four functions that are the inputs 
## for cacheSolve function below.

makeCacheMatrix <- function(x = matrix()) {
        m<-NULL
        set<-function(y){
                x<<-y
                m<<-NULL
        }
        get<-function()x
        set_matrix<-function(source) m<<-source
        get_matrix<-function() m
        list(set=set,get=get,set_matrix=set_matrix,get_matrix=get_matrix)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {  ## Return a matrix that is the inverse of 'x'
        m<-x$get_matrix()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        data<-x$get()
        m<-source(data, ...)
        x$set_matrix(m)
        m
        
}
