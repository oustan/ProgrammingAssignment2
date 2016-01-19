## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix function takes as only imput an invertible matrix
## creates a list of four functions that are the inputs 
## for cacheSolve function below.

makeCacheMatrix <- function(x = matrix()) {
        m<-NULL ##1 sets the value of m to NULL (provides a default if cacheSolve 
                ##2 has not yet been used)
        set<-function(y){ #set the value of the matrix
                x<<-y   ##1 caches the inputted matrix so that cacheSolve can check 
                        ##2 whether it has changed (note this is within the set function)
                m<<-NULL## sets the value of m (the matrix inverse if used cacheSolve) to NULL
        }
        get<-function()x ## gets the value of the non-singular matrix
        set_matrix<-function(source) m<<-source ##1 calculates the inverse of non-singular 
                                                ##2 matrix via the solve function
        get_matrix<-function()                  ## gets the inverse
                ## passes the value of the function makeCacheMatrix
        list(set=set,get=get,set_matrix=set_matrix,get_matrix=get_matrix)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {  
        m<-x$get_matrix()       ## used to get the cache of the matrix
        if(!is.null(m)){        ## if the inverse exists, it gets it.
                message("getting cached data")
                return(m)
        }
        data<-x$get()   ## if the inverse if not there, first it is calculated and then retrieved.
        m<-source(data, ...)
        x$set_matrix(m)
        m
        
}
