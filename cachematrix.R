## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## The first function "makeCacheMatrix" creates a special "matrix", 
## which containing a function to:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of inverse of the matrix
## 4. get the value of inverse of the matrix

makeCacheMatrix<-function(x=matrix()){
        m<- NULL
        set<-function(y){
                x<<-y
                m<<-NULL
        }
        get<-function() x
        setinv<-function(inverse) m<<-inverse
        getinv<-function() m
        list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## The function "cacheSolve" calculates the inverse of the special "matrix".
## At first checks if the inverse matrix has already been computed it gets 
## the result and skips the computation. If not - calculates the inverse matrix.

cacheSolve<-function(x, ...){
        ## Return a matrix that is the inverse of 'x'
        m<- x$getinv()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        data<-x$get()
        m<-solve(data, ...)
        x$setinv(m)
        m
}

