## makeCacheMatrix is a function that returns functions as a list.
## It store a martix and a cached value of the inverse.
## It contains the following functions:
## * set set the value of a matrix
## * get get the value of a matrix
## * setmatrix get the cahced value (inverse of the matrix)
## * getmatrix get the cahced value (inverse of the matrix)

## The first function, makeCacheMatrix, creates a special "matrix".
makeCacheMatrix <- function(x = matrix()) {
        
        m<-NULL
        

        set<-function(y){
                x<<-y
                m<<-NULL
        }
        
        get<-function() x
        
        setmatrix<-function(solve) m<<- solve
        
        getmatrix<-function() m
        
        list(set=set, get=get,
             setmatrix=setmatrix,
             getmatrix=getmatrix)
}

## The cacheSolve function calculates the inverse of the special matrix
## If the inverse has already been calculated, and the matrix is unchanged, then cacheSolve 
## retrieves the inverse from the cache.
cacheSolve <- function(x=matrix(), ...) {
        
        m<-x$getmatrix()
        
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }

        matrix<-x$get()
        m<-solve(matrix, ...)
        x$setmatrix(m)

        m
}

## Example
a <- makeCacheMatrix( matrix(c(1,2,3,4), nrow = 2, ncol = 2) )
