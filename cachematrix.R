
## This function takes a matrix as input and returns 
## a list of functions that will find and set its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inverse<-NULL
  
  set<-function(y) {
    x<<-y
    inverse<<- NULL
  }
    
  get<-function() {x}
  
  setinverse<-function(solve) {inverse<<-solve}
    
  ## This function returnss the
## inverse matrix if it is already in the cache.

getinverse <- function() {inverse}

## This function creates a matrix.
  
  list(set=set,
       get=get,
       setinverse=setinverse,
       getinverse=getinverse)
  
}


## This function will find the inverse of a matrix. If the inverse has not
## already been calculated it will get the inverse from a cache.Otherwise
## it will test whether the solve function can operate on the matrix and
## if it can, it will return the output of the solvee function.
## If it cant't the function prints a message stating it is not possible
## to invert the matrix.

cacheSolve <- function(x=matrix(), ...) {
        ## Return a matrix inverse that is the inverse of 'x'
  
    inverse<-x$getinverse()
    if(!is.null(inverse)) {
      print("Obtaining inverse matrix from cache.")
      return(inverse)
    }
    else
      {
        inputmatrix<-x$get()
        if( length(try(solve(inputmatrix),silent=TRUE)) > 1) {
          inverse<-solve(inputmatrix)
          x$setinverse(inverse)
          inverse
      }
    else
      {
        print("The matrix cannot be inverted")
      }
   }
}

