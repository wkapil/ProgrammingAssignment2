## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse.
## x is input matrix and i is inverse of matrix x. 

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    
    ## Setting value to matrix x
    set <- function(matrix){
        x <<- matrix
        i <<- NULL
    }
    
    ## Getting value of matrix x
    get <- function(){
        return(x)
    }
    
    ## Setting inverse value to  i
    setInverse <- function(inverse){
        i <<- inverse
    }
    
    ##Getting inverse matrix
    getInverse <- function(){
        return(i)
    }
    
    ##Providing list of functions
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  
      i <- x$getInverse()
      if(!is.null(i)){
          return(i)
      }
      
      # tempX is temporary variable for getting matrix x
      tempX <- x$get()
      # solve is the function to get inverse
      i <- solve(tempX, ...)
      x$setInverse(i)
      return(i)
      
}
