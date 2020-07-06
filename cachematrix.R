## Put comments here that give an overall description of what your
## functions do

##There are two functions mackeCacheMatrix, cacheSolve
##makecacheMatrix consist of set, get, setInv, getInv

makeCacheMatrix <- function(x= matrix()){
      inv <- NULL                       ## Initializing inverse as NULL
      set <- function(y){
       x <<- y
             inv <<- NULL
          }
          get <- function(){x}          #function to get matrix x
          setInverse <- function(inverse){inv <<- inverse}
          getInverse <- function(){inv}  # function to obtain inverse of the matrix
          list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## Write a short comment describing this function
## This is used to get the cache data

cacheSolve <- function(x, ...){       ## gets cache data
       inv <- x$getInverse()
       if(!is.null(inv)){             # cheking wether inverse is NULL
             message("getting cached data")
             return(inv)              # return inverse value
       }
       mat <- x$get()
       inv <- solve(mat,...)           # calculate inverse value
       x$setInverse(inv)
       inv
} 

        
