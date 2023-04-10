## Programming Assignment 2: Lexical Scoping
## The following functions 
## 1. makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## 2. cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.

## 1. makeCacheMatrix 

makeCacheMatrix <- function(x = matrix()){
            inverse <- NULL
            set <- function(y){
              x <<-y
              inverse <<- NULL
            }
            get <- function(){x}
            setInverse <- function(inversecalculation){inverse <<- inversecalculation}
            getInverse <- function(){inverse}
            list(set = set, get = get,
                 setInverse = setInverse,
                 getInverse = getInverse)
                }

## 2. cacheSolve:

cacheSolve <- function(x, ...){
          inverse <- x$getInverse()
          if(! is.null(inverse)){
                message("getting cached data")
                return(inverse)
          }
          data <- x$get()
          inverse <- solve(data, ...)
          x$setInverse(inverse)
          inverse
}


