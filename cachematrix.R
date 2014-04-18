############################################################################################
# cachematrix.R
# submitted for R programming course on coursera 'rprog-002' by James Elvy on 18 April 2014
# course website: https://class.coursera.org/rprog-002
# file 'cachematrix.R' can be downloaded from: https://github.com/jameselvy/ProgrammingAssignment2
# file template created by Roger Peng. 
# Forked and cloned from https://github.com/rdpeng/ProgrammingAssignment2
############################################################################################
#
# Matrix inversion is usually a costly computation and their may be some benefit to caching the 
# inverse of a matrix rather than compute it repeatedly (there are also alternatives to matrix 
# inversion that we will not discuss here). 
#
# The assignment was to write a pair of functions that cache the inverse of a matrix.
# the skeletons of the functions were prepared by Rodger Peng and the functions have been adapted from
# exemplar functions makeVector() and cachemean()
#
# cacheSolve employs the R function 'solve' which is included in the base package.
# The 'solve' function computes the inverse of a square matrix where number of rows == number of columns. 
# For example, if X is a square invertible matrix, then solve(X) returns its inverse.
# cacheSolve assumes the input matrix is always invertible.


##  function: makeCacheMatrix
# This function creates a special "matrix" object that can cache its inverse.
# It returns a list of four functions:
# 1. set - sets the value of the matrix
# 2. get - get the value of the matrix
# 3. setInverse - set the value of the inverse using 'solve' function to cache
# 4. getInverse - retrieves the value of the inverse from the cache
makeCacheMatrix <- function(x = matrix()) {     # function requires matrix input, assumes square matrix
  m <- NULL
  set <- function(y) {
    x <<- y             
    m <<- NULL          
  }                     
  get <- function() x
  setInverse <- function(solve) m <<- solve
  getInverse <- function() m
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


##  function: cacheSolve
# This function computes the inverse of the special "matrix" returned by makeCacheMatrix.
# If the inverse has already been calculated (and the matrix has not changed), 
# then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()               # if no cached inverse matrix exists, 
                                    # copy existing inverse matrix to cache
  
  if(!is.null(m)) {                 # return inverse of cached matrix if it exists
    message("getting cached data")
    return(m)
  }
  
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}


## test routines
matrix_example <- rbind(c(1,4),c(2,3))
matrix_example

test <- makeCacheMatrix(matrix_example)
cacheSolve(test)
cacheSolve(test)

solve(matrix_example)
test$set(rbind(c(1,4),c(5,6)))
cacheSolve(test)
solve() <- rbind(c(1,4),c(2,3))
