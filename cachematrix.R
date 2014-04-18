#############################################################################################
# cachematrix.R
# submitted for R programming course on coursera 'rprog-002' by James Elvy on 18 April 2014
# course website: https://class.coursera.org/rprog-002
# file 'cachematrix.R' can be downloaded from: https://github.com/jameselvy/ProgrammingAssignment2
# file template created by Roger Peng. 
# Forked and cloned from https://github.com/rdpeng/ProgrammingAssignment2
#############################################################################################
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



########################
# function: makeCacheMatrix
# This function creates a special "matrix" object that can cache its inverse.
# It returns a list of four functions:
# 1. set - sets the value of the matrix
# 2. get - gets the value of the matrix
# 3. setInverse - set the value of the inverse using 'solve' function to cache
# 4. getInverse - retrieves the value of the inverse from the cache
makeCacheMatrix <- function(x = matrix()) {  # function requires matrix input, assumes square matrix
  m <- NULL
  set <- function(y) {                       # sets the value of the matrix
    x <<- y             
    m <<- NULL          
  }                     
  get <- function() x                        # gets the value of the matrix
  setInverse <- function(solve) m <<- solve  # set the value of the inverse of matrix to cache
  getInverse <- function() m                 # retrieves the value of the inverse from the cache
  list(set = set,                            # return a list of the 4 functions defined above                  
       get = get,                        
       setInverse = setInverse,          
       getInverse = getInverse)          
}



########################
# function: cacheSolve
# This function computes the inverse of the special "matrix" returned by makeCacheMatrix.
# If the inverse has already been calculated (and the matrix has not changed), 
# then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()                        # IF cached inverse matrix exists, assign to 'm' 
  if(!is.null(m)) {                          # end function and return inverse 'm', as no need to recompute inverse
    message("getting cached data")
    return(m)
  }
                                             # ELSE no cache of inverse of matrix exists, so need to compute
  data <- x$get()                            # get matrix and assign to 'data'
  m <- solve(data, ...)                      # compute inverse of matrix 'data'
  x$setInverse(m)                            # store inverse matrix in cache
  m                                          # return inverse of matrix and exit function
}



########################
# test routines
matrix_example <- rbind(c(1,4),c(2,3))       # make a test matrix
matrix_example                               # print 'matrix_example' to console to check matrix

test <- makeCacheMatrix(matrix_example)      # creates matrix using 'matrix_example' and returns
                                             # and returns list of 4 functions for cacheSolve 
                                             # function to use

cacheSolve(test)                             # check if cached inverse matrix exists (FALSE)
                                             # compute inverse of 'matrix_example'
                                             # write inverse of matrix to cache using setInverse() function
                                             # return inverse of 'matrix_example'

cacheSolve(test)                             # check if cached inverse matrix exists (TRUE)
                                             # retrieve cache of inverse of 'matrix_example'
                                             # return inverse of 'matrix_example'

solve(matrix_example)                        # check that cacheSolve is computing inverse correctly
                                             # by comparing with output of 'solve(matrix_example)'

test$set(rbind(c(1,4),c(5,6)))               # check that 'set' function is overiding cache
cacheSolve(test)                             # check inverse for new matrix returned
solve(rbind(c(1,4),c(5,6)))                  # check inverse of new matrix is computed correctly
cacheSolve(test)                             # check uses cached of matrix inverse this time













