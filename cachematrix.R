# Coursera Assignment
# R Programming - Assignment 2 (https://class.coursera.org/rprog-003/human_grading/view/courses/972138/assessments/3/submissions)
# Caching the Inverse of a Matrix
# Matrix inversion is usually a costly computation and their may be some benefit to 
# caching the inverse of a matrix rather than compute it repeatedly.
# NB - This code has been written to calculate the inverse of a square matrix

# Function: makeCacheMatrix
# Arguements:  x (matrix object)
# Returns: inverse of matrix object
# Description:
# Checks to see if we have a square / symmetric matrix
# Creates a list object with getter and setter functions: 
# 1. Set value of matrix
# 2. Get value of matrix
# 3. Set inverse of matrix
# 4. Get inverse of matrix
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  if (isSymmetric(x)) {
    set <- function(y) {
      x <<- y
      inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)    
  }
  else {
    print("Please enter a square matrix")
  }
}


# Function: cacheSolve
# Arguements:  x (matrix object)
# Returns: matrix object
# Description: 
# Returns a matrix that is the inverse of the matrix object.
# If value not present we calculate and cache value
# Then we return the value
cacheSolve <- function(x, ...) {
  
  if(is.null(x$getinverse())) {
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
  }
  
  x$getinverse()
  
}

#Testing Code
m <- rbind(c(1,2), c(2,1))
n <- makeCacheMatrix(m)
n$get()
n$geti()
cacheSolve(n)
n$get()
n$geti()