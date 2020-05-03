## Matrix multiplication, Generating prime numbers and Integer factorization are very computationally intensive, as they're intended to be representative of numerical algorithms used in scientific computing. 
## This can be solved by R function which is able to cache potentially time-consuming computations
## R cache is Fast and Light-Weight Caching (Memoization) of Objects and Results to Speed Up Computations.
## For example inverse of matrix also one of the time consuming and intesive computing in many scintific application
## Let us use cache function for inverse of matrix
## The first function, makeCacheMatrix creates a special “matrix”, which is really a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of inverse of the matrix
## 4. get the value of inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) { ## If the inverse of the matrix is not catched the computed value has set into that.
        x <<- y
        i <<- NULL
    }
    get <- function() x   ## If the inverse of the matrix is catched the value has get from it.
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}
## The second function returns the inverse of the matrix by makeCacheMatrix(). 
## Firstly it checks whether the inverse has computed already by calling of getinverse().  
## If yes i.e. i is not NULL, then it return inverse of matrix from makeCacheMatrix()and skip computing. 
## Otherwise compute it and store the value in object to get it whenever required.
    
cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)) {
        message("Cached inverse matrix found, getting the matrix.")
        return(i) ## if inverse of matrix is catched for given matrix, return the inverse of matrix..
    }
    data <- x$get()
    i <- solve(data) ## If inverse of matrix is not catched, computing the inverse of the matrix
    x$setinverse(i)
    i
}
## This cache function for inverse of matrix can be checked by following way 
## Create the matrix by matrix_test <- makeCacheMatrix(matrix(1:4, 2, 2))
## Find the inverse of that matrix by cacheSolve(matrix_test)
## Thank you. 