## R Learning course Programming Assignment 2


## A function to creates a matrix element that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  
  inverse_mtx <- NULL ##variable to store cashed inverse matrix
  
  set <- function(y) { ##function to update x
    x <<- y
    inv <<- NULL  #if matrix is updated, cache resets to NULL
  }
  
  get <- function() x ##function be able to retrieve x 
  setInverse <- function(inv) inverse_mtx <<- inv
  getInverse <- function() inv
  
  list(matrix = x, set = set, get = get, setInverse = setInverse, getInverse = getInverse) ##returning a list that holds the matrix x and the functions used in the function

}


## A function to compute inverse of the matrix returned by makeCacheMatrix function
cacheSolve <- function(x, ...) {
    
  inverse_mx <- x$getInverse()
  
  if (!is.null(inverse_mx)) { ##checking to see if it already cached
    return(inverse_mx)
  }
  
  mx <- x$get()
  inverse_mx <- solve(mx, ...) ##compute inverse
  x$setInverse(inverse_mx)  #cache inverse
  inverse_mx  #return the computed inverse
}
