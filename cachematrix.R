## The functions below create a special object that stores a matrix and caches its inverse.
## This function creates a special object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  ## Initialize the inverse property
  inv <- NULL
  
  ## Method to set the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  ## Method the get the matrix
  get <- function() x
  ## Method to set the inverse of the matrix
  setInverse <- function(inverse) inv <<- inverse
  ## Method to get the inverse of the matrix
  getInverse <- function() inv
  ## Return a list of the methods
   list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" object created by 
## createCacheMatrix above. If the inverse was already calculated (and the 
## matrix has not changed), then it should return the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  ## Get the matrix from our object
  mat <- x$get()
  ## Calculate the inverse using matrix multiplication
  inv <- solve(mat, ...)
  ## Set the inverse to the object
  x$setInverse(inv)
  ## Return the matrix
  inv
}


##Test
my_matrix <- makeCacheMatrix(matrix(3:6, 2, 2))
my_matrix$get()

my_matrix$getInverse()
cacheSolve(my_matrix)

my_matrix$set(matrix(c(4, 4, 2, 8), 2, 2))
my_matrix$get()

my_matrix$getInverse()
cacheSolve(my_matrix)








