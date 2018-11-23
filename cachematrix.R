## The following functions intend to cache the inverse of a matrix so its value
##is only calculated the first time, and retireved from cache the following ones




## This function creates a special matrix object that includes 
## a list of functions that set the values of the matrix and NULLS the inverse 
## matrix, get the matrix, set the inverse matrix and get the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  m_inverse <- NULL
  set <- function(y) {
    x <<- y
    m_inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m_inverse<<-inverse
  getinverse <- function() m_inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}




## This function calculates the inverse of the special "matrix" created with the
## above function.Firstly checks if the inverse matrix has already been calculated, 
##and if so, retrieves it from cache and skips the computation. Otherwise,
## it calculates the inverse matrix of the provided data and sets the value of
## the inverse matrix in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
        ## Return a matrix that is the inverse of 'x'
}


