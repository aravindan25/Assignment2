## Two functions one is to create a special vector and other 
## is to compute the inverse of matrix. Compute if applicable, 
##else return from the cache
## makeCacheMatrix is a function which creates a square matrix

makeCacheMatrix <- function(x = matrix()) {
      mat <- NULL       # define the variable mat to NULL
      
## Option to set a matrix through set function      
      set <- function(y) {
            x <<- y
            mat <<- NULL      ## mat should be set to NULL to ensure even outside scope of this function
      }
## Option to get a matrix , which would be set by the call of the parent 
## function - makeCacheMatrix
      get <- function()x

## Option to set value of matrix , but seldom used in this scenario
      
      setmatrix <- function(solve) mat <<- solve

## Option to get the matrix returned,       
      getmatrix <- function() mat
      
      list(set = set, get = get,
           setmatrix = setmatrix,
           getmatrix = getmatrix)
}

## Returns the inverse of a square matrix. 
## advantage of this function - computes only when required, else 
## returns from cache

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      mat <- x$getmatrix() # get value of mat from cache
      
      if(!is.null(mat)) {
            message("getting cached data")
            return(mat)
      }
      ## If mat is NULL, compute the inverse of the matrix
      
      data <- x$get()
      mat <- solve(data, ...) ## invoking the solve function to retutn 
                              ##the inverse of a square matrix 
      x$setmatrix(mat)
      mat
}
