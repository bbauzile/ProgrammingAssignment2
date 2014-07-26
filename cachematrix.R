## this function calculates the inverse of the special matrix
## it first check if the inverse is in the cached (calculated before)
## if so it skips the calculation (cacheSolve) and the answer from the cached 
## if not it calculates it and set it in the cached

makeCacheMatrix <- function(mat = matrix()) {
  inv_mat <- NULL
  set <- function(y) {
    mat <<- y
    inv_mat <<- NULL
  }
## here it gets for the matrix
  get <- function() mat

## setting the result to the cached
  setinverse <- function(inverse) inv_mat <<- inverse

## getting the inverse
  getinverse <- function() inv_mat

## return the matrix with newly defined function
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## cacheSolve compute the inverse of the matrix created with
## the makeCacheMatrix function.
cacheSolve <- function(mat, ...) {
  inv_mat <- mat$getinverse()

  ## if the inverse is already calculated, it will return it
  if(!is.null(inv_mat)) {
    message("getting cached data-inverse of the matrix")
    return(inv_mat)
  }

  ## if inv is null, so it compute it
  else {
    data <- mat$get()
    inv_mat <- solve(data)

    ## setting to the cached
    mat$setinverse(inv_mat)

    ## return it 
    inv_mat
  }
}
# raw_data <- matrix(c(1:4), nrow=2)
# data <- makeCacheMatrix(raw_data)
# data$get()
# cacheSolve(data)
# cacheSolve(data)