## Matrix inversion is usually a costly computation and there may be some benefit to caching 
## the inverse of a matrix rather than compute it repeatedly 

## The makeCacheMatrix function creates a special "matrix" object that can cache its inverse

## 1. set the value of the matrix 
## 2. get the value of the matrix 
## 3. set the value of inverse of the matrix 
## 4. get the value of inverse of the matrix 


makeCacheMatrix <- function(x = matrix()) {
  # initialize the matrix to NULL during the first call to makeCacheMatrix
  # this is needed because getinv() is called immediately after
  # the makeCacheMatrix funciton is constructed, without a call to setinv
  # we know we must first calculate the inverse in cacheinv.  
  m <- NULL 
  
  # funciton to set a new value for the underlying matrix
  # this invalidates the cached inverse, m
  # we use the <<- operator to set the value of x and m because we want 
  # to modify x and m defined in the enclosing environment (created 
  # when makeCacheMatrix was first called), not in the environment local to set(),
  # in which x and m are undefined.
  # we must reset m to NULL since we are modifying the underlying
  # matrix and the cached value is no longer the valid 
  set <- function(y) 
  {
    x <<- y
    m <<- NULL
  }
  
  # getter function for underlying matrix
  get <- function()
  {
    x
  }
  
  # set the inverse of the matrix x.  Called by cachesolve,
  setinv <- function(inverse) 
  {
    m <<- inverse
  }
  
  # returns the inverse. It Will be null if setinv has not been called or
  # if set is called after the last call to setinv
  getinv <- function() 
  {
    m
  }
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  # get the inverse of the matrix defined inside x.
  # we can use the $ operator to access the function since it was
  # defined in the list of function pointers returned by the call to
  # makeVector
  m <- x$getinv()
  
  # if we've already computed the inverse and stored it via setinv(),
  # and have not invalidated the cache by calling set(), return the cached
  # version of x
  if(!is.null(m)) 
  {
    message("getting cached data")
    # we have to explicily use return here otherwise we'd keep
    # executing the code after the if conditional ends.  Since
    # the cached version is good, just return it and we are done.
    return(m)
  }
  
  # either we havent computed the cached version yet, or we've called
  # set() previously and invalidated the cache.
  
  # call get() to get the underlying vector
  data <- x$get()
  
  # calculate the inverse of the underlying matrix, passing with it
  # any varargs passed to cacheSolve
  m <- solve(data, ...)
  
  # now set the inverse in x so we cache it and dont need to needlessly
  # recompute it
  x$setinv(m)
  
  # return the caching matrix
  m
}
