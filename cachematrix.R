# the functions makeCacheMatrix and cacheSolve are contrived to illustrate
# how function values are passed/inherited in a lexical scoping scenario


# makeCacheMatrix takes an invertable martix as input and returns a list
# of functions. these functions are then used to set/get the matrix value
# and to set/get the inverse matrix value

makeCacheMatrix <- function(x = matrix()) 
  {
  inv <- NULL
  set <- function(y)
      {
      x <<- y
      inv <<- NULL # if a new matrix is input, inv is reset to 'uncalculated'
      }
  get <- function() {x} # retrieve the input matrix
  setinv <- function(temp) {inv <<- temp} # make temp available outside makeCacheMatrix with the name inv
  getinv <- function() {inv} # retrieve inverse within makeCacheMatrix
  
  list(set=set, get=get, setinv=setinv, getinv=getinv) # the 'returned' value of makeCacheMatrix
  }


# cacheSolve first checks if the inverse exists in memory
# if not, it computes the inverse and makes that value available
# via the functions defined within makeCacheMatrix

cacheSolve <- function(x, ...)
  {
  inv <- x$getinv() # the *list* from makeCacheMatrix is the input now
  if(!is.null(inv)) # check: is the inverse already set?
    {
    message('getting cached data')
    return (inv) # if so, return it!
    }
  
  # if the return didn't happen, then this code below will
  mat <- x$get() # get the matrix originally input into *makeCacheMatrix*
  inv <- solve(mat, ...) # compute the inverse
  x$setinv(inv) # update the *list* that was input
  inv # 'return' the inverse just computed
  }
