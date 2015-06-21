## These Functions will cacahe a matrix and then compute the inverse of the matrix.


## Create the matrix object to be cached.

makeCacheMatrix <- function(m = matrix()) {
  inverse <- NULL
  set <- function(x){
    m <<- x;
    inverse <<- NULL;
  }
  get <- function() return(m);
  setinv <- function(inv) inverse <<- inv;
  getinv <- function() return(inverse);
  return (list(set = set, get=get,setinv = setinv,getinv = getinv))

}


## In the below function the inverse of the matrix object returned form above function is calculated.
##

cacheSolve <- function(m, ...) {
        inverse <- m$getinv()
        if(!is.null(inverse)) {
          return(inverse)
        }
        data <- m$get()
        inverse <- solve(data, ...)
        m$setinv(inverse)
        return(inverse)
}
