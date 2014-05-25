# Caching the inverse of a matrix with 2 functions.
#makeCacheMatrix: Build a list of 4 subfunctions to retrieve and cache the inverse of a matrix
#set() initializes objects - the matrix and its inverse
#get() fetches the value of the matrix from cache
#setinv() computes the inverse 
#getinv() stores the inverse in cache

makeCacheMatrix<-function(x=matrix())
{
  m<- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- solve
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

#cachesolve retrieves inverse of a matrix from cache
#if inverse is not in cache, the function computes the inverse and caches it
cachesolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached inverse")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}

