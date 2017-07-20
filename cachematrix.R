##`makecacheMatrix` creates a special "matirx", which is
## a list containing the following functions:

## 1.  set the value of the matrix
## 2.  get the value of the matrix
## 3.  set the value of the matrix inverse
## 4.  get the value of the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## The following function calculates the inverse of the matrix stored in the 
## makeCacheMatrix function. However, it first checks to see if the
## inverse has already been calculated. If so, it `get`s the inverse from the
## list created by makeCacheMatrix and skips the computation. Otherwise, it 
##calculates the inverse of the matrix and sets the value of the inverse in 
##the cache via the `setinverse` function.

cacheSolve <- function(x) {
  
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinverse(m)
  m
}
