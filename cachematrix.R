## Programming Assignment #2
## Wade Hansen - 2014 Dec 20

## Write the following functions:
## 1.makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## 2.cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
      ## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should 
      ## retrieve the inverse from the cache.



## makeCacheMatrix takes matrix input, it also contains 4 subfunctions
      ## set - inputs the matrix and stores
      ## get - returns matrix
      ## setinv - stores inverse matrix
      ## getinv - returns inverse matrix


makeCacheMatrix <- function (x = matrix()) {
      i <- NULL
      set <- function (y) {
            x <<- y
            i <<- NULL
      }
      get <- function() x
      setinv <- function(solve) i <<- solve
      getinv <- function() i
      list (set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve takes input of the result of the  makeCacheMatrix function, it looks to see if the inverse cache exists already
      ## if so it sends a message indicating the use of cached data and returning the inverse matrix
      ## if not it does the calculates the inverse matrix, stores it and returns inverse matrix


cacheSolve <- function(x, ...) {
      i <- x$getinv()
      if(!is.null(i)) {
            message("getting cached data")
            return(i)
      }
      data <- x$get()
      i <- solve(data, ...)
      x$setinv(i)
      i
}
