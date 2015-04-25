## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

##This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  
  ##Cache matrix object defaulted to NULL
  cmtx <- NULL
  print(cmtx)

  ##set the special matrix
  set <- function(y)
  {
    x<<- y
    cmtx <<- NULL
    
  }
  
  ##retrieving the special matrix
  get <- function() x
  
  
  
  ##calculate inverse of matrix and assign to cache variable
  setInverse <-function(x)
  {
    message("set Inverse")
    cmtx <<- solve(x)
      
  }
  
  ##return the inverse matrix in cache
  getInverse <- function() cmtx
  
  list(set=set,get=get,
       getInverse=getInverse,
       setInverse=setInverse )
  
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  
  ##get the inverse matrix from cache
  Inv <- x$getInverse()
  
  ##return the inverse matrix, caculated and cached
  if(!is.null(Inv))
  {
    message("getting cached data")
    return(Inv)
  }
  
  ##No Cached inverse matrix, retrieve the special matrix that is set
  data <- x$get()
  
  ##compute Inverse of special matrix 
  m <- solve(data,...)
  
  ##cache the inverse of matrix
  x$setInverse(m)
  
  return(m)
  
}
