#Assignment2
#makeCacheMatrix is used to cache the inverse of a matrix.
#This function has 4 functions
#set function is for setting the value of the matrix
#get function is to get the value of the matrix
#getinv gets the inverse of a matrix
#setinv sets the inverse of a matrix

makeCacheMatrix <- function(x = matrix())
{
 i <- NULL
 set <- function(y){
   x <- y
   i <<- NULL
 }
 
 get <- function() x
 setinv <- function(inverse) i <<- inverse
 getinv <- function() i
 list(set = set, get = get,
      setinv = setinv,
      getinv = getinv)
}


## cacheSolve is used to compute the inverse of a matrix
#It first calls getinv() to chek if an inverse already exists. 
#If so it will not do any computation and return the inverse(i)
#else it computes the inverse and sets the value in cache via setinv

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  return(i)
}

#sample test
#x = matrix(c(4,1,1,4),nrow=2,ncol=2)
#a = makeCacheMatrix(x)
#cacheSolve(a)
#            [,1]        [,2]
#[1,]  0.26666667 -0.06666667
#[2,] -0.06666667  0.26666667
