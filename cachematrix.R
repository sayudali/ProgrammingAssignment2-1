##makeCacheMatrix will be the first function to be called and 
##it will create a "special" matrix object that can cache
##its inverse. The function will create and use getMatrix, setMatrix, setInverse, and
##getInverse to accomplish the task. cacheSolve will be the
##next function called and it will compute the inverse of the matrix 
##returned by makeCacheMatrix. If the inverse has already been calculated
##and the cacheSolve function is called, then it will retrieve the 
##inverse from the cache. It will also display a message. 



##makeCacheMatrix functions to create a matrix object that can cache its inverse.
##4 functions will be created (getMatrix, setMatrix, setInverse, getInverse) and used to 
##complete the task
makeCacheMatrix <- function(x = matrix()) {
  ##The variable invMatrix stores the inversion, it is given an initial NULL value
  invMatrix <- NULL
  ##The setMatrix function changes the vector argument, tempMatrix
  setMatrix <- function(tempMatrix) {
    x <<- tempMatrix
    invMatrix <<- NULL
  }
  ##The getMatrix function returns the matrix x
  getMatrix <- function() x
  ##The variable setInverse sets the inverse matrix, invMatrix
  ##with the help of the solve function
  setInverse <- function(solve) invMatrix <<- solve
  ##The variable getInverse returns the inverse matrix, invMatrix
  getInverse <- function() invMatrix
  list(setMatrix = setMatrix, getMatrix = getMatrix,
       setInverse = setInverse,
       getInverse = getInverse)
}

##cacheSolve functions to compute the inverse of
##the matrix returned by makeCacheMatrix. If the 
##inverse has already been calculated, then the 
##cacheSolve will retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  
  ##Returns the inversed matrix from matrix x and 
  ##assigns is to invMatrix
  invMatrix <- x$getInverse()
  ##If the inversion has already been calculated
  if(!is.null(invMatrix)) {
    ##then return the following message
    message("getting cached data")
    return(invMatrix)
  }
  ##Otherwise, obtain the matrix x and
  ##perform a inversion using the solve function
  solveMatrix <- x$getMatrix()
  invMatrix <- solve(solveMatrix, ...)
  ##Set the inversion to matrix invMatrix and return the result
  x$setInverse(invMatrix)
  invMatrix
}

##############Test Run#############
##Assign matrix test_run, a 4x4 matrix, with value 8 running diagonally
test_run <- diag(8,4)
test_run

##Call the makeCacheMatrix function and assign the 
##returned value to finalMatrix_cached
finalMatrix_cached <- makeCacheMatrix(test_run)

##Call the cacheSolve function to compute the matrix
##stored in finalMatrix_cached
cacheSolve(finalMatrix_cached)

##Call the cacheSolve function again to test in the situation
##where the inverse has already been calculated.
##A message prompt is expected, displaying that it is 
##being fetched by cached data.
cacheSolve(finalMatrix_cached)
