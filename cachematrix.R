## Test with these two functions
## Step1. Create a cache Matrix and assign to A
## > A <- makeCacheMatrix()
## Step2. Using setMatrix function to set martix value of A
## > A$setMatrix(matrix(c(4,4,-2,2,6,2,2,8,4),3,3))
## Step3. cacheSolve function will read the Matrix A value and verify if the cache existed
##        Here the cache is not existed, use solve to calculate the inverse matrix and store it
## > cacheSolve(A)
## Step4. run again cacheSolve funtion, this time the find the getInMatrix value, print message("getting cached data") 
## > cacheSolve(A)

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  aInvMatrix <- NULL
  ## set matrix and reset the inverse matrix value to null
               x <<- y
               aInvMatrix <<- NULL
  }
  ## return matrix set
  getMatrix <- function() x
  ## set the inverse matrix calculated
  setInvMatrix <- function(iInvMatrix) aInvMatrix <<- iInvMatrix
  ## return the inverse matrix value
  getInvMatrix <- function() aInvMatrix
  ## return all of the functions defined
  list(setMatrix = setMatrix,
       getMatrix = getMatrix,
       setInvMatrix = setInvMatrix,
       getInvMatrix = getInvMatrix)
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  ## assign value returned by inverse matrix function
  m <- x$getInvMatrix()
  ## check if the value is null
  if(!is.null(m)) {
    ## non null indicates the data has been processed
    message("getting cached data")
    return(m)
  }
  ## null case, get the input matrix value
  data <- x$getMatrix()
  ## solve function to calculated the inverse matrix
  m <- solve(data)
  ## set the inverse matrix value into the cache
  x$setInvMatrix(m)
  ## return the inverse matrix value 
  m
}
