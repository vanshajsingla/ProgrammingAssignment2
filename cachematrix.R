## The first function, makeCacheMatric creates a special "matrix", 
## which is really a list containing functions to
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse (mat)
## get the value of the inverse (mat)

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL                               # Create NULL invmat
  set <- function(y){                       # A function that uses a dull
    x <<- y                                 # var to store the matrix and
    inv <<- NULL                            # the state of invmat
  }
  get <- function() x                       # function to return matrix
  setInverse <- function(solveMatrix) inv <<- solveMatrix
  getInverse <- function() inv              # function to get inverse of mat
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}                                           # list returned with above subsets


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()                 #get the inverse of the matrix
  if(!is.null(inv)){                    #if it's not null then return it
    message("getting cached data")
    return(inv)
  }
  data <- x$get()                       #assign to data the cached (non-inverted) matrix
  inv <- solve(data)                    #solve (invert the matrix)
  x$setInverse(inv)                     #set-back to list
  inv                                   #return result
}
