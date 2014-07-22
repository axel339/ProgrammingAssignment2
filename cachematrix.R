makeCacheMatrix <- function(x) { # I assume the user supply an invertible square matrix
  i <- NULL     # "i" is the inverted matrix, which I set to NULL at the first call 
  set <- function(y) { # resetting the matrix with new matrix "y"
    x <<- y     
    i <<- NULL  # when resetting the matrix, the inverse is reset to NULL
  }
  get <- function() x # display the current matrix 
  setinv <- function(inv) i <<- inv #Do not call this function directly, its only purpose is for cachemean to update the list with the newly set inversed matrix
  getinv <- function() i # display the current inversed matrix which has been created in the global environment by the previous function and the operator "<<-"
  list(set = set, get = get, # This creates the list of function required to cache and access the matrix/inversed matrix
       setinv = setinv,
       getinv = getinv)
}

cacheinv <- function(x, ...) { # caching function
  i <- x$getinv() 
  if(!is.null(i)) { # If inv has already been calculated
    message("getting cached data")
    return(i)       # return the inversed matrix without calculation
  }
  data <- x$get() # otherwise, get the original matrix
  i <- solve(data, ...) # and solve the inverse
  x$setinv(i) # Finally stores the inversed matrix in the corresponding list 
  i
}

# Use example : m <- matrix(rnorm(9),3)
#               mat <- makeCacheMatrix(m)
#               cacheinv(mat)
#               mat$getinv()
#               mat$cacheinv(mat)
#               mat$set(matrix(rnorm(9),3))
#               etc...