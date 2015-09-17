## makeCacheMatrix: Function that creates a special vector to store in cache 
## a value of any invertible matrix and its inverse. 

makeCacheMatrix <- function(x = matrix()) {
  
  # m is the variable that represents the inverse matrix, which is initialized with NULL
  m <- NULL
  
  # function to assign a new matrix y to variable x
  # also, this function set m to NULL, overwriting the old value of inverse matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  # to get the matrix value
  get <- function() x
  # initializes the variable setinverse using 'solve' function, as suggested at assignment
  setinverse <- function(solve) m <<- solve
  
  # to get the inverse matrix
  getinverse <- function() m
  
  # list to store the available functions of makeCacheMatrix
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve: Function that calculates the inverse matrix using cached stored variables
## avaiable through makeCacheMatrix

cacheSolve <- function(x, ...) {
        # get matrix that is the inverse
        m <- x$getinverse()
        
        # If m is not null, returns m
        if (!is.null(m)) {
          message("getting cached data")
          return(m)
        }
        # get the matrix 
        data <- x$get()
        # using the solve function to calculate the inverse matrix
        m <- solve(data)
        
        #setting the m value at makeCacheMatrix environment
        x$setinverse(m)
        
        # return m
        m
}
