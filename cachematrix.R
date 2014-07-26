## makeCacheMatrix: This function creates a matrix vector and cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL ##create a new variable
  set <- function(y) {
    x <<- y
    m <<- NULL ##the <<- operator tells R not to create a new variable but to look for the variable in some parent context.
  }
  get <- function() x
  setmatrix <- function(solve) m <<- solve ## use the function solve() to compute the inverse of a matrix
  getmatrix <- function() m ##get the inverse of the matrix
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}

## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above, If the inverse has already been calculated, retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  m <- x$getmatrix() ##work with fuction(makeCacheMatrix) above, call/pass getmatrix() from above environment to x and assign to variable m
  if(!is.null(m)) {
    message("getting cached data") ## if m is not equal to null which means from the above function the inverse of the matrix has already been computed and stored
    return(m) ## tell R to read it from where it is stored using a free floating variable m and stop
  }
  data <- x$get() ##get matrix vector
  m <- solve(data, ...) ##compute the inverse of the matrix, now we are in the environment associated with "cacheSolve", the m is set to the inverse of data
  x$setmatrix(m) ##pass m to x via setmatrix
  m ##return a matrix that is the inverse of 'x'
}

