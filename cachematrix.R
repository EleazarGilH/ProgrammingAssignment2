## This functions are able to cache the inverse of a matrix. 
# to avoid computed the inverse of a matrix repetidealy if the contents of the matrix
# are not changing.

# This function (makeCacheMatrix) creates a special "matrix",
# represented as a list containing functions to set and get the matrix values and
# set and get the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
   ##assuming the matrix is inversable
      m_inv <- NULL
      set <- function(y){
              x<<- y
              m_inv <<- NULL
      }
      get <- function() x
      setinv <- function (solve) m_inv <<- solve
      getinv <- function () m_inv
      list(set = set, get = get, setinv=setinv, getinv= getinv)
}


# The following function calculates the inverse of the special "matrix" 
# created with makeCacheMatrix. First it checks if the inverse has already
#been calculated. If so, it gets the inverse from the cache and skips the computation.
#Otherwise, it calculates the inverse of the data and sets 
#the value of the inverse in the cache via the setinv function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m_inv <-x$getinv()
        if(!is.null(m_inv)){
                message("getting cache data")
                return(m_inv)
        }
        a_matrix <- x$get()
        m_inv <- solve(a_matrix)
        x$setinv(m_inv) 
        m_inv
}
