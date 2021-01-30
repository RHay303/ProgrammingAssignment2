## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

##makeCacheMatrix creates a special matrix object that can cache its inverse
##x needs to be a square matrix 
## returns a list containing functions to do the following
## 1. set the matrix 
## 2. get the matrix 
## 3. set the inverse 
## 4. get the inverse
# this is used an input to the cacheSolve() function


makeCacheMatrix <- function(x = matrix()) {
  #create matrix to hold the inverse of x and set it equal to NULL
  inv_x <- NULL
  
  ##defines a function to set the vector x to a new vector y 
  set <- function(y) {
    ##set x to assign the value -y to the parent environment
    x <<- y
    ##assign the value of NULL to inv_x and set it to NULL so that 
    ##subsequent calls of cacheSolve() recalculate the inverse
    inv_x <<- NULL
  }
  
  ##returned the function of x
  get <- function() x
  
  ##set the value of the inverse
  setinverse<- function(inverse) inv_x <<-inverse
  
  ##get the value of the inverse
  getinverse <- function() inv_x
  
  ##returns the 'special vector' containing the four arguments defined above
  ##into a list that can be used by the parent environment
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


##cacheSolve calculates the inverse of a matrix
##If the inverse has already been found and is cached, then cacheSolve retrieves it and if not computes the
##inverse matrix, caches it, and returns it.


cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv_x <- x$getinverse()
    
    ##if the inverse is not NULL (meaning the inverse was already calculated)
    ##then calculate the inverse
    if (!is.null(inv_x)) {
        message("getting cached inverse matrix")
        return(inv_x)
    } else {
      inv_x <- solve(x$get())
      
      ##set the value of the inverse in the cache
      x$setinverse(inv_x)
      
      return(inv_x)
    }
}
  
  



  
  