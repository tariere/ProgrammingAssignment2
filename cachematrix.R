## Write a short comment describing this function
## Function makeCacheMatrix will (1) assign the variables mat and set to their initial values so that errors are not returned.
makeCacheMatrix <- function(x = matrix()) {
  mat <- NULL
  
  ##Function set will set the functions for the setters and getters, to access and change the data within a function. In this case, it's taking my matrix and changing it if needed. We use <<- in this case because we are referencing variables in the parent environment
  set <- function (y){
    x<<-y
    mat<<-NULL
  }
  ## these assignments are required for the rest of the setting and getting operation. 
  get <- function() x
  setmat <- function(inverse) mat<<- inverse
  getmat <- function () mat
  ## creates a new list object using the variables that were created above. 
  list (set = set, get = get,
        setmat = setmat,
        getmat = getmat)
}


## used to retrive the values from above. if null, it will calculate the value.  if not null, then it will pull the cached data. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  mat<-x$getmat()
  if(!is.null(mat)){
    message("getting cached data")
    return(mat)
  }
  data<-x$get()
  mat<-solve(data,...)
  x$setmat(mat)
  mat
}
