## These functions will cache the inverse of a matrix.
## "PE" will denote " the Parent Environment" in the notes used here

## This function creates a special "matrix" object that can stores its inverse
makeCacheMatrix <- function(x = matrix()) { ## Initialises x as an empty matrix
   inv <- NULL          ## Initialise inv as null. It´ll store the matrix´s inverse
  set <- function(y){   ## Set() function takes an argument "y"
    x <<- y             ## Assigns the value of y to x in PE
    inv <<- NULL        ## Assigns the value null to Inv in PE
  }                     ## This code clears any value of inv that has been stored
  ## The two lines of code above with "<<-" do the same as the lines in makeCacheMatrix                   
  
  get <- function()x    ## As "x" is not defined in get(), R retrieves it from PE
    setinverse <- function(inverse) inv <<- inverse ## Assigns the input argument to inv in PE
    getinverse <- function () inv   ## makeCacheMatrix defines the getter for the inverse of inv
  ## At this point we have setters and getters defined for both of the data objects 
  ## withihn  makeCacheMatrix
    
  ## Assigns each of these functions as elements within list() and returns it to PM  
  ## When this function ends, it returns a fully object of type makeCacheMatrix
  ## Each element in the list is named, meaning is created with the syntax 
  ## "elementName = value"
    list( set = set, ## gives the name set to the set() function defined above
          get = get, ## gives the name get to the get() function defined above
          setinverse = setinverse, ## gives the name setinverse to the setinverse() function defined above
          getinverse = getinverse) ## gives the name getinverse to the getinverse() function defined above
 ## By naming the list of elements, allow us to use "$" to access the functions 
 ## by name rather than using "[[ ]]" to get contents in a matrix
  }


## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed),then the cacheSolve should retrieve 
## the inverse from the cache.

cacheSolve <- function(x, ...) { ## Passes "x" as an argument and allows additional arguments to be passed
  inv <- x$getinverse() ## Attemps to retrieve the inverse from the object passed as an argument by calling first getinverse()
    if (!is.null(inv)){ ## checks if the result is null,     
      message("Getting cache data")
      return(inv) ## If the result is not equal to null, we have a valid value
    }             ## and can return it to the PE
  data <- x$get()         ## if the result is null, cacheSolve gets the matrix
  inv <- solve(data, ...) ## calculates the matrix´s inverse
  x$setinverse(inv)       ## sets the inv in the input object
  inv                     ## returns the matrix's inverse to PE
}
