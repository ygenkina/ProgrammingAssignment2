## short program to initialize a matrix object
## and chache the inverse of this matrix

## create a matrix with a cached option for the inverse of itself

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL ##initial value for the inverse upon matrix creation
  set <- function(y){ ##set method for the matrix x, this will allow for creating a matrix with
    ## a chached inverse value
    x <<- y
    inv <<- NULL 
  }
  get <-function() x ## get method for matrix x will return x
  setinv<-function(solve) inv <<- solve ## setinv method for the matrix x, will store the inverse value in chache
  getinv<-function() inv ##get inv method will return the inverse matrix
  list(set=set,get=get,setinv=setinv,getinv=getinv) ##list of available x methods
}


## retrieve the chached inverse of the matrix x
## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
        i<-x$getinv() #store the current value of inverse in i
        if (!is.null(i)){ #check to see if there is already a stored inverse matrix
          message("getting chached matrix")
          return(i) #if true, print a statement and then the matrix inverse
        }
        my_matr<-x$get()#call the get fnxn from the makeChacheMatrix constructor
        i<-solve(my_matr,...) # calculate the inverse and store that in i
        x$setinv(i) #make sure that this value is stored in cache
        i #return the matrix inverse value
}
