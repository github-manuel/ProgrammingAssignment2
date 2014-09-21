## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# This function generates a list containing a function to:
# 1. set the value of the matrix
# 2. get the inverse of the matrix
# 3. set the value of the inverse
# 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()){
	inv <- NULL
	set <- function(y){
		x <<- y
		inv <<- NULL
	}
	get <- function(){
		x
	}
	set.inverse<-function(inverse){
		inv <<- inverse
	}
	get.inverse <- function(){
		inv
	}
	list(set=set, get=get, set.inverse=set.inverse, get.inverse=get.inverse)
}


## Write a short comment describing this function:

# This function checks if the matrix has changed. If it hasn't, 
# it calculates the inverse and stores it in the cache for 
# future use. If the matrix has changed, it calculates the inverse
# of the new matrix and stores the inverse in the cache for 
# future use.

cacheSolve <- function(x,...){
	inv <- x$get.inverse()
	if(!is.null(inv)){
		message("getting inverse from cache")
		return(inv)
	}
	data <- x$get()
	inv <-solve(data,...)
	x$set.inverse(inv)
	inv
}
