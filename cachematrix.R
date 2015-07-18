## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    inv<-NULL #always put the inverse as NULL
    set<- function(y){
        x<<-y
        inv <<- NULL 
        # this will reset the inv to NULL, should you put in a new matrix y will replace x
    }
    get<-function () x 
    #this will just return the matrix x. so other function can use makeCacheMatrix$get() to get/see the matrix
    setinv<-function(inv_matrix) {inv<<-inv_matrix}
    # once the inv is calculate, use this func to set it to the object
    getinv<- function() inv
    #give inv which is the inverse of matrix x
    list(set = set, get=get, setinv= setinv, getinv= getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv<-x$getinv()
    # reach into object x, and use $getinv func to get the inv
    if (!is.null(inv)){
        message("getting cached data")
        return (inv)
    }
    #if inv is not null, x already has a cached inv matrix, just return it and end the function. If not run the following
    data_matrix<-x$get()
    #get the matrix of x
    inv<-solve(data_matrix,...)
    #solve the matrix x
    x$setinv(inv)
    #put the inverse matrix of x back to x
    inv
    #return the inv of x 
}
