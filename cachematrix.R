# Daniele Sinibaldi, Programming assignment 2
#  



#         function isValidInputData <- function(f,x,fstringtotesting)
#                 INPUT PARAMETERS: f function to testing input parameter data, 
#                 x data, fstringtotesting string function to testing
#                 OUTPUT: if parameter x is not valid for function f 
#                 terminate execution with warning message and 0 value, 
#                 else return 1
#         This function is extensible
isValidInputData <- function(f,z,fstringtotesting='') {
        if(!is.function(f)){
                message("parameter f must be a function")
                return(FALSE)
        }
        if(identical(f,solve)){
                # Controls on input parameter
                if(!is.matrix(z)) {
                        message(paste("the parameter of function",fstringtotesting,"must be a matrix"))
                        return(FALSE)
                }
                if( nrow(z)!= ncol(z)) {
                        message(paste("the parameter of function",fstringtotesting,"must be a square matrix"))
                        return(FALSE)
                }
                if(det(z)==0) {
                        message(paste("the parameter of function",fstringtotesting,"is not invertible matrix"))
                        return(FALSE)
                }
                return(TRUE)
        }
}




#         function makeCacheMatrix <- function(x = matrix(data=c(1),nrow=1,ncol=1))
#                 INPUT PARAMETER: x square matrix invertible
#                 OUTPUT: Set matrix cached
makeCacheMatrix <- function(x = matrix(data=c(1),nrow=1,ncol=1)) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

#         function cacheSolve <- function(x, ...) {
#                 Version on solve function with cached parameter
cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        if(isValidInputData(solve,data,"cacheSolve ")){
                m <- solve(data, ...)
                x$setinverse(m)
                m
        }
        else{NULL} 
        
}


        




