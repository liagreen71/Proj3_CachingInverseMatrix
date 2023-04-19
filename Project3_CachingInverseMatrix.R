##
##This function creates a matrix that can cache its inverse 
makeCacheMatrix<- function(x = matrix()) {
    invs<-NULL
    set<- function(y) {
        x <<- y
        invs <<- NULL
    }
    get <- function()x
    setinvs <- function(inverse) invs <<-inverse
    getinvs <- function() invs
    list(set = set, get = get,
         setinvs = setinvs ,
         getinvs = getinvs)
}

##This function computes or retrieves the inverse of matrix from cache

cacheSolve<- function(x, ...) {
    invs<-x$getinvs()
    if(!is.null(invs)) {
        message("getting cached data")
        return(invs)
    }
    mx<-x$get()
    invs<-solve(mx,...)
    x$setinvs(invs)
    invs
    ##Returns a matrix that is the inverse of 'x' 
}

