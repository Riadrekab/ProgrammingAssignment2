
makeCacheMatrix <- function(x = matrix()) {
    inv<-NULL
    set<-function(y) {
        x<<-y 
        inv<<-NULL
    }
    get <- function()x
    setinv<-function(inv) x<-inv
    getinv<-function() inv
    list(set=set,get=get,setinv=setinv,getinv=getinv)
}

cacheSolve <- function(x, ...) {
    inv<-x$getinv()
    if(!is.null(inv))
    {
        return(inv)
    }
    data<-x$get()
    inv<-solve(data)
    x$setinv(inv)
    inv
}
