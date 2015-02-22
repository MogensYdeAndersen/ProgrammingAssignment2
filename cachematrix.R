## The functions "makeCacheMatrix" and "cacheSolve" in kombination calculates
## the inverse matrix of a matrix, but in computational rational way.
## Since calculating af matrix and a inverse matrix demands computational
## power, the second function "cacheSolve" avoids to recalculate, if a
## user has not changed the vlue of the input matrix. In this case the
## second function "only" retunrs the cached value

## The makeCacheMatrix set and gets the input matrix and set og gets the solve
## function (= inverse matrix function)

makeCacheMatrix <- function(x = matrix()) {
      m<-NULL
      set<-function(y){
              x<<-y
              m<<-NULL
              }
      get<-function() x
      setmatrix<-function(solve) m<<- solve
      getmatrix<-function() m
      list(set=set, get=get,
      setmatrix=setmatrix,
      getmatrix=getmatrix)
}

## Return a matrix that is the inverse of 'x'
## The cacheSolve function is the energisafer function. If user has not
## inputted a new matrix, cacheColve gets cached inverted matrix data.
##If user changed input matrix, cacheSolve recalculates inverted matrix data.

cacheSolve <- function(x, ...) {

cacheSolve <- function(x=matrix(), ...) {
      m<-x$getmatrix()
      if(!is.null(m)){
              message("getting cached inverted matrix data")
              return(m)
              }
      matrix<-x$get()
      m<-solve(matrix, ...)
      x$setmatrix(m)
      m
}