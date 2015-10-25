## The main purpose of the following two functions
## are to demonstrate the ability of 'R' to cash
## resource intensive calculations so that it is not 
## necesarry to redo the calculation every time it is 
## needed. 

##They will:

## 1.) Create a matrix, solve the inverse of said 
##     matrix and cash inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
  ##create empty matrix to store the inverse matrix in
  ##since the matrix have not yet been set, the inverse 
  ##matrix is set to NULL
  invmat<-NUll
  ##Set the matrix
  setMatr <- function(matr){
    x<<-matr
    ##Value of matrix has changed so invmat has to be reset to NULL
    invmat<-NUll
  }
  #Return stored matrix
  getMatr<-function(){
    x
  }
  ##Cash argument
  cachInv<-function(matinv){
    invmat<<-matinv
  }
  ##Get value cashed
  getinv<-function(){
    invmat
  }
  ##Return a list containing the named elements, all functions
  list(setMatr=setMatr,
       getMatr=getMatr,
       cachInv=cachInv,
       getinv=getinv)
}

## 2.) Returns the inverse matrix created above by
##     solving the matrix if it have not already been 
##     solved or if it has already been solved, fetching
##     it from the cash.

cacheSolve <- function(x, ...) {
  ##Retrieve the cashed inverse
  invck<-x$getinv()
  ##Check if cashed value exists and return it if it does
  if(!is.null(invck)){
    message("Retrieving cashed data")
    return(invck)
  }
  ##If cash is empty, get matrix, calculate invers and cash result
  matrx<-x$getMatr()
  invck<-solve(matrx)
  x$cachInv(invck)
  #Return result
  invck
}
