##R Programming Course on Coursera, Assignment 2
#The goal is to make an empty "storage" for the inverse of a matrix, so it can be fetched quickly, 
#without the need to compute the inverse again if the matrix remains unchanged.

#So, the first function creates the "storage" for the inverse of any (square and non-singular) matrix x 
#in the environment different from global, enclosed in this function. It defines functions needed to: 
#1. register any changes to the matrix, 2. retrieve the matrix, 3. compute the inverse and 4. retrieve the inverse.
makeCacheMatrix<-function(x=matrix()) {
      if (nrow(x)!=ncol(x)) stop ("x is not a square matrix")      
      Inverse<-NULL
      setMatrix<-function(y) {
            x<<-y
            Inverse<<-NULL
      }
      getMatrix<-function() x
      setInverse<-function(solve) Inverse<<-solve(x)
      getInverse<-function() Inverse
      list(setMatrix=setMatrix,
           getMatrix=getMatrix,
           setInverse=setInverse,
           getInverse=getInverse)
}

#The second function actually calculates the inverse, but first it checks if the inverse, for the unchanged matrix, 
#is "stored" by the first function.
cacheSolve<-function(x,...) {
      Inverse<-x$getInverse()
      if(!is.null(Inverse)) {
            message ("getting cached data")
            return(Inverse)
      }
      data<-x$getMatrix()
      Inverse<-solve(data)
      x$setInverse(Inverse,...)
      Inverse
}


#Return a matrix that is the inverse of 'x'
A<-matrix(rnorm(25),5,5)
m<-makeCacheMatrix(A)
B<-cacheSolve(m)
B
#B should be the inverse of 'x' (that is, A in my case)

#additional:basic test
round(A%*%B, digits=0)#Matrix multiplied by its inverse results in an identity matrix.
#So, this should yield an identity matrix, with 1's on its diagonal, and otherwise zeros.
#It's rounded because we expect very small numbers that are very close to zero.