NNImputation <- function(dataX,k){
  dataX<-kNN(dataX,k=k,imp_var=FALSE)
  return (dataX)
}
