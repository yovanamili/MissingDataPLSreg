#' Title
#'
#' @param dataX  explicative data with NA's
#' @param datay  explained data
#' @param Ncomp  number of components
#' @param NK
#' @param imputMethod
#' @param k  number of the nearest neighbors only kNN Imputation method
#' @param m number of imputations only for MICE Imputation method
#'
#' @return
#' @export
#'
#' @examples
Imputation_Data_PLSreg <- function(dataX,datay,Ncomp,NK, imputMethod,k=5, m=5){
  results <- list();
  AnalyzedDataImputation <- NULL
  if(imputMethod=="KNN"){
    dataX <- kNNImputation(dataX,k);
    AnalyzedDataImputation <- AnalyzeImputation(dataX,datay,Ncomp,NK,imputMethod=imputMethod)
  }
  else if(imputMethod=="SVD"){
    dataX <- SVDImputation(dataX, Ncomp);
    AnalyzedDataImputation <- AnalyzeImputation(dataX,datay,Ncomp,NK,imputMethod)
  }
  else{
    dataX <- MICEImputation(dataX, m);
    AnalyzedDataImputation <- AnalyzeImputation(dataX,datay,Ncomp,NK,imputMethod)
  }

  results$ComponentsNumber <- AnalyzedDataImputation

  return(results)
}



