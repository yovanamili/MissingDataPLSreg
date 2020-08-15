AnalyzeImputation <- function(dataX, datay, Ncomp, NK, imputMethod){
  ImputationRegresion <- NULL
  results <- list()
  ImputationRegresion <-PLS_lm(datay,dataX,Ncomp,typeVC="standard")

  if(Ncomp == 1){
    cv.modpls<-cv.plsR(datay,dataX,nt= 1,NK=NK)}
  else if(Ncomp == 2){
    cv.modpls<-cv.plsR(datay,dataX,nt= 2,NK=NK)}
  else if(Ncomp == 3){
    cv.modpls<-cv.plsR(datay,dataX,nt= 3,NK=NK)}
  else if(Ncomp == 4){
    cv.modpls<-cv.plsR(datay,dataX,nt= 4,NK=NK)}
  else if(Ncomp == 5){
    cv.modpls<-cv.plsR(datay,dataX,nt= 5,NK=NK)}
  else if(Ncomp == 6){
    cv.modpls<-cv.plsR(datay,dataX,nt= 6,NK=NK)}
  else if(Ncomp == 7){
    cv.modpls<-cv.plsR(datay,dataX,nt= 7,NK=NK)}
  else if(Ncomp == 8){
    cv.modpls<-cv.plsR(datay,dataX,nt= 8,NK=NK)}
  else if(Ncomp == 9){
    cv.modpls<-cv.plsR(datay,dataX,nt= 9,NK=NK)}
  else {
    cv.modpls<-cv.plsR(datay,dataX,nt= 10,NK=NK)}
  res.cv.modpls<-cvtable(summary(cv.modpls)) # calcul de nombre de composantes par validation croisse

  results$Number_components_CV<-res.cv.modpls[1]
  results$CVPRESSCriteria<-res.cv.modpls[2]
  results$imputMethod <- imputMethod
  results$Coeffs <- ImputationRegresion$Std.Coeffs
  results$AICperComposant <- ImputationRegresion$AIC
  results$plot_components <- plot(res.cv.modpls)

  return(results)
}
