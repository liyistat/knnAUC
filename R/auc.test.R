#' AUC based independence test
#'
#' AUC based independence test.
#' We evaluate the significance of the AUC based statistic (Null hypothesis: AUC = 0.5, that is to say, X have no prediction ability for Y).
#' @author Yi Li, \email{liyistat@@gmail.com}
#' @param testy A binary (0 or 1) vector, which will be predicted.
#' @param yhat A vector, prediction probabilities of testy equals to 1.
#' @return result   A list of average AUC estimator and pvalue of AUC based significance test.
#' @export
#' @references  Yi Li, Xiaoyu Liu, et al. knnAUC : k-nearest neighbors AUC test.BMC Bioinformatics.2018.
#' @references  Mason, S. J. and Graham, N. E. (2002), Areas beneath the relative operating characteristics (ROC) and relative operating levels (ROL) curves: Statistical significance and interpretation. Q.J.R. Meteorol. Soc. doi:10.1256/003590002320603584

auc.test<-function(testy,yhat){
  pred <- ROCR::prediction(yhat,testy)
  auc<-attributes(ROCR::performance(pred, 'auc'))$y.values[[1]]
  pvalue<-verification::roc.area(testy,yhat)$p.value
  result<-list(AUC=auc,p.value=pvalue)
  return(result)
}
auc.test<-compiler::cmpfun(auc.test)
