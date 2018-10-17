#' K-nearest neighbors classifier
#'
#' K-nearest neighbors classifier.
#' We'll automatically find the best parameter k for knn from RWeka.
#' @author Yi Li, \email{liyistat@@gmail.com}
#' @param train A training dataset containing both a continuous variable (X) and a binary (0 or 1) variable (Y).
#' @param test A testing dataset containing both a continuous variable (X) and a binary (0 or 1) variable (Y).
#' @param k A positive integer, we'll automatically find the best parameter for knn between 1 and number k.
#' @return yhat A vector, prediction probabilities of testy equals to 1.
#' @export
#' @references Yi Li, Xiaoyu Liu, et al. knnAUC : k-nearest neighbors AUC test. BMC Bioinformatics. 2018.

knn<-function(train,test,k){
  #train$y<-as.factor(train$y)
  classifier <- RWeka::IBk(as.factor(y) ~ ., data = train, control = RWeka::Weka_control(K = k, X = TRUE))
  yhat<-stats::predict(classifier,test,type="probability")[,2]
  return(yhat)
}
knn<-compiler::cmpfun(knn)
