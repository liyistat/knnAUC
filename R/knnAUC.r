#' knnAUC (k-nearest neighbors AUC test)
#'
#' knnAUC : k-nearest neighbors AUC test.
#' Testing dependence/correlation of two variables is one of the fundamental tasks in statistics.
#' In this work, we proposed a new way of testing nonlinear dependence between one continuous variable (X) and one binary variable (Y).
#' We addressed this research question by using R package 'knnAUC'.
#' In the knnAUC framework, we first calculated the AUC estimator based on a k-nearest neighbors classifier, and then
#' evaluate the significance of the AUC based statistic (Alternative hypothesis: AUC > 0.5, that is to say, X has prediction power for Y).
#' @author Yi Li, \email{liyistat@@gmail.com}
#' @param x A vector containing values of a continuous variable (X).
#' @param y A vector containing values of a binary (0 or 1) discrete variable (Y).
#' @param ratio A number, the training sample size ratio.
#' @param kmax A positive integer, we'll automatically find the best parameter for knn between 1 and number kmax.
#' @return result   A list of AUC estimator and pvalue of knnAUC test.
#' @export
#' @references  Yi Li, Xiaoyu Liu, et al. knnAUC : k-nearest neighbors AUC test.BMC Bioinformatics. 2018.
#' @examples
#'#Example1
#'#Generate random variable X and random binary variable Y
#'  set.seed(1234)
#'  x<-rnorm(100, mean = 0, sd = 1)
#'  y<-rbinom(100, size = 1, prob = 0.5)
#'
#'#Test whether X and Y are independent or not using knnAUC method
#'#Calculate the average AUC estimator and pvalue of knnAUC method
#'#Set the parameters:ratio=0.5, k=50; And show the running time of knnAUC
#'  start.time <- Sys.time()
#'  library(knnAUC)
#'  knnAUC(x,y,ratio=0.5,k=50)
#'  end.time <- Sys.time()
#'  end.time - start.time
#'
#'
#'#Example2
#'#Generate random variables X and Y from logistic regression simulation
#'  set.seed(1234)
#'  N <- 100
#'  intercept <- 1
#'  beta <- 1
#'  x <- rnorm(N,mean=0,sd=1)
#'  linpred <- intercept + (x*beta)
#'  prob <- exp(linpred)/(1 + exp(linpred))
#'  runis <- runif(N,0,1)
#'  y <- ifelse(runis < prob,1,0)
#'
#'#Test whether X and Y are independent or not using knnAUC method
#'#Calculate the average AUC estimator and pvalue of knnAUC method
#'#Set the parameters:ratio=0.46, k=100; And show the running time of knnAUC
#'  library(knnAUC)
#'  start.time <- Sys.time()
#'  knnAUC(x,y,ratio=0.46,k=100)
#'  end.time <- Sys.time()
#'  end.time - start.time
knnAUC<-function(x,y,ratio=0.46,k=100){
  if((k<0)|(ratio<0)) {stop("Parameters ratio and k should be positive")}
  if((ratio>=1)) {stop("Parameters ratio should be less than 1")}
  data<-data.table::data.table(y,x)
  #repeat loop to resample data
  repeat{
    n<-nrow(data)
    size<-round(n*ratio)
    s<-sample.int(nrow(data),size,replace=F)
    train<-data[s,]
    test<-data[-s,]
    if((length(unique(train$y))>1)&(length(unique(test$y))>1)){
      break
    }
  }
  #update idx and results
  results<-c(rep(0, nrow(data)))
  idx <- c(rep(0, nrow(data)))
  f<-function(j){
    if((length(unique(train$y))>1)){
      #testy and yhat by knn
      yhat<-knn(train,test,k)
      results[-s] <<- results[-s]+yhat
      idx[-s] <<- idx[-s] + 1
    }
    return(ifelse(j>0,TRUE,FALSE))
  }
  purrr::map_lgl(1,function(j) f(j))
  #mark samples not selected
  idx[which(idx == 0)] <- NA
  #get yhat
  get_yhat <- results/idx
  #auc test
  testy<- data$y[which(is.na(get_yhat)==F)]
  yhat<-  get_yhat[which(is.na(get_yhat)==F)]
  auc<- auc.test(testy,yhat)
  return(auc)
}
knnAUC<-compiler::cmpfun(knnAUC)
