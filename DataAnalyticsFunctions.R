## This file contains several functions used in the course

##
## These codes are used for installing packages
## function for installing needed packages

installpkg <- function(x){
  if(x %in% rownames(installed.packages())==FALSE) {
    if(x %in% rownames(available.packages())==FALSE) {
      paste(x,"is not a valid package - please check again...")
    } else {
      install.packages(x)           
    }
    
  } else {
    paste(x,"package already installed...")
  }
}

###################
###################
### Used on Class 3 and Online Test 3

### FPR_TPR
FPR_TPR <- function(prediction, actual){
  
  TP <- sum((prediction)*(actual))
  FP <- sum((prediction)*(!actual))
  FN <- sum((!prediction)*(actual))
  TN <- sum((!prediction)*(!actual))
  result <- data.frame( FPR = FP / (FP + TN), TPR = TP / (TP + FN), ACC = (TP+TN)/(TP+TN+FP+FN) )
  
  return (result)
}

BinaryAccuracy <- function(prediction, actual){

  TP <- sum((prediction)*(actual))
  FP <- sum((prediction)*(!actual))
  FN <- sum((!prediction)*(actual))
  TN <- sum((!prediction)*(!actual))
  result <-  (TP+TN)/(TP+TN+FP+FN) 
  
  return (result)
}

#################################################
### Functions used in Class 4 and Online Test 4
#  
## deviance calculations

## pred must be probabilities (0<pred<1) for binomial
deviance <- function(y, pred, family=c("gaussian","binomial")){
  family <- match.arg(family)
  if(family=="gaussian"){
    return( sum( (y-pred)^2 ) )
  }else{
    if(is.factor(y)) y <- as.numeric(y)>1
    return( -2*sum( y*log(pred) + (1-y)*log(1-pred) ) )
  }
}

devianceQR <- function(y, pred, tau){
  return( sum(  tau*max(0, y-pred ) + (1-tau)*max(0, pred-y ) ) )
  
}

## get null devaince too, and return R2
R2 <- function(y, pred, family=c("gaussian","binomial")){
  fam <- match.arg(family)
  if(fam=="binomial"){
    if(is.factor(y)){ y <- as.numeric(y)>1 }
  }
  dev <- deviance(y, pred, family=fam)
  dev0 <- deviance(y, mean(y), family=fam)
  return(1-dev/dev0)
}


### Returns the indices for which |x[i]| > tr
support<- function(x, tr = 10e-6) {
  m<- rep(0, length(x))
  for (i in 1:length(x)) if( abs(x[i])> tr ) m[i]<- i
  m <- m[m>0]
  m
}

### Penalty choice for Quantile Regression
lambda.BC<- function(X, R = 1000, tau = 0.5, c = 1, alpha = .05){
  n <- nrow(X)
  norm2n<-function(z){sqrt(mean(z^2))}
  sigs <- apply(X,2,norm2n)
  U <- matrix(runif(n * R),n)
  R <- (t(X) %*% (tau - (U < tau)))/(sigs*sqrt(tau*(1-tau)))
  r <- apply(abs(R),2,max)
  c * quantile(r, 1 - alpha) * sqrt(tau*(1-tau))*c(1,sigs)
}

### Used in Class 5, Class 9 and Online Test 5
## Selects the Number of Clusters via an Information Criteria
## get AIC (option "A"), BIC (option "B"), HDIC (option "C") for the output of kmeans
kIC <- function(fit, rule=c("A","B","C")){
  df <- length(fit$centers) # K*dim
  n <- sum(fit$size)
  D <- fit$tot.withinss # deviance
  rule=match.arg(rule)
  if(rule=="A")
    #return(D + 2*df*n/max(1,n-df-1))
    return(D + 2*df)
  else if(rule=="B") 
    return(D + log(n)*df)
  else 
    return(D +  sqrt( n * log(df) )*df)
}
