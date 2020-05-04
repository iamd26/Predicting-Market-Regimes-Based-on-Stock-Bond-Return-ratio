#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Set up ----
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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

path = getwd()
setwd(path)
source("bullbeaR.R")

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#Import time series of prices
df <- read.csv("^GSPC.csv", header=TRUE, sep = ",", na.strings = "", stringsAsFactors=FALSE)
bonddf <- read.csv("DGS10.csv", header=TRUE, sep = ",", na.strings = "", stringsAsFactors=FALSE)

#Set column names to expected values
names(df) <- c("date","open",	"high","low",	"value", "adj close",	"volume")
names(bonddf) <- c("date", "DGS10")
#Convert to date
df$date <- as.Date(as.character(df$date),"%m/%d/%Y")
# To match the timeline with each other
# Drop off the data point in 2020 (Covid 19)
stock <- subset(df, date >= "1962-01-01" & date <= "2019-12-01")
bonddf <- subset(bonddf, date >= "1962-01-01" & date <= "2019-12-01")

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Define ratio
# Option 1
stock$value <- log(stock$value/bonddf$DGS10)
# Option 2: Drops crazily in 2020
# stock$value <- log(stock$value)/log(bonddf$DGS10)
# Option 3: 
# stock$value <- log(stock$value)/bonddf$DGS10
# Option 4
#stock$value <- log(stock$value[2:length(stock$value)]/stock$value)/log(1 + bonddf$DGS10/100)
#stock$value[length(stock$value)] <- 0
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Plot ratio
ggplot()+
  geom_line(data = stock, aes(date, value), color = "red")+
  ggtitle("Ratio")+
  xlab("Date")+
  ylab("Ratio")



#Set Model
#model = "PS"
model = "LT"
#model = "TD"

yourname = "XXX"

mod <- new(model, name= yourname, ts = stock)

mod$units            = "months"
mod$plotinterval     = "1 month"
mod$plotdateformat   = "%Y%m"
mod$plotstartdate    = as.Date('1962-01-01')
mod$plotenddate      = as.Date('2019-12-31')
mod$ylabels          = c(100,500,2500)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Model PS
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#Pagan, A.R. and Sossounov, K.A., 2003. 
#A simple framework for analysing bull and bear markets. 
#Journal of Applied Econometrics, 18(1), pp.23-46.

if (model=="PS"){
  
  mod$window = 8*30.5
  mod$endbuffer = 6*30.5
  mod$mincycle = 2*mod$window
  mod$minphase = 4*30.5;
  mod$minphaseexception = 0.05
}

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Model LT
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#Lunde, A. and Timmermann, A., 2004. 
#Duration dependence in stock prices: An analysis of bull and bear markets. 
#Journal of Business & Economic Statistics, 22(3), pp.253-273.

if (model=="LT"){
  
  mod$initwindow = 30.5*6;
  
  mod$minbull <- 0.05
  mod$minbear <- mod$minbull/(1+mod$minbull)
  
}

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Model TD
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#Hanna, A.J., 2018. 
#A top-down approach to identifying bull and bear market states. 
#International Review of Financial Analysis, 55, pp.93-110.

if (model=="TD"){
  
  mod$initwindow = 30.5*6
  
  mod$minbull <- 0.05
  mod$minbear <- mod$minbull/(1+mod$minbull)
  
  mod$mincycle <- 0.0
  mod$minphase <- 0.0
  mod$window   <- 0.0
  
  mod$minphaseexception <- 100.0
  
}


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Run Model
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

mod$turningpoints()

mod$plot() 


turningpoints <- read.csv("df.csv")

# Match the turning point on to the ratio table
names(turningpoints) <- c("x", "date", "open", "high", "low", "value", "adj.close", "volume", "peak", "trough", "group")
turningpoints$date <- as.Date(turningpoints$date, format = "%Y-%m-%d")
# Left join two tables
library(dplyr)
ratioTable <- left_join(stock, turningpoints, by = "date")
ratioTable <- ratioTable[, c(1, 2, 3, 4, 5, 15, 16, 17)]
# Fill in the NA (MIGHT CUT OFF SOME OF THE VALUE IN THE BEGINNING)
library(zoo)
ratio <- na.locf(ratioTable)

# Matching Unemployment Rate (Freq: monthly)
UNRATE <- read.csv("UNRATE.csv")
names(UNRATE) <- c("date", "UNRATE")
UNRATE$date <- as.Date(UNRATE$date, format = "%Y-%m-%d")
ratio <- left_join(ratio, UNRATE, by = "date")

# Matching Industrial Production (Freq: monthly)
INDPRO <- read.csv("INDPRO.csv")
names(INDPRO) <- c("date", "INDPRO")
INDPRO$date <- as.Date(INDPRO$date, format = "%Y-%m-%d")
ratio <- left_join(ratio, INDPRO, by = "date")

# Matching Consumer Spending (PCE) (Freq: monthly)
PCE <- read.csv("PCE.csv")
names(PCE) <- c("date", "PCE")
PCE$date <- as.Date(PCE$date, format = "%Y-%m-%d")
ratio <- left_join(ratio, PCE, by = "date")

# Matching CPI for inflation
CPI <- read.csv("CPIAUCSL.csv")
names(CPI) <- c("date", "CPI")
CPI$date <- as.Date(CPI$date, format = "%Y-%m-%d")
ratio <- left_join(ratio, CPI, by = "date")

# Adjust the order of the table
ratio <- ratio[, c(1, 2, 3, 4, 5, 9, 10, 11, 12, 6, 7, 8)]
#nrow(ratio) <- 641

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Machine learning
# Load functions
source("DataAnalyticsFunctions.R")
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
set.seed(1)
head(ratio)
ratioData <- ratio[, c(5, 6, 7, 8, 9, 12)]
names(ratioData) <- c("Ratio", "UNRATE", "INDPRO", "PCE", "CPI", "Group")
head(ratioData)
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#Base Model: logistic  
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Split in 2 groups
nfold <- 2 
# the number of observations
n <- nrow(ratioData) 
# create a vector of fold memberships (random order)
foldid <- rep(1:nfold,each=ceiling(n/nfold))[sample(1:n)]

model.logistic <-glm(Group~., data=ratioData, subset=which(foldid==1), family="binomial")

# Find the R square
# R2 function
R2 <- function(y, pred, family=c("gaussian","binomial")){
  fam <- match.arg(family)
  if(fam=="binomial"){
    if(is.factor(y)){ y <- as.numeric(y)>1 }
  }
  dev <- deviance(y, pred, family=fam)
  dev0 <- deviance(y, mean(y), family=fam)
  return(1-dev/dev0)
}

R2.model.logistic <- R2(y=ratioData$Group[foldid==1], pred=model.logistic$fitted, family="binomial")

# Model Fit via R2 
M1<- c("Summary of R2 for logistic:" )
M2<- paste( " Logistic regression:                   R2 = ", R2.model.logistic)

cat(M1,M2)

par(mar=c(1.5,1.5,1,1))
par(mai=c(1.5,1.5,1,1))

barplot(c(R2.model.logistic), las=2, ylim=c(.00,.10), xlab="", names = c( "logistic "), ylab = bquote(R^2))


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#Base model, Lasso and Post Lasso, SVM & OOS performance 
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#OOS R^2 performance of Base model, Lasso and Post Lasso for supplement 
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#we did 10-Fold cross validation which splits the data into 10 "folds". 
# The code below allows for that by setting the variable 
# nfold to the value you would like.
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# create a vector of fold memberships (random order)
nfold <- 10
n <- nrow(ratioData) 
foldid <- rep(1:nfold,each=ceiling(n/nfold))[sample(1:n)]

# create an empty dataframe of results
OOS <- data.frame(logistic=rep(NA,nfold)) 

# Use a for loop to run through the nfold trails
for(k in 1:nfold){ 
  train <- which(foldid!=k) # train on all but fold `k'
  
  ## fit the logistic model
  model.logistic <-glm(Group~., data=ratioData, subset=train,family="binomial")
  pred.logistic <- predict(model.logistic, newdata=ratioData[-train,], type="response")
  
  ## calculate and log R2
  # Logistic
  OOS$logistic[k] <- R2(y=ratioData$Group[-train], pred=pred.logistic, family="binomial")
  OOS$logistic[k]
  
  ## We will loop this nfold times (I setup for 10)
  ## this will print the progress (iteration that finished)
  print(paste("Iteration",k,"of",nfold,"(thank you for your patience)"))
}

# List the mean of the results stored in the dataframe OOS
# we have nfold values in OOS for the model, this computes the mean of them
colMeans(OOS)
m.OOS <- as.matrix(OOS)
rownames(m.OOS) <- c(1:nfold)
barplot(t(as.matrix(OOS)), beside=TRUE, legend=TRUE, args.legend=c(xjust=1, yjust=0.5),
        ylab= bquote( "Out of Sample " ~ R^2), xlab="Fold", names.arg = c(1:10))

# This plots a box plot with the performance of the base model
if (nfold >= 10){
  names(OOS)[1] <-"logistic"
  boxplot(OOS, col="plum", las = 2, ylab=expression(paste("OOS ",R^2)), xlab="", main="10-fold Cross Validation")
}

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Next we start with Regularization via Lasso & Post Lasso
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Lasso setup 
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# install package for Lasso
install.packages("glmnet")
library(glmnet)
# Lets run Lasso
# First lets set up the data for it
# the features need to be a matrix ([,-1] removes the first column which is the intercept)
Mx<- model.matrix(Group ~ .^2, data=ratioData)[,-1]
My<- ratioData$Group == "0"
# This defined the features we will use the matrix Mx (X) and the target My (Y)

# Lasso requires a penalty parameter lambda

num.features <- ncol(Mx)
num.n <- nrow(Mx)
num.Group <- sum(My)
w <- (num.Group/num.n)*(1-(num.Group/num.n))
# For the binomial case, a theoretically valid choice is
lambda.theory <- sqrt(w*log(num.features/0.05)/num.n)

# next we call Lasso providing the 
# features as matrix Mx
# the target as a vector My
# telling it is for logistic, family="binomial"
# and specifying lambda = lambda.theory
lassoTheory <- glmnet(Mx,My, family="binomial",lambda = lambda.theory)
# by calling the summary we see the list of object in sclassoTheory
summary(lassoTheory)
# lassoTheory$a0 gives you the intercept
# lassoTheory$beta gives you all the coefficients. Many were set to zero.
# lassoTheory$lambda has the value of lambda stored.
# We can see the support of the selected coeffcients
# these are the indices
support(lassoTheory$beta)
# these are the labels
colnames(Mx)[support(lassoTheory$beta)]
# there are in total
length(support(lassoTheory$beta))
# coefficients selected by the model using the theoretical choice 


# If we omit the lambda, the function will solve for all values of lambda
# it takes a bit longer but not much longer at all. 
lasso <- glmnet(Mx,My, family="binomial")

# By running for all vaules of lambda we get many models back
# now we have the summary
summary(lasso)

# the length of lambda = 91. This means that it stored 
# the solutions for 91 different values of lambda.
# now lasso$a0 is a vector with 91 components (one for each lambda)
# and lasso$beta is a matrix of 1365 components 
# (each columns corresponds to a solution for a value of lambda)
# These are the first 5 values
lasso$lambda[1:5]

# They are in decreasing order so the most sparse solution is beta[,1]
# For each coefficient we can plot its "Path" as we change lambda
par(mar=c(1.5,1.5,0.75,1.5))
par(mai=c(1.5,1.5,0.75,1.5))

# Make 2 plots side by side: mfrow=c(1,2)  # c(nrow,ncol)
par(mfrow=c(1,2))
coef_ind <- 5
par(mar=c(1.5,0.5,0.75,0.5))
par(mai=c(1.5,0.5,0.75,0.5))
plot(log(lasso$lambda),lasso$beta[coef_ind,], ylab="Coefficient value", main=paste("Coefficient for",colnames(Mx)[coef_ind]),xlab = expression(paste("log(",lambda,")")),type="l")
coef_ind <- 2
par(mar=c(1,1,1,1))
par(mai=c(1.5,0.5,0.75,0.5))

plot(log(lasso$lambda),lasso$beta[coef_ind,], ylab="Coefficient value", main=paste("Coefficient for",colnames(Mx)[coef_ind]),xlab = expression(paste("log(",lambda,")")),type="l")

# make it back to 1 plot only
par(mfrow=c(1,1))
par(mar=c(1,1,1,1))
par(mai=c(1,1,1,1))

# we can see all the coefficients
# (note that the x axis is not lambda but L1 norm.
# The L1 norm is large when lambda is small and vice-versa.)
plot(lasso, xvar="lambda", main="# of non-zero coefficients", ylab ="Coefficient values", xlab = expression(paste("log(",lambda,")")))

# Now that we can compute the Lasso for all values of lambda,
# the whole "path" of models can be evaluated by a OOS experiment
# we can attempt to use cross valiadation to actually pick lambda.
# the following command yields a cross validation procedure
# the following command takes some time.
lassoCV <- cv.glmnet(Mx,My, family="binomial")

# We can plot the fitting graph
# red dots are mean values and the bars are the uncertainty
par(mar=c(1,1,1,1))
par(mai=c(1,1,1,1))
plot(lassoCV, main="Fitting Graph for CV Lasso \n \n # of non-zero coefficients  ", xlab = expression(paste("log(",lambda,")")))

# There are some rules that people like to use:
# The minimum of the mean values stored in lambda.min
# 1 standard error to the right stored in lambda.1se
# if we had to compute lambda.min we can simply write
lassoCV$lambda[which.min(lassoCV$cvm)]
# sclassoCV$cvm has the mean values
# which.min(sclassoCV$cvm) returns the index that minimizes it
# and remember that sclassoCV$lambda is the vector of lambda
# in any case we have lambda.min and lambda.1se.
# lambda.min is perceived as aggressive (picks too many variables)
# lambda.1se is perceived as conservative (picks too few variables)


# we plot them in the previous picture
text(log(lassoCV$lambda.min), .95,"min",cex=1)
text(log(lassoCV$lambda.1se), 1,"1se",cex=1)

lines(c(log(lambda.theory),log(lambda.theory)),c(0.3,2.4),lty=3,col="blue")
text(log(lambda.theory), 1.05,"theory",cex=1)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Post Lasso & OOS R^2 of Post Lasso, Lasso
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# we sepect a model based on the proposed rules
# and perform 10-fold cross validation

PL.OOS <- data.frame(Postlasso.min=rep(NA,nfold), Postlasso.1se=rep(NA,nfold), Postlasso.theory=rep(NA,nfold)) 
L.OOS <- data.frame(Lasso.min=rep(NA,nfold), Lasso.1se=rep(NA,nfold), Lasso.theory=rep(NA,nfold)) 
features.min <- support(lasso$beta[,which.min(lassoCV$cvm)])
length(features.min)
features.1se <- support(lasso$beta[,which.min( (lassoCV$lambda-lassoCV$lambda.1se)^2)])
length(features.1se) 
features.theory <- support(lassoTheory$beta)
length(features.theory)

data.min <- data.frame(Mx[,features.min],My)
data.1se <- data.frame(Mx[,features.1se],My)
data.theory <- data.frame(Mx[,features.theory],My)

for(k in 1:nfold){ 
  train <- which(foldid!=k) # train on all but fold `k'
  
  ### This is the CV for the Post Lasso Estimates
  rmin <- glm(My~., data=data.min, subset=train, family="binomial")
  if ( length(features.1se) == 0){  r1se <- glm(Group~0, data=ratioData, subset=train, family="binomial") 
  } else {r1se <- glm(My~., data=data.1se, subset=train, family="binomial")
  }
  
  if ( length(features.theory) == 0){ 
    rtheory <- glm(Group~0, data=ratioData, subset=train, family="binomial") 
  } else {rtheory <- glm(My~., data=data.theory, subset=train, family="binomial") }
  
  
  predmin <- predict(rmin, newdata=data.min[-train,], type="response")
  pred1se  <- predict(r1se, newdata=data.1se[-train,], type="response")
  predtheory <- predict(rtheory, newdata=data.theory[-train,], type="response")
  PL.OOS$Postlasso.min[k] <- R2(y=My[-train], pred=predmin, family="binomial")
  PL.OOS$Postlasso.1se[k] <- R2(y=My[-train], pred=pred1se, family="binomial")
  PL.OOS$Postlasso.theory[k] <- R2(y=My[-train], pred=predtheory, family="binomial")
  
  ### This is the CV for the Lasso estimates  
  lassomin  <- glmnet(Mx[train,],My[train], family="binomial",lambda = lassoCV$lambda.min)
  lasso1se  <- glmnet(Mx[train,],My[train], family="binomial",lambda = lassoCV$lambda.1se)
  lassoTheory <- glmnet(Mx[train,],My[train], family="binomial",lambda = lambda.theory)
  
  predlassomin <- predict(lassomin, newx=Mx[-train,], type="response")
  predlasso1se  <- predict(lasso1se, newx=Mx[-train,], type="response")
  predlassotheory <- predict(lassoTheory, newx=Mx[-train,], type="response")
  L.OOS$Lasso.min[k] <- R2(y=My[-train], pred=predlassomin, family="binomial")
  L.OOS$Lasso.1se[k] <- R2(y=My[-train], pred=predlasso1se, family="binomial")
  L.OOS$Lasso.theory[k] <- R2(y=My[-train], pred=predlassotheory, family="binomial")
  
  print(paste("Iteration",k,"of",nfold,"completed"))
}

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
R2performance <- cbind(PL.OOS,L.OOS,OOS)
par( mar=  c(8, 4, 4, 2) + 0.6 )
names(OOS)[1] <-"logistic"

# OOS R square performance 
barplot(colMeans(R2performance), las=2,xpd=FALSE, ylim=c(0,.2) , xlab="", ylab = bquote( "Average Out of Sample " ~ R^2))
m.OOS <- as.matrix(R2performance)
rownames(m.OOS) <- c(1:nfold)
par(mar=c(1,1,1,1))
par(mai=c(1,1,1,1))
barplot(t(as.matrix(m.OOS)), beside=TRUE, ylim=c(0,.8) ,legend=TRUE, args.legend=c(x= "topright", y=0.92, bty = "n"),
        ylab= bquote( "Out of Sample " ~ R^2), xlab="Fold", names.arg = c(1:10))


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#OOS Accuracy test 
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#Base Model; Lasso; Post Lasso; SVM
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
installpkg("e1071")
installpkg("rpart")
library(e1071)
library(rpart)

# 10 Fold Cross Validation
#
# create a vector of fold memberships (random order)
nfold <- 10
foldid <- rep(1:nfold,each=ceiling(n/nfold))[sample(1:n)]
# create an empty dataframe of results

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#Support Vector Machine
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
SVM.OOS <- data.frame(svm=rep(NA,nfold)) 
OOS <- data.frame(logistic=rep(NA,nfold)) 

#To find the best parameters (cost, gamma) we used the following line. This line takes time to run 
obj <- tune.svm(Group~., data = ratioData[which(foldid==1),],gamma = 2^(-2:2), cost = 2^(1:15))
summary(obj)

### Use a for loop to run through the nfold trails
for(k in 1:nfold){ 
  train <- which(foldid!=k) # train on all but fold `k'
  
  ## fit the two regressions and null model
  #theoretically, we can find the gamma and cost for each train set and set to the model; however it takes a significant amount of time to run
  #C is the cost of misclassification
  #A large C gives you low bias and high variance. Low bias because you penalize the cost of missclasification a lot.
  #A small C gives you higher bias and lower variance.
  #Gamma is the parameter of a Gaussian Kernel (to handle non-linear classification).
  #
  obj <- tune.svm(Group~., data = ratioData[ train, ],gamma = 2^(-2:2), cost = 2^(1:15))
  #model.svm <- svm(Group~., data = ratioData[train,], type="C-classification", cost = obj$best.parameters$cost, gamma=obj$best.parameters$gamma)
  model.svm <- svm(Group~., data = ratioData[train,], type="C-classification", cost = 2, gamma=4)

  
  ## get predictions: type=response so we have probabilities
  pred.svm  <- predict(model.svm,ratioData[-train,])
  pred.svm  <- pred.svm[1:nrow(ratioData[-train,])]
  pred.svm <- data.frame(keyName=names(pred.svm), value=pred.svm, row.names=NULL)
  
  ## calculate and log R2
  ##SVM
  pred.svm$value <- as.numeric(as.character(pred.svm$value))
  ## SVM
  SVM.OOS$TP[k] <- sum((pred.svm$value == 1 )*(ratioData[-train,]$Group==1))
  SVM.OOS$FP[k] <- sum((pred.svm$value == 1 )*(ratioData[-train,]$Group==0))
  SVM.OOS$FN[k] <- sum((pred.svm$value == 0 )*(ratioData[-train,]$Group==1))
  SVM.OOS$TN[k] <- sum((pred.svm$value == 0 )*(ratioData[-train,]$Group==0))
  SVM.OOS$svm[k] <- (SVM.OOS$TP[k]+SVM.OOS$TN[k])/(SVM.OOS$TP[k]+SVM.OOS$FP[k]+SVM.OOS$FN[k]+SVM.OOS$TN[k])
  SVM.OOS$svm[k]
  
  ### We will loop this fold 10 times 
  ### this will print the progress (iteration that finished)
  print(paste("Iteration",k,"of",nfold,"(thank you for your patience)"))
}

# Plot the OOS Accuracy Performance
barplot(colMeans(SVM.OOS), las=2,xpd=FALSE, ylim=c(0,1) , xlab="", ylab = bquote( "Average Out of Sample " ))
mean(SVM.OOS$svm)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#Lasso, Post Lasso, Base model OOS Accuracy 
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

PL.OOS.TP <- data.frame(Postlasso.min=rep(NA,nfold), Postlasso.1se=rep(NA,nfold), Postlasso.theory=rep(NA,nfold)) 
L.OOS.TP <- data.frame(Lasso.min=rep(NA,nfold), Lasso.1se=rep(NA,nfold), Lasso.theory=rep(NA,nfold)) 
PL.OOS.TN <- data.frame(Postlasso.min=rep(NA,nfold), Postlasso.1se=rep(NA,nfold), Postlasso.theory=rep(NA,nfold)) 
L.OOS.TN <- data.frame(Lasso.min=rep(NA,nfold), Lasso.1se=rep(NA,nfold), Lasso.theory=rep(NA,nfold)) 
PL.OOS.FP <- data.frame(Postlasso.min=rep(NA,nfold), Postlasso.1se=rep(NA,nfold), Postlasso.theory=rep(NA,nfold)) 
L.OOS.FP <- data.frame(Lasso.min=rep(NA,nfold), Lasso.1se=rep(NA,nfold), Lasso.theory=rep(NA,nfold)) 
PL.OOS.FN <- data.frame(Postlasso.min=rep(NA,nfold), Postlasso.1se=rep(NA,nfold), Postlasso.theory=rep(NA,nfold)) 
L.OOS.FN <- data.frame(Lasso.min=rep(NA,nfold), Lasso.1se=rep(NA,nfold), Lasso.theory=rep(NA,nfold)) 


OOS.TP <- data.frame(logistic=rep(NA,nfold)) 
OOS.TN <- data.frame(logistic=rep(NA,nfold))  
OOS.FP <- data.frame(logistic=rep(NA,nfold)) 
OOS.FN <- data.frame(logistic=rep(NA,nfold)) 


# Set the threshold for FPR_TPR test 
#FPR- False Positive Rate
#TPR-True Positivd Rate
#FNR- False Negative Rate
#TNR-True Negative Rate
val <- .5

# FPR_TPR
FPR_TPR_re <- function(prediction, actual){
  
  TP <- sum((prediction)*(actual))
  FP <- sum((prediction)*(!actual))
  FN <- sum((!prediction)*(actual))
  TN <- sum((!prediction)*(!actual))
  result <- data.frame( FPR = FP / (FP + TN), TPR = TP / (TP + FN), FN, TN, ACC = (TP+TN)/(TP+TN+FP+FN) )
  
  return (result)
}

for(k in 1:nfold){ 
  train <- which(foldid!=k) # train on all but fold `k'
  
  ### This is the CV for the Post Lasso Estimates
  rmin <- glm(My~., data=data.min, subset=train, family="binomial")
  if ( length(features.1se) == 0){  r1se <- glm(Group~0, data=ratioData, subset=train, family="binomial") 
  } else {r1se <- glm(My~., data=data.1se, subset=train, family="binomial")
  }
  
  if ( length(features.theory) == 0){ 
    rtheory <- glm(Group~0, data=ratioData, subset=train, family="binomial") 
  } else {rtheory <- glm(My~., data=data.theory, subset=train, family="binomial") }
  
  
  predmin <- predict(rmin, newdata=data.min[-train,], type="response")
  pred1se  <- predict(r1se, newdata=data.1se[-train,], type="response")
  predtheory <- predict(rtheory, newdata=data.theory[-train,], type="response")
  
  values <- FPR_TPR_re( (predmin >= val) , My[-train] )
  PL.OOS$Postlasso.min[k] <- values$ACC
  PL.OOS.TP$Postlasso.min[k] <- values$TP
  PL.OOS.TN$Postlasso.min[k] <- values$TN
  PL.OOS.FP$Postlasso.min[k] <- values$FP
  PL.OOS.FN$Postlasso.min[k] <- values$FN
  
  values <- FPR_TPR_re( (pred1se >= val) , My[-train] )
  PL.OOS$Postlasso.1se[k] <- values$ACC
  PL.OOS.TP$Postlasso.1se[k] <- values$TP
  PL.OOS.FP$Postlasso.1se[k] <- values$FP
  PL.OOS.TN$Postlasso.1se[k] <- values$TN
  PL.OOS.FN$Postlasso.1se[k] <- values$FN
  values <- FPR_TPR_re( (predtheory >= val) , My[-train] )
  PL.OOS$Postlasso.theory[k] <- values$ACC
  
  ### This is the CV for the Lasso estimates  
  lassomin  <- glmnet(Mx[train,],My[train], family="binomial",lambda = lassoCV$lambda.min)
  lasso1se  <- glmnet(Mx[train,],My[train], family="binomial",lambda = lassoCV$lambda.1se)
  lassoTheory <- glmnet(Mx[train,],My[train], family="binomial",lambda = lambda.theory)
  
  predlassomin <- predict(lassomin, newx=Mx[-train,], type="response")
  predlasso1se  <- predict(lasso1se, newx=Mx[-train,], type="response")
  predlassotheory <- predict(lassoTheory, newx=Mx[-train,], type="response")
  values <- FPR_TPR_re( (predlassomin >= val) , My[-train] )
  L.OOS$Lasso.min[k] <- values$ACC
  L.OOS.TP$Lasso.min[k] <- values$TP
  L.OOS.TN$Lasso.min[k] <- values$TN
  L.OOS.FP$Lasso.min[k] <- values$FP
  L.OOS.FN$Lasso.min[k] <- values$FN
  values <- FPR_TPR_re( (predlasso1se >= val) , My[-train] )
  L.OOS$Lasso.1se[k] <- values$ACC
  L.OOS.TP$Lasso.1se[k] <- values$TP
  L.OOS.TN$Lasso.1se[k] <- values$TN
  L.OOS.FP$Lasso.1se[k] <- values$FP
  L.OOS.FN$Lasso.1se[k] <- values$FN
  values <- FPR_TPR_re( (predlassotheory >= val) , My[-train] )
  L.OOS$Lasso.theory[k] <- values$ACC
  L.OOS.TP$Lasso.theory[k] <- values$TP
  L.OOS.TN$Lasso.theory[k] <- values$TN
  L.OOS.FP$Lasso.theory[k] <- values$FP
  L.OOS.FN$Lasso.theory[k] <- values$FN
  
  
  ### fit the logistic model
  
  model.logistic <-glm(Group~., data=ratioData, subset=train,family="binomial")
  
  ### get predictions: type=response so we have probabilities
  pred.logistic    <- predict(model.logistic, newdata=ratioData[-train,], type="response")
  
  ### Logistic
  values <- FPR_TPR_re( (pred.logistic >= val) , My[-train] )
  OOS$logistic[k] <- values$ACC
  OOS.TP$logistic[k] <- values$TP
  OOS.TN$logistic[k] <- values$TN
  OOS.FP$logistic[k] <- values$FP
  OOS.FN$logistic[k] <- values$FN
  
  
  
  print(paste("Iteration",k,"of",nfold,"completed"))
}


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#plot the Accuracy performance of base, Post Lasso, Lasso, SVM models
SVM<-SVM.OOS$svm
par(mar=c(1,1,1,1))
par(mai=c(1,1,1,1))
names(OOS)[1] <-"logistic"
ACCperformance <- cbind(PL.OOS,L.OOS,SVM,OOS)

barplot(colMeans(ACCperformance), xpd=FALSE, ylim=c(.0,1), xlab="Method", ylab = "Accuracy")
m.OOS <- as.matrix(ACCperformance)

rownames(m.OOS) <- c(1:nfold)
par(mar=c(1.5,1.5,1.5,1))
par(mai=c(1.5,1.5,1.5,1))
barplot(t(as.matrix(m.OOS)), beside=TRUE, legend=TRUE, args.legend=c(x= "topright", y=0.92,bty = "n"),
        ylab= bquote( "Out of Sample Accuracy"), xlab="Fold", names.arg = c(1:10))

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#supplement FPR-TPR plot 
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Lets plot FPR and TPR
plot( c( 0, 1 ), c(0, 1), type="n", xlim=c(0,1), ylim=c(0,1), bty="n", xlab = "False positive rate", ylab="True positive rate")
lines(c(0,1),c(0,1), lty=2)
#

#
TPR = sum(OOS.TP$logistic)/(sum(OOS.TP$logistic)+sum(OOS.FN$logistic))  
FPR = sum(OOS.FP$logistic)/(sum(OOS.FP$logistic)+sum(OOS.TN$logistic))  
text( FPR, TPR, labels=c("LR"))
points( FPR , TPR )

#
TPR = sum(SVM.OOS$TP)/(sum(SVM.OOS$TP)+sum(SVM.OOS$FN))  
FPR = sum(SVM.OOS$FP)/(sum(SVM.OOS$FP)+sum(SVM.OOS$TN))  
text( FPR, TPR, labels=c("svm"))
points( FPR , TPR )

#
TPR = sum(PL.OOS.TP$Postlasso.min)/(sum(PL.OOS.TP$Postlasso.min)+sum(PL.OOS.FN$Postlasso.min))  
FPR = sum(PL.OOS.FP$Postlasso.min)/(sum(PL.OOS.FP$Postlasso.min)+sum(PL.OOS.TN$Postlasso.min))  
text( FPR, TPR, labels=c("Postlasso.min"))
points( FPR , TPR )
#
TPR = sum(PL.OOS.TP$PLasso.1se)/(sum(PL.OOS.TP$PLasso.1se)+sum(PL.OOS.FN$PLasso.1se))  
FPR = sum(PL.OOS.FP$PLasso.1se)/(sum(PL.OOS.FP$PLasso.1se)+sum(PL.OOS.TN$Postlasso.1se))  
text( FPR, TPR, labels=c("Postlasso.1se"))
points( FPR , TPR )
#
TPR = sum(PL.OOS.TP$Postlasso.theory)/(sum(PL.OOS.TP$Postlasso.theory)+sum(PL.OOS.FN$Postlasso.theory))  
FPR = sum(PL.OOS.FP$Postlasso.theory)/(sum(PL.OOS.FP$Postlasso.theory)+sum(PL.OOS.TN$Postlasso.theory))  
#text( FPR, TPR, labels=c("Postlasso.1se"))
points( FPR , TPR )
#
TPR = sum(L.OOS.TP$Lasso.min)/(sum(L.OOS.TP$Lasso.min)+sum(L.OOS.FN$Lasso.min))  
FPR = sum(L.OOS.FP$Lasso.min)/(sum(L.OOS.FP$Lasso.min)+sum(L.OOS.TN$Lasso.min))  
text( FPR, TPR, labels=c("Lasso.min"))
points( FPR , TPR )
#
TPR = sum(L.OOS.TP$Lasso.1se)/(sum(L.OOS.TP$Lasso.1se)+sum(L.OOS.FN$Lasso.1se))  
FPR = sum(L.OOS.FP$Lasso.1se)/(sum(L.OOS.FP$Lasso.1se)+sum(L.OOS.TN$Lasso.1se))  
text( FPR, TPR, labels=c("Lasso.1se"))
points( FPR , TPR )
#
TPR = sum(L.OOS.TP$Lasso.theory)/(sum(L.OOS.TP$Lasso.theory)+sum(L.OOS.FN$Lasso.theory))  
FPR = sum(L.OOS.FP$Lasso.theory)/(sum(L.OOS.FP$Lasso.theory)+sum(L.OOS.TN$Lasso.theory))  
text( FPR, TPR, labels=c("Lasso.1se.theory"))
points( FPR , TPR )

