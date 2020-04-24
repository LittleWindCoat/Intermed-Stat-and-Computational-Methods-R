## Q1 part (d) 
## Bias
bias = function(h){
  ((1*h)^2/12)*sum((1/(1:1000))*dpois(1:1000,lambda= h*10)/(1- dpois(0, lambda=h*10)))
}
## Variance
var = function(sigma,h){
  (sigma)^2*(sum(1/(1:1000))*dpois(1:1000,lambda= h*10)/(1- dpois(0, lambda=h*10)))
} 
mse = function(sigma,h){
  bias(h)+var(sigma,h)
}
h = seq(0.001,10,0.001)

##plot
plot(h,mse(0.25,h))

## Q2

## Use data set Auto from the ISLR package, which contians MPG; 
## number of cylinders; model year; origin of car; and other information for 392 vehicles.
## Install the package
install.packages("ISLR")
library("ISLR")

## (a) Create a nre data frame containing an indicator function Y which equals 1 for model year >= 1975.
## Also retain only vehicles for which origin == 1 (only consider American cars)
Auto<-Auto[Auto$origin==1,]

hw2<-data.frame(Auto,Y=(Auto$year>=75))
## (b) Fit a logistic regression model with binary response Y and predictor variable mpg.
## Report the standard coefficient table 
## (for each B0, B1 there are columns for Estimate, Standard Error, Z-score and P-value).
summary(glm(hw2$Y~hw2$mpg,family=binomial))

## (c) 
## (i)
plot(hw2$mpg,hw2$Y,pch=3)
## (ii)
logistic = function(x) {(1+exp(-x))^(-1)}
fit0 = glm(Y ~ mpg, family='binomial',data=hw2)
mpg.range = seq(min(hw2$mpg),max(hw2$mpg),0.1)
pr = predict(fit0,newdata=list(mpg=mpg.range),se=T)
lines(mpg.range,logistic(pr$fit))
lines(mpg.range,logistic(pr$fit-2*pr$se.fit),lty=2)
lines(mpg.range,logistic(pr$fit+2*pr$se.fit),lty=2)

## (d)
boxplot(mpg~Y*cylinders,data=hw2)

## (e) 
summary(glm(hw2$Y~hw2$mpg*as.factor(hw2$cylinders),family=binomial))
glm(formula = hw2$Y ~ hw2$mpg * as.factor(hw2$cylinders), family = binomial)

## (f)
plot(hw2$mpg, hw2$Y, pch=3)
logistic = function(x) {(1+exp(-x)^(-1))}
fit0 = glm(Y ~ mpg, family = 'binomial', data=hw2)
mpg.range=seq(min(hw2$mpg), max(hw2$mpg), 0.1)
pr = predict(fit0,newdata=list(mpg=mpg.range),se=T)
lines(mpg.range, logistic(pr$fit))

mpg.col=2
mpg.type=4
mpg.range=seq(range.by.cylinder$'4'[1], range.by.cylinder$'4'[2], 0.1)
ngrid = length(mpg.range)
pr = predict(fit1, newdata=list(mpg=mpg.range,cylinders = rep(mpg,type.ngrid)), se=F)
lines(mpg.range, logistic(pr), col=mpg.col, lwd=2)

mpg.col=3
mpg.type=6
mpg.range = seq(range.by.cylinder$'6'[1], range.by.cylinder$'6'[2],0.1)
ngrid = length(mpg.range)
pr = predict(fit1, newdata=list(mpg=mpg.range, cylinders = rep(mpg.type, ngrid)),se=F)

mpg.col=4
mpg.type=8
mpg.range=seq(range.by.cylinder$'8'[1], range.by.cylinder$'8'[2],0.1)
ngrid = length(mpg.range)
pr = predict(fit1, newdata=list(mpg=mpg.range, cylinders = + rep(mpg.type, ngrid)), se=F)
lines(mpg.range, logistic(pr), col=mpg, col, lwd=2)

legend(10,0.8, col=c(1,2,3,4), legend=c("all level", "level4", "level6","level 8"), lty = c(1,1,1,1), title="plot")
fit1 = glm(Y ~ mpg*as.factor(cylinders), family = 'binomial', data=hw2)
anova(fit0, fit1)

## Q3
## (a)
library(MASS)
fgl[!(fgl$type == "Con"|fgl$type == "Tabl"),]


## (d)
## Stanardize only the RI column to sero mean and unit variance
step1 = (fgl$RI - mean(fgl$RI))/sd(fgl$RI)
RI.scale = c(1:150)/10
## part (i) lda portion
long = function (feature.matrix, class_vector, alpha) {
  class_error = rep(0,3)
  lda.fit <- lda(feature.matrix, class_vector, CV = T)
  predilda <- lda.fit$class
  class_error[1]<-1 - mean (predilda == class_vector)
  ## (ii) qda portion
  qda.fit <- qda(feature.matrix, class_vector, CV = T)
  prediqda <- qda.fit$class
  class_error[2]<-1 - mean (prediqda == class_vector)
  ##
  RI.scale = c(1:150)/10
  Ka = matrix(0, length(RI.scale), 2)
  for (j in 1:length(RI.scale)) {
    a = RI.scale[j]
    new_features <- feature.matrix
    new_features$RI <- a*(feature.matrix$RI - mean(feature.matrix$RI))/sd(feature.matrix$RI)
    ## (iii)
    K = seq (1, 25)
    ## Create an empty K
    K_error - rep(0, length(K))
    for (i in 1: length(K)) {
      knn.fit <- knn.cv(new_features, class_vector, k = K[i], prob = 1)
      K_error[i] <- 1- mean(knn.fit == class_vector)
    }
    ## take the smallest K
    k_min <- min (K[K_error == min(K_error)])
    Ka[j, ] = c(k_min, K_error[k_min]) 
  }
  ## Tell the min K
  K_min <- Ka[which (Ka[,2] == min(Ka[,2])), ]
  ## choosing the best K
  knn.fit<-knn.cn(feature.matrix, class_vector, k = K_min[1])
  class_error[3]<- 1- mean(knn.fit == class_vector)
}

pchisq(26.91,4,lower.tail = F)

## (e)
RI.scale = c(1:150)/10
Blist = NULL
Clist = NULL
B = c(1:9)
long(fgl[,1:9], fgl$type)
CE=min(long(fgl[,1:9],as.character(fgl$type)))
Blist = append(Blist,B)
CElist = append(CElist,CE)

CEarray=array()
Imin=0
while(length(B)>1){
  for(I in B){
    B1=setdiff(B,I)
    CEarray[I]=min(long(fgl[,B1],as.character(fgl$type)))
  }
  Imin=which.min(CEarray)
  B=setdiff(B,Imin)
  Blist=list(Blist,B)
  CElist=list(CElist,CEarray[Imin])
}

## (f)
confusionMatrix(CE,postive=NULL)




