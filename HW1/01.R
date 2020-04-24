## Q3
## We will make use of the birthwt data set from the Mass pakage
library(MASS)
birthwt
head(birthwt)
dim(birthwt)

attach(birthwt)
## Do a simple linear regression fit of response birth weight (bwt) against predictor age (age);
## Apply thr influence.measure() function to the fit
lm.bwt.age = lm(birthwt$bwt ~ birthwt$age)
obj=influence.measures(lm.bwt.age)
summary(lm.bwt.age)

## Creating the plot
## The fitted regresssion line should be superimposed on the scatter plot;
fit=bwt~age
abline(fit)
## The plot contains a scatter plot of bwt against age. Each point should be represented using a solid circle (pch=20)
## A point should be black, unless is it flagged as a high leverage point, in which case it should be red.
plot(bwt~age, pch=20, col=ifelse(obj$is.inf[,6],"red", "black"))
abline(lm.bwt.age)

## obj$is.inf[,3] will return the index of DFFITS diagnostic,mm$is.inf[,4] will return the index of cov.ratio diagnostic. 
## A point flagged by DFFITS should have a triangle (pointing up) surrounding it (pch = 2). The triangle should be blue if DFFITS < 0 and green otherwise.
obj2=obj$is.inf[,3]
points(birthwt$age[obj2],birthwt$bwt[obj2],pch=2, col=3+(obj$infmat[obj2,3]<0))
## A point flagged by the covariance ratio value should have a triangle (pointing down) surrounding it (pch = 6). The triangle should be blue if the covariance ratio is < 1 and green otherwise.
obj3=obj$is.inf[,4]
points(birthwt$age[obj3],birthwt$bwt[obj3],pch=6, col=3+(obj$infmat[obj3,4]<1))

## 3(g)
X1 = c(rep(1,11))
X2 = c(rep(1,10))
seq(0, 1, 0.1)
X1 = cbind(X1,seq(0, 1, 0.1))
X1 = solve(t(X1)%*%X1)
c(rep(0,5),rep(1,5))
X2 = cbind(X2, c(rep(0,5),rep(1,5)))
X2 = solve(t(X2)%*%X2)

X1
X2

## Q4
## (a)
## use the data set birthwt data set from the MASS package used in Q3.
## Create a subset of this data by removing all observations flagged by the DFFITTS diagnostic calculated in Q3. Use the subset() function.
sub.birthwt = subset(birthwt, !obj2) 

## (b)
## Examine 6 regression models, using bwt as a response and age and smoke as predictors.
model.list = list(
  sub.birthwt$bwt ~ sub.birthwt$age,
  sub.birthwt$bwt ~ sub.birthwt$smoke,
  sub.birthwt$bwt ~ sub.birthwt$smoke+sub.birthwt$age,
  sub.birthwt$bwt ~ sub.birthwt$smoke+sub.birthwt$smoke:sub.birthwt$age,
  sub.birthwt$bwt ~ sub.birthwt$smoke+sub.birthwt$age:I(1-sub.birthwt$smoke),
  sub.birthwt$bwt ~ sub.birthwt$smoke+sub.birthwt$age+sub.birthwt$smoke:sub.birthwt$age
)
table = NULL 

for (i in 1:6) {
  x = lm(model.list[[i]])
  x
  modelx = summary(lm(model.list[[i]]))$adj.r
  f1 = summary(lm(model.list[[i]]))$fstatistic[1]
  f2 = summary(lm(model.list[[i]]))$fstatistic[2]
  f3 = summary(lm(model.list[[i]]))$fstatistic[3]
  modelx = c(modelx, f1, f2, f3, 1-pf(f1,f2,f3))
  table = rbind (table, modelx)
}

Int1non = lm(model.list[[1]])$coefficient[1]
Slope1non = lm(model.list[[1]])$coefficient[2]
Int1smoke = lm(model.list[[1]])$coefficient[1]
Slope1smoke = lm(model.list[[1]])$coefficient[2]

Int2non =  lm(model.list[[2]])$coefficient[1]
Slope2non = lm(model.list[[2]])$coefficient[]
Int2smoke = lm(model.list[[2]])$coefficient[1]+coefficient[2]
Slope2smoke = lm(model.list[[2]])$coefficient[1]

Int3non =  lm(model.list[[3]])$coefficient[1]
Slope3non = lm(model.list[[3]])$coefficient[3]
Int3smoke = lm(model.list[[3]])$coefficient[1]+coefficient[2]
Slope3smoke = lm(model.list[[3]])$coefficient[3]
  
Int4non =  lm(model.list[[4]])$coefficient[1]
Slope4non = lm(model.list[[4]])$coefficient[]
Int4smoke = lm(model.list[[4]])$coefficient[1]+coefficient[2]
Slope4smoke = lm(model.list[[4]])$coefficient[3]

Int5non =  lm(model.list[[5]])$coefficient[1]
Slope5non = lm(model.list[[5]])$coefficient[3]
Int5smoke = lm(model.list[[5]])$coefficient[1]+coefficient[2]
Slope5smoke = lm(model.list[[5]])$coefficient[]

Int6non =  lm(model.list[[6]])$coefficient[1]
Slope6non = lm(model.list[[6]])$coefficient[3]
Int6smoke = lm(model.list[[6]])$coefficient[1]+coefficient[2]
Slope6smoke = lm(model.list[[6]])$coefficient[3]+coefficient[4]

int.non = c(Int1non, Int2non, Int3non, Int4non, Int5non, Int6non)
slope.non = c(Slope1non, Slope2non, Slope3non, Slope4non, Slope5non, Slope6non)
int.smoke = c(Int1smoke, Int2smoke, Int3smoke, Int4smoke, Int5smoke, Int6smoke)
slope.smoke = c(Slope1smoke, Slope2smoke, Slope3smoke, Slope4smoke, Slope5smoke, Slope6smoke)
  
table = cbind(int.non, table)
table = cbind(slope.non, table)
table = cbind(int.smoke, table)
table = cbind(slope.smoke, table)

par(mfrow=c(3,2))
for (i in 1:6) {
  plot(sub.birthwt$age, sub.birthwt$bwt)
  abline(table[i,1], table[i,2])
  abline(table[i,3], table[i,4])
}

anova(lm(model.list[[5]]), lm(model.list[[1]]))


