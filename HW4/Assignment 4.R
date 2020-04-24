Q2
library(MASS)
Cars93

## (a)
xf = Cars93[,c(5,7,8,12,13,14,15,17,19,20,21,22,25)]
## gr = Man.trans.avai

## (b)
## (c)
## (d)
## (e)
## (f)

Q3
library(MASS)
crabs

## (a) By combining sp and sex we can identify 4 classes of crab in total
       ## create a class variable gr which does this


## (b)

## Can't figure out this part by myself
## Ch 8a. cluster centers
my.pch = rep(c(1,2,3),each=50)
pairs(iris[,1:4],col=my.pch)
fit = kmeans(iris[,1:4],centers=nc,nstart=100)
x = rbind(iris[,1:4],fit$centers)
colv=c(rep(1,150),rep(2,nc))
pchv=c(rep(3,150),rep(19,nc))
pairs(x,col=colv,pch=pchv)
## (c)


## Ch 8a. cluster centers
my.pch = rep(c(1,2,3),each=50)
pairs(iris[,1:4],col=my.pch)
fit = kmeans(iris[,1:4],centers=nc,nstart=100)
x = rbind(iris[,1:4],fit$centers)
colv=c(rep(1,150),rep(2,nc))
pchv=c(rep(3,150),rep(19,nc))
pairs(x,col=colv,pch=pchv)
## (d)
## (e)

some.function(crabs[,4:8],4,gr)
some.function(prc$x,4,gr)
some.function(prc$x[,2:5],4,gr)


## (f)
## R^2 depends on SSE and SSR but because Principal Components are a distance preserving rotation, the distances in SSE and SSR are the same, thus R^2 is the same.
## (g)
## K-means clustering also depends on distances too


