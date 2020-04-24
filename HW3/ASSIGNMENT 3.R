## Question 2

## a 
library(MASS)
plot(log(Animals$body), log(Animals$brain),type = "n")
text(log(Animals$body), log(Animals$brain), rownames(Animals))

## b
fit1 = lm(log(Animals$brain)~log(Animals$body))
summary(fit1)
abline(fit1)

##take out 3 outliers
Animals = subset(Animals,!(rownames(Animals)%in%c("Dipliodocus", "Triceratops", "Brachiosaurus")))
fit2 = lm(log(Animals$brain)~log(Animals1$body))
summary(fit2)
abline(fit2, col=2)

## c 
names(fit2$residuals) = rownames(Animals1)
sort(exp(fit2$residuals))

## Question 3

## a 
library(MASS)
biopsy1 = na.omit(biopsy)
class - 1*(biopsy1$class == "malignant")
features = biopsy1[ ,c(2:10)]

## b
object = prcomp(features, center = TURE, scale. = FALSE)

## c
pairs(object$x, col = 1+class)
plot(object)


## h
## The plots are centered around the malignant values and thus, if score at 10 somewhere it is more likely to have malignant tumors
## Features 1 and 6 fall a little below wich suggests that it is helpful in determining benign patients.

