##### Multiple Regression Chap 3 ISL

## Install packages
install.packages("leaps");install.packages("car");
install.packages("glmnet"); install.packages("plotmo")
install.packages("corrplot") # plotting covariance matrix
## load libraries
require(MASS);require(car)#para el vif
require(glmnet)#lasso y ridge
require(leaps)#subset selection, Cp, AIC, BIC
require(corrplot)
require(plotmo)
###### Data
data("Boston")
names(Boston)
# medv=median house value
attach(Boston)
pairs(Boston)
M=cor(Boston)
corrplot(M, method = "square")
corrplot.mixed(M) #(colormap&values)
hist(Boston$medv,br=50)
modelo1=lm(medv~.,data=Boston) #full-model
summary(modelo1)
vif(modelo1) #cutoff 5 or 10 are usually considered signs of colinearity
modelo2=update(modelo1,~.-tax)
summary(modelo2)
modelo3=update(modelo1,~.-age)
summary(modelo3)
saic=stepAIC(modelo1)
plot(saic)

######### seleccion de modelos 
stepAIC(modelo1)
fit.full=regsubsets(medv~.,data=Boston) 
summary(fit.full) #max 8 predictors , change it with nvmax=...
summary.fit.full=summary(fit.full)
names(summary.fit.full)
summary.fit.full$cp # proportional to AIC
summary.fit.full$bic
summary.fit.full$adjr2
## All criteria choose model 8

fit.full.larger=regsubsets(medv~.,data=Boston,nvmax=13) 
summary.fit.full.larger=summary(fit.full.larger)
summary.fit.full.larger$cp  #mod 11
summary.fit.full.larger$bic #mod 11
summary.fit.full.larger$adjr2 #mod 11

par(mfrow=c(2,2))
plot(summary.fit.full.larger$cp,xlab="Number of variables",ylab="Cp", type="b",col="darkblue",lwd=2)
plot(summary.fit.full.larger$bic,xlab="Number of variables",ylab="BIC", type="b",col="darkcyan",lwd=2)
plot(summary.fit.full.larger$adjr2,xlab="Number of variables",ylab="RSSadj", type="b",col="darkblue",lwd=2)

### a particular plot from regsubsets:
par(mfrow=c(1,1))
#squares indicate which variable is present
plot(fit.full.larger, scale="adjr2", col="darkcyan")
plot(fit.full.larger, scale="bic", col="maroon4")
plot(fit.full.larger, scale="Cp", col="dodgerblue2")

###### forward selection
forward=regsubsets(medv~.,data=Boston,nvmax=13,method="forward") 
summary(forward)

stepAIC(modelo1)


######### how do we choose among these models????? Cross-validation (later)

########## LASSO  lambda is chosen with CV as well...  train/validate
## in order to split the data, put it in vector Y matrix X
library(plotmo) # for plot_glmnet
set.seed(115) #to get always the same results
x=model.matrix(medv~.,data=Boston)[,-1]
y=Boston$medv
train=sample(1:nrow(x),nrow(x)/2)
test=(-train)
y.test=y[test]
lasso.mod=glmnet(x[train,], y[train],alpha=1,standardize=TRUE)  #alpha=1:lasso
plot(lasso.mod, label=TRUE,lwd=2)
print(lasso.mod)  ## nr. predictors, % deviance , Lambda

### choosing lambda with corssvalidation
cv.out =cv.glmnet (x[train ,],y[train],alpha =1)
plot(cv.out)
bestlam =cv.out$lambda.min;bestlam ## might be too conservative
## try also 
bestlam =cv.out$lambda.1se;bestlam
lasso.pred=predict (lasso.mod ,s=bestlam ,newx=x[test ,])
mean((lasso.pred -y.test)^2)

out=glmnet (x,y,alpha =1) #### with all data
lasso.who=predict(out,type="nonzero",s=bestlam);lasso.who # regressors at chosen lambda
lasso.coef=predict (out,type="coefficients",s=bestlam )[1:13,]
lasso.coef


#### other training proportion
train2=sample(1:nrow(x),2*nrow(x)/3)
test2=(-train2)
y2.test=y[test2]
cv.out =cv.glmnet (x[train2,],y[train2],alpha =1)
plot(cv.out)
bestlam =cv.out$lambda.min;bestlam
out=glmnet (x,y,alpha =1) #### with all data
lasso.coef=predict (out,type="coefficients",s=bestlam )[1:13,]
lasso.coef

############### mtcars  (far away from "big" data)
data(mtcars)
correlations=cor(mtcars)
corrplot(correlations, method = "square")
corrplot.mixed(correlations) #(colormap&values)
round(correlations,2)
modelo=lm(mpg~., data=mtcars)
summary(modelo)
anova(modelo)
x=as.matrix(mtcars[-1])
# cyl  disp  hp drat    wt  qsec vs am gear carb
y=mtcars[,1]
mod=glmnet(as.matrix(mtcars[-1]),as.matrix(mtcars[,1]),standardize=T,alpha=1)
mod=glmnet(x,y,standardize=T,alpha=1)
plot.glmnet(mod,label=T,lwd=2)
#mod=glmnet(as.matrix(x),y,standardize=T,alpha=1)
plot.glmnet(mod, label=T, lwd=2, xvar="norm")                             # default colors
plot.glmnet(mod, label=5)  # label the 5 biggest final coefs

######choosing lambda
#### not mandatory to separate train/test
train=sample(1:nrow(x),nrow(x)*0.66)
test=(-train)
y.test=y[test]
cv.out =cv.glmnet (as.matrix(x[train,]),y[train],alpha =1,nfold=5,standardize=T)
plot(cv.out) # dotted line on the left min cv-error, right error within 1 stdev from min
bestlam =cv.out$lambda.1se;bestlam

out=glmnet(x,y,alpha =1) #### with all data, no standarization
plot(out,xvar = "lambda", label = TRUE)
plot(out,xvar = "dev", label = TRUE) # -loglik
features=predict (out,type="nonzero",s=bestlam );features
coef=predict (out,type="coefficients",s=bestlam )
## choose a model with 3 variables: 5 (wt) 1(cyl) and 3 (hp)
## take a look at coef paths

lasso.coef=predict (out,type="coefficients",s=bestlam)[1:10,]
length(lasso.coef[lasso.coef!=0])
lasso.coef


########### with lars
install.packages("lars");
require(lars)
lars=lars(as.matrix(mtcars[-1]),mtcars$mpg,type="lasso")
lars
cv=lars::cv.lars(as.matrix(mtcars[-1]),mtcars$mpg,plot.it=T)

#######################  SIMMULATED DATA
#######################  p >> n
##  model: Y=Xbeta+error, beta=(1,...,1,0,....,0) number of 1s=real_p
##
set.seed(19875)
n=1000    # Number of observations
p=5000     # Number of predictors included in model

####### in this setting, small signal and big i.i.d. noise
real_p=15  # Number of true predictors
x=matrix(rnorm(n*p), nrow=n, ncol=p)
y=apply(x[,1:real_p], 1, sum) + rnorm(n)

# Split data into train and test sets
train_rows <- sample(1:n, .66*n)
x.train <- x[train_rows, ]
x.test <- x[-train_rows, ]

y.train <- y[train_rows]
y.test <- y[-train_rows]

fit.lasso=glmnet(x.train, y.train, family="gaussian", alpha=1)
fit.lasso.cv=cv.glmnet(x.train, y.train, type.measure="mse", alpha=1,family="gaussian")
plot.cv.glmnet(fit.lasso.cv)
bestlam =fit.lasso.cv$lambda.1se;bestlam

coef=coef.cv.glmnet(fit.lasso.cv)
sum(coef!=0) 

lasso.coef=predict (fit.lasso,type="coefficients",s=bestlam)#[1:5000,]
length(lasso.coef[lasso.coef!=0])

lasso.who=predict(fit.lasso,type="nonzero",s=bestlam);lasso.who

##predictions on test set
y.hat=predict(fit.lasso, s=bestlam, newx=x.test)
mse=mean((y.test - y.hat)^2)
##Lasso mse should be smaller than Ridge or elastic net in this case

fit.ridge=glmnet(x.train, y.train, family="gaussian", alpha=0)
fit.ridge.cv=cv.glmnet(x.train, y.train, type.measure="mse", alpha=0,family="gaussian")
plot.cv.glmnet(fit.ridge.cv)
bestlam.ridge =fit.ridge.cv$lambda.1se;bestlam.ridge

coef=coef.cv.glmnet(fit.ridge.cv)
sum(coef!=0) 
ridge.coef=predict (fit.ridge,type="coefficients",s=bestlam.ridge)
##predictions on test set
y.hat.ridge=predict(fit.ridge, s=bestlam.ridge, newx=x.test)
mse=mean((y.test - y.hat.ridge)^2)

######### Other Scenarios.
#big signal, big i.i.d noise 
real_p=1500
x=matrix(rnorm(n*p), nrow=n, ncol=p)
y=apply(x[,1:real_p], 1, sum) + rnorm(n)
train_rows <- sample(1:n, .66*n)
x.train <- x[train_rows, ]
x.test <- x[-train_rows, ]

y.train <- y[train_rows]
y.test <- y[-train_rows]

fit.lasso=glmnet(x.train, y.train, family="gaussian", alpha=1)
fit.lasso.cv=cv.glmnet(x.train, y.train, type.measure="mse", alpha=1,family="gaussian")
plot.cv.glmnet(fit.lasso.cv)
bestlam =fit.lasso.cv$lambda.min;bestlam

coef=coef.cv.glmnet(fit.lasso.cv)
sum(coef!=0) 

lasso.coef=predict (fit.lasso,type="coefficients",s=bestlam)#[1:5000,]
length(lasso.coef[lasso.coef!=0])

lasso.who=predict(fit.lasso,type="nonzero",s=bestlam);lasso.who

##predictions on test set
y.hat=predict(fit.lasso, s=bestlam, newx=x.test)
mse.lasso=mean((y.test - y.hat)^2)
##Lasso mse should be smaller than Ridge or elastic net in this case

fit.ridge=glmnet(x.train, y.train, family="gaussian", alpha=0)
fit.ridge.cv=cv.glmnet(x.train, y.train, type.measure="mse", alpha=0,family="gaussian")
plot.cv.glmnet(fit.ridge.cv)
bestlam.ridge =fit.ridge.cv$lambda.1se;bestlam.ridge

coef=coef.cv.glmnet(fit.ridge.cv)
sum(coef!=0) 
ridge.coef=predict (fit.ridge,type="coefficients",s=bestlam.ridge)
##predictions on test set
y.hat.ridge=predict(fit.ridge, s=bestlam.ridge, newx=x.test)
mse=mean((y.test - y.hat.ridge)^2)
#######  Ridge is slightly better

##Varying signals. High correlation between predictors
## β=(10,10,5,5,rep(1,10),rep(0,36))
##
library(MASS)
p=50
n=100
##Correlated predictors: Cov(X)ij=(0.7)|i−j|
CovMatrix <- outer(1:p, 1:p, function(x,y) {.7^abs(x-y)})
x <- mvrnorm(n, rep(0,p), CovMatrix)
y <- 10 * apply(x[, 1:2], 1, sum) + 
	5 * apply(x[, 3:4], 1, sum) +
	apply(x[, 5:14], 1, sum) +
	rnorm(n)

# Split data into train and test sets
train_rows <- sample(1:n, .66*n)
x.train <- x[train_rows, ]
x.test <- x[-train_rows, ]

y.train <- y[train_rows]
y.test <- y[-train_rows]
# Fit models:
fit.lasso <- glmnet(x.train, y.train, family="gaussian", alpha=1)
fit.ridge <- glmnet(x.train, y.train, family="gaussian", alpha=0)
fit.elnet <- glmnet(x.train, y.train, family="gaussian", alpha=.5)



fit.lasso.cv <- cv.glmnet(x.train, y.train, type.measure="mse", alpha=1, 
													family="gaussian")
fit.ridge.cv <- cv.glmnet(x.train, y.train, type.measure="mse", alpha=0,
													family="gaussian")
fit.elnet.cv <- cv.glmnet(x.train, y.train, type.measure="mse", alpha=.5,
													family="gaussian")

coef.lasso=coef.cv.glmnet(fit.lasso.cv)
sum(coef.lasso!=0)
coef.ridge=coef.cv.glmnet(fit.ridge.cv)
sum(coef.ridge!=0)
coef.elnet=coef.cv.glmnet(fit.elnet.cv)
sum(coef.elnet!=0)

bestlam.lasso =fit.lasso.cv$lambda.1se;
bestlam.ridge =fit.ridge.cv$lambda.1se;
bestlam.elnet =fit.elnet.cv$lambda.1se;
y.hat.lasso=predict(fit.lasso, s=bestlam.lasso, newx=x.test)
mse.lasso=mean((y.test - y.hat.lasso)^2);mse.lasso
y.hat.ridge=predict(fit.ridge, s=bestlam.ridge, newx=x.test)
mse.ridge=mean((y.test - y.hat.ridge)^2);mse.ridge
y.hat.elnet=predict(fit.elnet, s=bestlam.elnet, newx=x.test)
mse.elnet=mean((y.test - y.hat.elnet)^2);mse.elnet

######
fit.elnet.cv <- cv.glmnet(x.train, y.train, type.measure="mse", alpha=.03,
													family="gaussian")
plot(fit.lasso, xvar="lambda")
plot(fit.ridge, xvar="lambda")
plot(fit.elnet, xvar="lambda")
