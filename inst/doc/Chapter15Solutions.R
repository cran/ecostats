## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----code15.1, warning=FALSE--------------------------------------------------
library(ecostats)
data(windFarms)
set.seed(5) # use this seed to get the same results as below:
nStations = length(levels(as.factor(windFarms$X$Station)))
isTestStn = sample(nStations,nStations/2)
isTest = windFarms$X$Station %in% levels(windFarms$X$Station)[isTestStn]
library(mvabund)
windMV = mvabund(windFarms$abund)
windFt_Train=manyglm(windMV[isTest==FALSE,]~Year+Zone,
      data=windFarms$X[isTest==FALSE,],family="poisson")
windFt_Int_Train=manyglm(windMV[isTest==FALSE,]~Year*Zone,
      data=windFarms$X[isTest==FALSE,],family="poisson")
prWind_Test = predict.manyglm(windFt_Train,newdata=windFarms$X[isTest,], type="response")
prWind_Int_Test = predict.manyglm(windFt_Int_Train, newdata=windFarms$X[isTest,], type="response")
predLogL = dpois(windMV[isTest,],lambda=prWind_Test,log=TRUE)
predLogL_Int = dpois(windMV[isTest,],lambda=prWind_Int_Test,log=TRUE)
c(sum(predLogL),sum(predLogL_Int))

## ----ex15.3, warning=FALSE----------------------------------------------------
notRare=colSums(windMV>0)>10
windMVnotRare=mvabund(windFarms$abund[,notRare])

## ----ex15.3val, warning=FALSE-------------------------------------------------
windFt_TrainRare=manyglm(windMVnotRare[isTest==FALSE,]~Year+Zone,
      data=windFarms$X[isTest==FALSE,],family="poisson")
windFt_Int_TrainRare=manyglm(windMVnotRare[isTest==FALSE,]~Year*Zone,
      data=windFarms$X[isTest==FALSE,],family="poisson")
prWind_TestRare = predict.manyglm(windFt_TrainRare,newdata=windFarms$X[isTest,], type="response")
prWind_Int_TestRare = predict.manyglm(windFt_Int_TrainRare, newdata=windFarms$X[isTest,], type="response")
predLogLRare = dpois(windMVnotRare[isTest,],lambda=prWind_TestRare,log=TRUE)
predLogL_IntRare = dpois(windMVnotRare[isTest,],lambda=prWind_Int_TestRare,log=TRUE)
c(sum(predLogLRare),sum(predLogL_IntRare))

## ----code15.1R2, warning=FALSE------------------------------------------------
set.seed(1) # use this seed to get the same results as below:
nStations = length(levels(as.factor(windFarms$X$Station)))
isTestStn = sample(nStations,nStations/2)
isTest = windFarms$X$Station %in% levels(windFarms$X$Station)[isTestStn]
windFt_Train=manyglm(windMV[isTest==FALSE,]~Year+Zone,
      data=windFarms$X[isTest==FALSE,],family="poisson")
windFt_Int_Train=manyglm(windMV[isTest==FALSE,]~Year*Zone,
      data=windFarms$X[isTest==FALSE,],family="poisson")
prWind_Test = predict.manyglm(windFt_Train,newdata=windFarms$X[isTest,], type="response")
prWind_Int_Test = predict.manyglm(windFt_Int_Train, newdata=windFarms$X[isTest,], type="response")
predLogL = dpois(windMV[isTest,],lambda=prWind_Test,log=TRUE)
predLogL_Int = dpois(windMV[isTest,],lambda=prWind_Int_Test,log=TRUE)
c(sum(predLogL),sum(predLogL_Int))

windFt_TrainRare=manyglm(windMVnotRare[isTest==FALSE,]~Year+Zone,
      data=windFarms$X[isTest==FALSE,],family="poisson")
windFt_Int_TrainRare=manyglm(windMVnotRare[isTest==FALSE,]~Year*Zone,
      data=windFarms$X[isTest==FALSE,],family="poisson")
prWind_TestRare = predict.manyglm(windFt_TrainRare,newdata=windFarms$X[isTest,], type="response")
prWind_Int_TestRare = predict.manyglm(windFt_Int_TrainRare, newdata=windFarms$X[isTest,], type="response")
predLogLRare = dpois(windMVnotRare[isTest,],lambda=prWind_TestRare,log=TRUE)
predLogL_IntRare = dpois(windMVnotRare[isTest,],lambda=prWind_Int_TestRare,log=TRUE)
c(sum(predLogLRare),sum(predLogL_IntRare))

## ----code15.1R3, warning=FALSE------------------------------------------------
set.seed(2) # use this seed to get the same results as below:
nStations = length(levels(as.factor(windFarms$X$Station)))
isTestStn = sample(nStations,nStations/2)
isTest = windFarms$X$Station %in% levels(windFarms$X$Station)[isTestStn]
library(mvabund)
windMV = mvabund(windFarms$abund)
windFt_Train=manyglm(windMV[isTest==FALSE,]~Year+Zone,
      data=windFarms$X[isTest==FALSE,],family="poisson")
windFt_Int_Train=manyglm(windMV[isTest==FALSE,]~Year*Zone,
      data=windFarms$X[isTest==FALSE,],family="poisson")
prWind_Test = predict.manyglm(windFt_Train,newdata=windFarms$X[isTest,], type="response")
prWind_Int_Test = predict.manyglm(windFt_Int_Train, newdata=windFarms$X[isTest,], type="response")
predLogL = dpois(windMV[isTest,],lambda=prWind_Test,log=TRUE)
predLogL_Int = dpois(windMV[isTest,],lambda=prWind_Int_Test,log=TRUE)
c(sum(predLogL),sum(predLogL_Int))

windFt_TrainRare=manyglm(windMVnotRare[isTest==FALSE,]~Year+Zone,
      data=windFarms$X[isTest==FALSE,],family="poisson")
windFt_Int_TrainRare=manyglm(windMVnotRare[isTest==FALSE,]~Year*Zone,
      data=windFarms$X[isTest==FALSE,],family="poisson")
prWind_TestRare = predict.manyglm(windFt_TrainRare,newdata=windFarms$X[isTest,], type="response")
prWind_Int_TestRare = predict.manyglm(windFt_Int_TrainRare, newdata=windFarms$X[isTest,], type="response")
predLogLRare = dpois(windMVnotRare[isTest,],lambda=prWind_TestRare,log=TRUE)
predLogL_IntRare = dpois(windMVnotRare[isTest,],lambda=prWind_Int_TestRare,log=TRUE)
c(sum(predLogLRare),sum(predLogL_IntRare))

## ----code15.1R4, warning=FALSE------------------------------------------------
set.seed(3) # use this seed to get the same results as below:
nStations = length(levels(as.factor(windFarms$X$Station)))
isTestStn = sample(nStations,nStations/2)
isTest = windFarms$X$Station %in% levels(windFarms$X$Station)[isTestStn]
library(mvabund)
windMV = mvabund(windFarms$abund)
windFt_Train=manyglm(windMV[isTest==FALSE,]~Year+Zone,
      data=windFarms$X[isTest==FALSE,],family="poisson")
windFt_Int_Train=manyglm(windMV[isTest==FALSE,]~Year*Zone,
      data=windFarms$X[isTest==FALSE,],family="poisson")
prWind_Test = predict.manyglm(windFt_Train,newdata=windFarms$X[isTest,], type="response")
prWind_Int_Test = predict.manyglm(windFt_Int_Train, newdata=windFarms$X[isTest,], type="response")
predLogL = dpois(windMV[isTest,],lambda=prWind_Test,log=TRUE)
predLogL_Int = dpois(windMV[isTest,],lambda=prWind_Int_Test,log=TRUE)
c(sum(predLogL),sum(predLogL_Int))

windFt_TrainRare=manyglm(windMVnotRare[isTest==FALSE,]~Year+Zone,
      data=windFarms$X[isTest==FALSE,],family="poisson")
windFt_Int_TrainRare=manyglm(windMVnotRare[isTest==FALSE,]~Year*Zone,
      data=windFarms$X[isTest==FALSE,],family="poisson")
prWind_TestRare = predict.manyglm(windFt_TrainRare,newdata=windFarms$X[isTest,], type="response")
prWind_Int_TestRare = predict.manyglm(windFt_Int_TrainRare, newdata=windFarms$X[isTest,], type="response")
predLogLRare = dpois(windMVnotRare[isTest,],lambda=prWind_TestRare,log=TRUE)
predLogL_IntRare = dpois(windMVnotRare[isTest,],lambda=prWind_Int_TestRare,log=TRUE)
c(sum(predLogLRare),sum(predLogL_IntRare))

## ----code15.2-----------------------------------------------------------------
windComp = manyglm(windMV~Zone*Year,data=windFarms$X,composition=TRUE)
library(glmmTMB)
wind_glmm = glmmTMB(windMV~Year*Zone+diag(Year*Zone|cols),
family=poisson(), data=windComp$data)
summary(wind_glmm)

## ----ex15.4-------------------------------------------------------------------
isTest = windComp$data$Station %in% levels(windComp$data$Station)[isTestStn]
wind_glmm = glmmTMB(windMV~Year*Zone+diag(Year*Zone|cols),
  family=poisson(), data=windComp$data[isTest==FALSE,])
prGLMM_Test = predict(wind_glmm, newdata=windComp$data[isTest==TRUE,], type="response")
predGLMM = sum(dpois(windComp$data$windMV[isTest==TRUE],lambda=prGLMM_Test,log=TRUE))
c(predGLMM,sum(predLogL))

## ----code 15.3, fig.width=6, fig.height=4-------------------------------------
library(glmnet)
X = model.matrix(windMV~Year*Zone*cols,data=windComp$data)
y = windComp$data$windMV
windLasso = glmnet(X,y, family="poisson")
isTest = windComp$data$Station %in%
levels(windComp$data$Station)[isTestStn]
windLassoTrain = glmnet(X[isTest==FALSE,], y[isTest==FALSE], family="poisson")
prLassoTest = predict(windLassoTrain,X[isTest,],type="response")
predLLlasso=colSums(dpois(windComp$data$windMV[isTest],prLassoTest,
log=TRUE))
plot(windLassoTrain$lambda,predLLlasso,type="l",log="x")
isBestLambda = which(predLLlasso==max(predLLlasso))

## ----code 15.3beta, fig.width=6, fig.height=4---------------------------------
matplot(windLasso$lambda,t(windLasso$beta),type="n", log="x")
matlines(windLasso$lambda,t(windLasso$beta))

## ----code15.4, fig.width=6, fig.height=4--------------------------------------
library(grplasso)
windLambdaTrain = lambdamax(windMV~Year*Zone*cols, data=windComp$data,
  subset=isTest==FALSE, model = PoissReg()) * 0.7^(0:19)
windGrplasso = grplasso(windMV~Year*Zone*cols, data=windComp$data,
  lambda=windLambdaTrain, subset=isTest==FALSE, model = PoissReg())
prGrpTest = predict(windGrplasso,newdata=windComp$data[isTest,], type="response")
predLLgrplasso = colSums(dpois(windComp$data$windMV[isTest], prGrpTest,log=TRUE))
plot(windGrplasso$lambda,predLLgrplasso,log="x",type="l")
isBestLambdaGrplasso = which(predLLgrplasso==max(predLLgrplasso))

## ----code15.4beta, fig.width=6, fig.height=4----------------------------------
matplot(windGrplasso$lambda,t(windGrplasso$coefficients),type="n",log="x")
matlines(windGrplasso$lambda,t(windGrplasso$coefficients))

## ----ex15.5-------------------------------------------------------------------
c(sum(predLogL),sum(predLogL_Int), max(predLLlasso), max(predLLgrplasso))

## ----code15.5, fig.width=6, fig.height=4, warning=FALSE-----------------------
library(VGAM)
wind_RR2=rrvglm(as.matrix(windFarms$abund)~Year*Zone, family=poissonff, data=windFarms$X, Rank=2)
wind_manyglm = manyglm(windMV~Year*Zone, data=windFarms$X, family=poisson())
c( BIC(wind_RR2), sum(BIC(wind_manyglm)))
zoneyear = interaction(windFarms$X$Zone,windFarms$X$Year)
matplot(as.numeric(zoneyear),latvar(wind_RR2),pch=c(1,19))

## ----code15.6, fig.width=8, fig.height=8, warning=FALSE, eval=FALSE-----------
#  library(mvabund)
#  data(spider)
#  spid.trait = traitglm(spider$abund,spider$x,method="cv.glm1path")
#  library(lattice)
#  a = max( abs(spid.trait$fourth.corner) )
#  colort = colorRampPalette(c("blue","white","red"))
#  plot.4th = levelplot(t(as.matrix(spid.trait$fourth.corner)),
#  xlab="Environmental Variables", ylab="Species",
#  col.regions=colort(100), at=seq(-a, a, length=100),
#  scales = list( x= list(rot = 45)) )
#  print(plot.4th)

## ----code15.7, eval=FALSE-----------------------------------------------------
#  spidXY = data.frame(scale(spider$x),spider$abund) # scale standardises data!
#  library(reshape2)
#  spiderLong = melt(id=1:6,spidXY,variable.name="cols")
#  Xformula = paste(colnames(spider$x),collapse="+")
#  fullFormula = formula(paste0("value~cols+",Xformula,"+(",Xformula,"|cols)"))
#  library(glmmTMB)
#  spid_glmm = glmmTMB(fullFormula,family=nbinom2(),data=spiderLong)
#  summary(spid_glmm)

