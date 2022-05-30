## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----fig5.1-2, fig.width=8----------------------------------------------------
sigma=1.2
n=20
nDegree=8

seed=61
set.seed(seed)

x = sort( rnorm(2*n) )
y = 3*x-3*x^2 + rnorm(2*n)*sigma
idTrain = sample(1:(2*n),n)
xTest = x[-idTrain]
yTest = y[-idTrain]
x = x[idTrain]
y = y[idTrain]
X = data.frame(x)

err=rep(NA,nDegree)
errTrain = err
whichPlot=c(1,2,4,8)
cols = colorRampPalette(c("red", "blue"))( length(whichPlot) )

par(mar = c(2,1.5,1,1),mgp=c(1.75,0.75,0),mfrow=c(1,length(whichPlot)),las=1,oma=c(1,1,1,1))
dgreen=rgb(0,0.75,0,names="dgreen")
xpred = seq(min(x),max(x),length=500)
for(iDegree in 1:nDegree)
{
  fti=lm(y~poly(x,degree=iDegree,raw=TRUE),data=X)
  err[iDegree] = mean( (yTest - predict(fti,newdata=data.frame(x=xTest)))^2 ) 
  errTrain[iDegree] = mean(fti$residuals^2)
  if(iDegree %in% whichPlot)
  {
    plot(y~x,xaxt="n",yaxt="n",ylab="",xlab="")
    if(iDegree==1)
      mtext("y",2,line=1.5)
    mtext("x",1,line=1.5)
    axis(1,at=-2:1,labels=1:4,cex.axis=0.75)
    axis(2,at=5*(-3:0),labels=1:4,cex.axis=0.75)
    points(yTest~xTest,col=dgreen,pch="*")
    lines(xpred,predict(fti,newdata=data.frame(x=xpred)),col=cols[which(whichPlot==iDegree)])
    mtext(paste("Degree =",iDegree),3,line=0.5)
  }
}
legend("bottomright",c("Training data","Test data"),pch=c("o","*"),col=c("black",dgreen))
print(err)


par(mar = c(2.75,2.75,1,1),mgp=c(1.75,0.75,0),mfrow=c(1,1),las=1)
plot(1:nDegree,errTrain,type="l",ylim=c(0,max(c(err,errTrain))),ylab="Mean squared error",xlab="Degree of polynomial [log scale]",xaxt="n",log="x",yaxt="n")
lines(1:nDegree,err,col=dgreen)
for(iDegree in whichPlot)
  axis(1,iDegree,col.ticks=cols[which(whichPlot==iDegree)],col.axis=cols[which(whichPlot==iDegree)])
axis(2,c(0,2,4,6))
legend("topright",c("Training data","Test data"),lty=1,col=c("black",dgreen))

## ----box51--------------------------------------------------------------------
library(ecostats)
data(globalPlants)
n = dim(globalPlants)[1]
indTrain = sample(n,n^0.75) #select a training sample of size n^0.75:
datTrain = globalPlants[indTrain,]
datTest = globalPlants[-indTrain,]
ft_r = lm(log(height)~rain,dat=datTrain)
ft_rs = lm(log(height)~rain+rain.seas,dat=datTrain)
pr_r = predict(ft_r,newdata=datTest)
pr_rs = predict(ft_rs,newdata=datTest)
rss_r = mean( (log(datTest$height)-pr_r)^2 )
rss_rs = mean( (log(datTest$height)-pr_rs)^2 )
print( c(rss_r,rss_rs) )

## ----box52, fig.height=6, fig.width=8-----------------------------------------
library(DAAG)
ft_r = lm(log(height)~rain,dat=globalPlants)
ft_rs = lm(log(height)~rain+rain.seas,dat=globalPlants)
cv_r = cv.lm(data=globalPlants, ft_r, m=5, printit=FALSE) # 5 fold CV
cv_rs = cv.lm(data=globalPlants, ft_rs, m=5, printit=FALSE) # 5 fold CV
print( c( attr(cv_r,"ms"),attr(cv_rs,"ms") ), digits=6 )

cv_r = cv.lm(data=globalPlants, ft_r, m=5, printit=FALSE,seed=1) # 5 fold CV
cv_rs = cv.lm(data=globalPlants, ft_rs, m=5, printit=FALSE,seed=1) # 5 fold CV
print( c( attr(cv_r,"ms"),attr(cv_rs,"ms") ), digits=6 )

cv_r = cv.lm(data=globalPlants, ft_r, m=5, printit=FALSE,seed=2) # 5 fold CV
cv_rs = cv.lm(data=globalPlants, ft_rs, m=5, printit=FALSE,seed=2) # 5 fold CV
print( c( attr(cv_r,"ms"),attr(cv_rs,"ms") ), digits=6 )

cv_r = cv.lm(data=globalPlants, ft_r, m=5, printit=FALSE,seed=3) # 5 fold CV
cv_rs = cv.lm(data=globalPlants, ft_rs, m=5, printit=FALSE,seed=3) # 5 fold CV
print( c( attr(cv_r,"ms"),attr(cv_rs,"ms") ), digits=6 )

## ----box53--------------------------------------------------------------------
ft_r = lm(log(height)~rain,dat=globalPlants)
ft_rs = lm(log(height)~rain+rain.seas,dat=globalPlants)
c( AIC(ft_r), AIC(ft_rs) )
c( BIC(ft_r), BIC(ft_rs) )

## ----box 54-------------------------------------------------------------------
library(leaps)
fit_heightallsub<-regsubsets(log(height)~temp+rain+rain.wetm+temp.seas,
  data=globalPlants,nbest=2)
cbind(summary(fit_heightallsub)$outmat,summary(fit_heightallsub)$bic)

## ----box 55-------------------------------------------------------------------
ft_clim = lm(log(height)~temp+rain+rain.wetm+temp.seas,
   data=globalPlants)
stepClim=step(ft_clim,trace=0)
stepClim$anova

ft_int = lm(log(height)~1,data=globalPlants)
stepForward <- step(ft_int,scope=formula(ft_clim),direction="forward",
    trace=0)
stepForward$anova

## ----subset sim, fig.width=8, eval=FALSE--------------------------------------
#  library(mvtnorm)
#  nSim = 100 # increase this for a more precise answer
#  p=8
#  n=32
#  
#  beta = c(1,1,rep(0,p-2))
#  pTrue=2
#  
#  rho=0.5
#  Sigma = diag(rep(1-rho,p))+rho
#  X=rmvnorm(n,sigma=Sigma)
#  eta = X %*% beta
#  
#  
#  resArray = array(0,c(3,3,nSim))
#  dimnames(resArray)[[1]]=c("AIC","MStrue","propTrue")
#  dimnames(resArray)[[2]]=c("all","for","back")
#  counter  = matrix(0,3,3)
#  
#  for(iSim in 1:nSim)
#  {
#    y = eta + rnorm(n)*2
#  
#    # construct matrix of all possible subsets
#    allSubs = matrix(NA,2^p,p)
#    for(iVar in 1:p)
#      allSubs[,iVar] = rep(c(0,1),each=2^(p-iVar),times=2^(iVar-1))
#  
#    # define vectors to stor AIC and MS
#    aics = rep(NA,2^p)
#    ms   = aics
#    isTrue=aics
#  
#    # intercept first as it will give error
#    ft0 = lm(y~1)
#    aics[1] = AIC(ft0)
#    ms[1] = mean((predict(ft0)-eta)^2)
#    isTrue[1] = FALSE
#  
#    # now get all subset results
#    for(iModel in 2:2^p)
#    {
#      ft = lm(y~X[,allSubs[iModel,]==1])
#      aics[iModel] = AIC(ft)
#      ms[iModel] = mean((predict(ft)-eta)^2)
#      isTrue[iModel] = allSubs[iModel,1]==1 & allSubs[iModel,2]==1 & sum(allSubs[iModel,])==2
#    }
#  
#    whichBest = which(aics==min(aics))[1] #if tie take the first one, probably smaller, but whatever this won't happen
#  
#    # now backward stepwise
#    ftBack = step(lm(y~.,data=data.frame(X)),trace=0,direction="backward")
#    aicBack = AIC(ftBack)
#    msBack = mean((predict(ftBack)-eta)^2)
#    trueBack = length(coef(ftBack))==3 & "X1" %in% names(coef(ftBack)) & "X2" %in% names(coef(ftBack))
#  
#    # now forward stepwise
#    scopeForm=paste0("X",1:p,collapse="+")
#    ftFor = step(lm(y~1,data=data.frame(X)),scope=paste("~",scopeForm),trace=0,direction="forward")
#    aicFor = AIC(ftFor)
#    msFor = mean((predict(ftFor)-eta)^2)
#    trueFor = length(coef(ftFor))==3 & "X1" %in% names(coef(ftFor)) & "X2" %in% names(coef(ftFor))
#  
#    counterAdd = matrix(0,3,3)
#    resArray[1,,iSim] = c(aics[whichBest],aicFor,aicBack)
#    resArray[2,,iSim] = c(ms[whichBest],msFor,msBack)
#    resArray[3,,iSim] = c(isTrue[whichBest],trueFor,trueBack)
#  
#    eps=1.e-8
#    whichAIC = which(resArray[1,,iSim]<min(resArray[1,,iSim])+eps)
#    counterAdd[1,whichAIC] = 1
#    whichMS = which(resArray[2,,iSim]<min(resArray[2,,iSim])+eps)
#    counterAdd[2,whichMS] = 1
#    counterAdd[3,] = resArray[3,,iSim]
#  
#    counter = counter + counterAdd
#  }
#  #end sim
#  
#  resMean = apply(resArray,c(1,2),mean)
#  resSD = apply(resArray,c(1,2),sd)/sqrt(nSim)
#  counter = counter/nSim
#  
#  print(resMean)
#  
#  par(mar=c(3,1,1,1),mgp=c(2,0.75,0),mfrow=c(1,2),oma=c(0,4,0,0))
#  plot(resMean[1,],1:3,ylim=c(0.5,3.5),col="blue",yaxt="n",pch=19,xlab="AIC",ylab="",xlim=range(resMean[1,]-2*resSD[1,],resMean[1,]+2*resSD[1,]))
#  for(iMethod in 1:3)
#    lines(c(resMean[1,iMethod]-resSD[1,iMethod]*2,resMean[1,iMethod]+resSD[1,iMethod]*2),c(iMethod,iMethod),col="blue")
#  axis(2,at=1:3,labels=c("All subsets","Forward","Backward"),las=1)
#  
#  plot(resMean[2,],1:3,ylim=c(0.5,3.5),col="blue",yaxt="n",pch=19,xlab=expression(paste("Mean squared error of ",hat(mu))),ylab="",xlim=range(resMean[2,]-2*resSD[2,],resMean[2,]+2*resSD[2,]))
#  for(iMethod in 1:3)
#    lines(c(resMean[2,iMethod]-resSD[2,iMethod]*2,resMean[2,iMethod]+resSD[2,iMethod]*2),c(iMethod,iMethod),col="blue")
#  axis(2,at=1:3,labels=c(" "," "," "),las=1)

## ----box 56-------------------------------------------------------------------
data(globalPlants)
library(glmnet)
X = cbind(globalPlants$temp, globalPlants$rain, globalPlants$rain.wetm,
   globalPlants$temp.seas)
ft_heightcv=cv.glmnet(X,log(globalPlants$height))
plot(ft_heightcv)
ft_lasso=glmnet(X,log(globalPlants$height),lambda=ft_heightcv$lambda.min)
ft_lasso$beta

## ----box5.7-------------------------------------------------------------------
ft_clim = lm(log(height)~temp+rain+rain.wetm+temp.seas,data=globalPlants)
ft_int = lm(log(height)~1,data=globalPlants)
stepAnova = step(ft_int, scope=formula(ft_clim), direction="forward",
                     trace=0, k=0)$anova
stepAnova$R2 = stepAnova$Deviance/deviance(ft_int)
stepAnova

## ----box5.8-------------------------------------------------------------------
stepMargin=add1(ft_int,scope=formula(ft_clim))
stepMargin$R2=stepMargin$`Sum of Sq`/deviance(ft_int)
stepMargin

leave1out=drop1(ft_clim)
leave1out$R2=leave1out$`Sum of Sq`/deviance(ft_int)
leave1out

## ----box5.9-------------------------------------------------------------------
# first create a dataset with standardised predictors:
globalPlantStand=globalPlants
whichVars=c("temp","rain","rain.wetm","temp.seas")
globalPlantStand[,whichVars]=scale(globalPlantStand[,whichVars])
# then fit the model:
ft_climStand = lm(log(height)~temp+rain+rain.wetm+temp.seas,
               data=globalPlantStand)
summary(ft_climStand)

## ----box5.10------------------------------------------------------------------
ft_onlyTemp = lm(log(height)~temp+temp.seas,data=globalPlants)
tempAn=anova(ft_int,ft_onlyTemp,ft_clim)
tempAn$R2=tempAn$`Sum of Sq`/deviance(ft_int)
tempAn

ft_onlyRain = lm(log(height)~rain+rain.wetm,data=globalPlants)
rainAn=anova(ft_int,ft_onlyRain,ft_clim)
rainAn$R2=rainAn$`Sum of Sq`/deviance(ft_int)
rainAn

## ----fig5.5, fig.width=8------------------------------------------------------
par(mgp=c(1.75,0.75,0),mar=c(2.75,0.5,0.5,0))
  
  R2sTemp = c(0,tempAn$R2[2])
  R2sRain = c(rainAn$R2[3],sum(rainAn$R2[2:3]))
  R2error=c(sum(rainAn$R2[2:3]),1)
  darkBlue=rgb(0,0,0.5,alpha=0.9)
  darkRed=rgb(0.5,0,0,alpha=0.9)
  darkYellow=rgb(0.5,0.5,0,alpha=0.9)
  plot(c(0,1),c(0,1),type="n",yaxt="n",xlab=expression(R^2),ylab="",bty="n")
  polygon(c(R2error,R2error[2:1]),c(0,0,1,1),col=rgb(1,1,0,alpha=0.25),lwd=0.25,border=darkYellow)
  polygon(c(R2sTemp,R2sTemp[2:1]),c(0,0,1,1),col=rgb(1,0,0,alpha=0.25),border=darkRed,lwd=1.5)
  polygon(c(R2sRain,R2sRain[2:1]),c(0,0,1,1),col=rgb(0,0,1,alpha=0.25),border=darkBlue,lwd=1.5)
  yText=0.15
  eps=0.008
  text(mean(R2error),0.5,'Unexplained',adj=0.5,col=darkYellow)
  text(eps,yText,'Temperature',adj=0,col=darkRed)
  text(R2sRain[2]-eps,1-yText,'Rainfall',adj=1,col=darkBlue)

## ----headbobs-----------------------------------------------------------------
data(headbobLizards)
str(headbobLizards)

# try some AIC funny-business
library(MASS)
plot(Hbspd_max~TemperatureC,data=headbobLizards)
plot(Hbspd_max~Bg_noise_max,data=headbobLizards)

# my vote is log-transformation, covers almost a factor of 3 and a bit of right-skew:
plot(Hbspd_max~TemperatureC,data=headbobLizards,log="y")
plot(Hbspd_max~Bg_noise_max,data=headbobLizards,log="y")

headbobLizards$bobspeed=log(headbobLizards$Hbspd_max)
ft_headbob=lm(bobspeed~TemperatureC+AmbientLight+Bg_noise_max,data=headbobLizards)
plotenvelope(ft_headbob)
stepAIC(ft_headbob)
summary(ft_headbob)

ft_2=lm(bobspeed~TemperatureC+AmbientLight,data=headbobLizards)
summary(ft_2)

ft_int=lm(bobspeed~1,data=headbobLizards)
BIC(ft_int,ft_2,ft_headbob)

## ----bob import---------------------------------------------------------------
stepMargin=add1(ft_int,scope=formula(ft_headbob))
stepMargin$R2=stepMargin$`Sum of Sq`/deviance(ft_int)
stepMargin

leave1out=drop1(ft_headbob)
leave1out$R2=leave1out$`Sum of Sq`/deviance(ft_int)
leave1out

## ----plant precip-------------------------------------------------------------
data(globalPlants)
str(globalPlants)
globalPlants$logHt = log(globalPlants$height)
ft_temp = lm(logHt~temp, data=globalPlants)
ft_tempRain = lm(logHt~temp+rain+rain.wetm+rain.drym+rain.seas+rain.wetqr+rain.dryqr+rain.warmqr+rain.coldqr, data=globalPlants)
plotenvelope(ft_tempRain)
ft_step = step(ft_temp, scope=formula(ft_tempRain), direction="forward")

## ----lasso rain---------------------------------------------------------------
XtempRain=with(globalPlants,cbind(temp,rain,rain.wetm,rain.drym,rain.seas,rain.wetqr,rain.dryqr,rain.wetqr,rain.dryqr))
library(glmnet)
lasso_rain=cv.glmnet(XtempRain,globalPlants$logHt,penalty.factor=c(0,rep(1,8)))
plot(lasso_rain)
ft_lasso=glmnet(XtempRain,globalPlants$logHt,lambda=lasso_rain$lambda.min)
ft_lasso$beta

## ----collinear----------------------------------------------------------------
library(car)
vif(ft_tempRain)
cor(XtempRain)

## ----less rain----------------------------------------------------------------
ft_tempLessRain = lm(logHt~temp+rain+rain.seas+rain.wetqr+rain.dryqr+rain.warmqr+rain.coldqr, data=globalPlants)
ft_Lstep = step(ft_temp, scope=formula(ft_tempLessRain), direction="forward")

XlessRain=model.matrix(ft_tempLessRain)[,-1]
lasso_Lrain=cv.glmnet(XlessRain,globalPlants$logHt,penalty.factor=c(0,rep(1,6)))
plot(lasso_Lrain)
ft_Llasso=glmnet(XlessRain,globalPlants$logHt,lambda=lasso_Lrain$lambda.min)
ft_Llasso$beta

