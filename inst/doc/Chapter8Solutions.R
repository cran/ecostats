## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----box81--------------------------------------------------------------------
library(mgcv)
library(ecostats)
data(maunaloa)
maunaJan = maunaloa[maunaloa$month==1,]
ft_maunagam=gam(co2~s(year), data=maunaJan)
summary(ft_maunagam)

## ----box82, fig.width=4, fig.height=4-----------------------------------------
plotenvelope(ft_maunagam)

## ----box83 bic----------------------------------------------------------------
maunaJan$year00 = pmax(2000,maunaJan$year)
ft_maunaPiece = lm(co2~year+year00,data=maunaJan)
ft_maunagam20 = gam(co2~s(year,k=20), data=maunaJan)
summary(ft_maunagam20)$edf # this gam added about 6 extra knots:
BIC(ft_maunaPiece,ft_maunagam,ft_maunagam20)

## ----box83 validation---------------------------------------------------------
isTrain = which(maunaJan$year<=2006)
datTrain = maunaJan[isTrain,]
datTest = maunaJan[-isTrain,]
ft_piece = lm(co2~year+year00,dat=datTrain)
ft_gam = gam(co2~s(year),dat=datTrain)
ft_gam20 = gam(co2~s(year,k=20),dat=datTrain)
pr_piece = predict(ft_piece,newdata=datTest)
pr_gam = predict(ft_gam,newdata=datTest)
pr_gam20 = predict(ft_gam20,newdata=datTest)
preds = cbind( predict(ft_piece,newdata=datTest),
    predict(ft_gam,newdata=datTest), predict(ft_gam20,newdata=datTest) )
print( apply((datTest$co2-preds)^2,2,sum)) # getting SS by column

## ----box84, fig.width=5,fig.height=5------------------------------------------
data(Myrtaceae)
ft_tmprain=gam(log(richness+1)~te(TMP_MIN,RAIN_ANN),data=Myrtaceae)
vis.gam(ft_tmprain,theta=-135) #rotating the plot to find a nice view
summary(ft_tmprain)$edf

## ----box84 quad---------------------------------------------------------------
ft_tmprain2=gam(log(richness+1)~s(TMP_MIN)+s(RAIN_ANN)+TMP_MIN*RAIN_ANN,
      data=Myrtaceae)
summary(ft_tmprain2)

## ----ex83---------------------------------------------------------------------
data(globalPlants)

ft_1temprain = gam(log(height)~temp+rain, dat=globalPlants) #linear model
ft_2tempPlusrain = gam(log(height)~poly(temp,2)+poly(rain,2), dat=globalPlants) #quadratic, no interaction
ft_2temprain = gam(log(height)~poly(temp,2)+poly(rain,2)+rain:temp, dat=globalPlants) #quadratic, interactions
ft_stempPlusrain = gam(log(height)~s(temp)+s(rain), dat=globalPlants) #smoother+no interaction
ft_stemprain = gam(log(height)~s(temp)+s(rain)+rain:temp, dat=globalPlants) #smoother+interaction
BIC(ft_1temprain,ft_2tempPlusrain,ft_2temprain,ft_stempPlusrain,ft_stemprain)

## ----box85, fig.width=4, fig.height=4-----------------------------------------
data(globalPlants)
globalPlants$logHt = log(globalPlants$height)
ft_heightlm = lm(logHt~lat,dat=globalPlants)
plot(ft_heightlm,which=1)
ft_temp = gam(logHt~s(temp), dat=globalPlants)
ecostats::plotenvelope(ft_temp, which=1, main="")

## ----ex84 smooth--------------------------------------------------------------
data(Myrtaceae)
Myrtaceae$logrich=log(Myrtaceae$richness+1)
ft_richAdd = lm(logrich~soil+poly(TMP_MAX,degree=2)+
     poly(TMP_MIN,degree=2)+poly(RAIN_ANN,degree=2), data=Myrtaceae)
ft_richSmooth = gam(logrich~soil+s(TMP_MAX)+s(TMP_MIN)+s(RAIN_ANN),
                    data=Myrtaceae)
BIC(ft_richAdd,ft_richSmooth)

## ----box86, fig.width=7, fig.height=4-----------------------------------------
data(maunaloa)
library(mgcv)
ft_cyclic=gam(co2~s(DateNum)+sin(month/12*2*pi)+cos(month/12*2*pi),
  data=maunaloa)
plot(maunaloa$co2~maunaloa$Date,type="l",
  ylab=expression(CO[2]),xlab="Time")
points(predict(ft_cyclic)~maunaloa$Date,type="l",col="red",lwd=0.5)

## ----box87, fig.width=7, fig.height=3.5---------------------------------------
par(mfrow=c(1,2))
plot(residuals(ft_cyclic)~maunaloa$Date,type="l", xlab="Time")
plot(residuals(ft_cyclic)~sin(maunaloa$month/12*2*pi),
       type="l",xlab="Season")

## ----box87 sim, fig.width=7, fig.height=3.5-----------------------------------
maunaloa$simCO2 = unlist(simulate(ft_cyclic))
ft_simCyclic=gam(simCO2~s(DateNum)+sin(month/12*2*pi)+cos(month/12*2*pi),
  data=maunaloa)
par(mfrow=c(1,2))
plot(residuals(ft_simCyclic)~maunaloa$Date,type="l", xlab="Time")
plot(residuals(ft_simCyclic)~sin(maunaloa$month/12*2*pi),
       type="l",xlab="Season")

## ----box88, fig.width=7, fig.height=3.5---------------------------------------
ft_cyclic2=gam(co2~s(DateNum)+sin(month/12*2*pi)+cos(month/12*2*pi)+
       sin(month/12*4*pi)+cos(month/12*4*pi),data=maunaloa)
par(mfrow=c(1,2))
plot(residuals(ft_cyclic2)~maunaloa$Date,type="l", xlab="Time")
plot(residuals(ft_cyclic2)~sin(maunaloa$month/12*2*pi),
       type="l",xlab="Season")

## ----box89, fig.width=5, fig.height=4-----------------------------------------
ft_gamm = gamm(co2~s(DateNum)+sin(month/12*2*pi)+cos(month/12*2*pi)+
                sin(month/12*4*pi)+cos(month/12*4*pi),correlation=corAR1(),
                data=maunaloa)
acf(residuals(ft_gamm$gam))
acf(residuals(ft_gamm$lme,type="normalized"))

## ----ex86, fig.width=7, fig.height=3.5----------------------------------------
ft_cyclic3=gam(co2~s(DateNum)+sin(month/12*2*pi)+cos(month/12*2*pi)+
       sin(month/12*4*pi)+cos(month/12*4*pi) +
       + sin(month/12*6*pi)+cos(month/12*6*pi), data=maunaloa)
ft_cyclic4=gam(co2~s(DateNum)+sin(month/12*2*pi)+cos(month/12*2*pi)+
       sin(month/12*4*pi)+cos(month/12*4*pi) +
       + sin(month/12*6*pi)+cos(month/12*6*pi)+
         sin(month/12*8*pi)+cos(month/12*8*pi), data=maunaloa)
par(mfrow=c(1,2))
plot(residuals(ft_cyclic4)~maunaloa$Date,type="l", xlab="Time")
plot(residuals(ft_cyclic4)~sin(maunaloa$month/12*2*pi),
       type="l",xlab="Season")
BIC(ft_cyclic,ft_cyclic2,ft_cyclic3,ft_cyclic4)

## ----ex87, fig.width=5, fig.height=4------------------------------------------
maunaJan = maunaloa[maunaloa$month==1,]
ft_gammJan = gamm(co2~s(year),correlation=corAR1(), data=maunaJan)
acf(residuals(ft_gammJan$gam))
acf(residuals(ft_gammJan$lme,type="normalized"))
ft_gammJan$lme

## ----ex88 aspect--------------------------------------------------------------
data(Myrtaceae)
summary(Myrtaceae$aspect)

## ----ex88 period--------------------------------------------------------------
data(Myrtaceae)
Myrtaceae$logrich=log(Myrtaceae$richness+1)
ft_richAsp = lm(logrich~soil+poly(TMP_MAX,degree=2)+
     poly(TMP_MIN,degree=2)+poly(RAIN_ANN,degree=2)+
               cos(aspect*2*pi/360)+sin(aspect*2*pi/360)+cos(aspect*4*pi/360)+sin(aspect*4*pi/360),
     data=Myrtaceae)
ft_richAdd = lm(logrich~soil+poly(TMP_MAX,degree=2)+
     poly(TMP_MIN,degree=2)+poly(RAIN_ANN,degree=2), data=Myrtaceae)
BIC(ft_richAsp,ft_richAdd)

## ----ex88 spatial, eval=FALSE-------------------------------------------------
#  library(nlme)
#  ft_richNugg = gls(logrich~soil+poly(TMP_MAX,degree=2)+poly(TMP_MIN,degree=2)+
#         poly(RAIN_ANN,degree=2),data=Myrtaceae,
#           correlation=corExp(form=~X+Y,nugget=TRUE))
#  ft_richAspNugg = gls(logrich~soil+poly(TMP_MAX,degree=2)+poly(TMP_MIN,degree=2)+
#         poly(RAIN_ANN,degree=2)+ cos(aspect*2*pi/360)+sin(aspect*2*pi/360)+cos(aspect*4*pi/360)+sin(aspect*4*pi/360),data=Myrtaceae,
#           correlation=corExp(form=~X+Y,nugget=TRUE))
#  BIC(ft_richNugg)
#  BIC(ft_richAspNugg)

