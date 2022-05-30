## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- echo=TRUE, results='asis'-----------------------------------------------
library(ecostats)
data(iris)
Y = with(iris, cbind(Sepal.Length,Sepal.Width,Petal.Length,Petal.Width))
iris.mlm=lm(Y~Species,data=iris)
# check normality assumption:
plotenvelope(iris.mlm,n.sim=199)

## ---- echo=TRUE, results='asis'-----------------------------------------------
y=rnorm(20)
qqenvelope(y)

## ----anovaPB, echo=TRUE, results='asis'---------------------------------------
# generate random Poisson data and a predictor:
y = rpois(50,lambda=1)
x = 1:50
# fit a Poisson regressions with and without x:
rpois_glm = glm(y~x,family=poisson())
rpois_int = glm(y~1,family=poisson())
# use the parametric bootstrap to test for an effect of x (will usually be non-significant)
anovaPB(rpois_int,rpois_glm,n.sim=99)

## ---- echo=TRUE, results='asis'-----------------------------------------------
 data(aphids)
 cols=c(rgb(1,0,0,alpha=0.5),rgb(0,0,1,alpha=0.5)) #transparent colours
 with(aphids$oat, interaction.plot(Time,Plot,logcount,legend=FALSE,
                                col=cols[Treatment], lty=1, ylab="Counts [log(y+1) scale]",
                                xlab="Time (days since treatment)") )
 legend("bottomleft",c("Excluded","Present"),col=cols,lty=1)

