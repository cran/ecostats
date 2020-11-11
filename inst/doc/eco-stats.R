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

## ---- echo=TRUE, results='asis'-----------------------------------------------
 data(aphids)
 cols=c(rgb(1,0,0,alpha=0.5),rgb(0,0,1,alpha=0.5)) #transparent colours
 with(aphids$oat, interaction.plot(Time,Plot,logcount,legend=FALSE,
                                col=cols[Treatment], lty=1, ylab="Counts [log(y+1) scale]",
                                xlab="Time (days since treatment)") )
 legend("bottomleft",c("Excluded","Present"),col=cols,lty=1)

