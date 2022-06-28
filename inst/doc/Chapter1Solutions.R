## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----proptest-----------------------------------------------------------------
prop.test(65,65+44)

## ----proptest graph, fig.width=5----------------------------------------------
barplot(c(65,44),names.arg=c("females","males"))

## ----ravens-------------------------------------------------------------------
library(ecostats)
data(ravens)
ravens1 = ravens[ravens$treatment==1,] #limit to just gunshot treatment
t.test(ravens1$Before,ravens1$After,paired=TRUE,alternative = "less")

## ----ravens plot, fig.width=5-------------------------------------------------
boxplot(ravens1$delta,ylab="After-Before differences")

## ----guineapigs ttest---------------------------------------------------------
t.test(errors~treatment,data=guineapig, var.equal=TRUE)

## ----guineapigs plot, fig.width=6---------------------------------------------
plot(errors~treatment,data=guineapig)

## ----Code 1.1-----------------------------------------------------------------
prop.test(65,109,0.5)
2*pbinom(64,109,0.5,lower.tail=FALSE)

## ----Code 1.2, fig.width=8,fig.height=4---------------------------------------
par(mfrow=c(1,2), mgp=c(1.75,0.75,0), mar=c(3,3,1,1))
Before = c(0, 0, 0, 0, 0, 2, 1, 0, 0, 3, 5, 0)
After = c(2, 1, 4, 1, 0, 5, 0, 1, 0, 3, 5, 2)
qqnorm(After-Before, main="")
qqline(After-Before,col="red")
library(ecostats)
qqenvelope(After-Before)

## ----Code 1.3, fig.width=4,fig.height=4---------------------------------------
# Enter the data
Before = c(0, 0, 0, 0, 0, 2, 1, 0, 0, 3, 5, 0)
After = c(2, 1, 4, 1, 0, 5, 0, 1, 0, 3, 5, 2)
# Transform the data using y_new = log(y+1):
logBefore = log(Before+1)
logAfter = log(After+1)
# Construct a normal quantile plot of the transformed data
qqenvelope(logAfter-logBefore)

## ----global plants, fig.width=4,fig.height=4----------------------------------
data(globalPlants)
hist(globalPlants$height)

## ----global plants logHt, fig.width=4,fig.height=4----------------------------
hist(log(globalPlants$height))

## ----seaweed plot, fig.width=4,fig.height=4-----------------------------------
data(seaweed)
boxplot(Total~Dist,data=seaweed)

## ----seaweed transform, fig.width=8,fig.height=4------------------------------
par(mfrow=c(1,2), mgp=c(1.75,0.75,0), mar=c(3,3,1,1))
hist(seaweed$Total)
hist(log(seaweed$Total))

