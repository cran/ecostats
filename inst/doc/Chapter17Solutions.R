## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----code17.1, fig.width=7, fig.height=3.5------------------------------------
library(mvabund)
data(spider)
library(ecoCopula)
par(mfrow=c(1,2), mgp=c(1.75,0.75,0), mar=c(3,3,1,1))
spiderAbund = mvabund(spider$abund)
spider_glmInt = manyglm(spiderAbund~1,data=spider$x)
ord_spiderInt = cord(spider_glmInt)
plot(ord_spiderInt, biplot=TRUE) #for a biplot
# now plot a correlation matrix
library(gclus)
sortVars=order.single(ord_spiderInt$sigma)
library(corrplot)
corrplot(ord_spiderInt$sigma[sortVars, sortVars], type="lower", diag=FALSE, method="square")

## ----code17.2, fig.width=7, fig.height=3.5------------------------------------
par(mfrow=c(1,2), mgp=c(1.75,0.75,0), mar=c(3,3,1,1))
spider_glmX = manyglm(spiderAbund~.,data=spider$x)
ord_spiderX=cord(spider_glmX)
plot(ord_spiderX,biplot=TRUE)
corrplot(ord_spiderX$sigma[sortVars,sortVars],type="lower",diag=FALSE, method="square")
ss = c(sum(ord_spiderInt$loadings^2), sum(ord_spiderX$loadings^2))
c(ss, 1-ss[2]/ss[1])
absCor = c( sum(abs(ord_spiderInt$sigma)),
sum( abs(ord_spiderX$sigma)) ) - ncol(spider$abund)
c(absCor, 1-absCor[2]/absCor[1])

## ----ex17.2, fig.width=7, fig.height=3.5--------------------------------------
par(mfrow=c(1,2), mgp=c(1.75,0.75,0), mar=c(3,3,1,1))
spiderPA=pmin(spiderAbund,1)
spiderPA_glmInt = manyglm(spiderPA~1,data=spider$x, family="cloglog")
ord_spiderPAInt=cord(spiderPA_glmInt)
plot(ord_spiderPAInt, biplot=TRUE) #for a biplot
# now plot a correlation matrix
sortVars=order.single(ord_spiderPAInt$sigma)
corrplot(ord_spiderPAInt$sigma[sortVars, sortVars], type="lower",
diag=FALSE, method="square")

## ----code17.2variation, fig.width=7, fig.height=3.5---------------------------
par(mfrow=c(1,2), mgp=c(1.75,0.75,0), mar=c(3,3,1,1))
spiderPA_glmX = manyglm(spiderPA~.,data=spider$x, family="cloglog")
ord_spiderPAX = cord(spiderPA_glmX)
plot(ord_spiderPAX, biplot=TRUE)
corrplot(ord_spiderPAX$sigma[sortVars,sortVars], type="lower", diag=FALSE, method="square")
ss = c(sum(ord_spiderPAInt$loadings^2), sum(ord_spiderPAX$loadings^2))
c(ss, 1-ss[2]/ss[1])
absCor = c( sum(abs(ord_spiderPAInt$sigma)),
sum( abs(ord_spiderPAX$sigma)) ) - ncol(spiderPA)
c(absCor, 1-absCor[2]/absCor[1])

## ----ex17.3-------------------------------------------------------------------
library(ade4)
data(aviurba)
abund=mvabund(aviurba$fau)
library(ordinal)
ft_birdsInt=manyany(abund~1, "clm", family="ordinal", data=aviurba$mil)

## ----ex17.3cord, fig.width=7, fig.height=3.5----------------------------------
par(mfrow=c(1,2),mgp=c(1.75,0.75,0),mar=c(3,3,1,1))
ord_birdsInt=cord(ft_birdsInt)
plot(ord_birdsInt, biplot=TRUE)
sortBirdVars=order.single(ord_birdsInt$sigma)
corrplot(ord_birdsInt$sigma[sortBirdVars, sortBirdVars], type="lower", diag=FALSE, method="square")

## ----ex17.3X, fig.width=7, fig.height=3.5, warning=FALSE----------------------
par(mfrow=c(1,2), mgp=c(1.75,0.75,0), mar=c(3,3,1,1))
ft_birdsX=manyany(abund~fields, "clm", family="ordinal", data=aviurba$mil)
ord_birdsX=cord(ft_birdsX)
plot(ord_birdsX,biplot=TRUE)
corrplot(ord_birdsX$sigma[sortBirdVars,sortBirdVars],type="lower",diag=FALSE, method="square")
ss = c(sum(ord_birdsInt$loadings^2), sum(ord_birdsX$loadings^2))
c(ss, 1-ss[2]/ss[1])
absCor = c( sum(abs(ord_birdsInt$sigma)), sum( abs(ord_birdsX$sigma)) ) - ncol(abund)
c(absCor, 1-absCor[2]/absCor[1])

## ----detach ordinal-----------------------------------------------------------
detach("package:ordinal", unload=TRUE)

## ----code17.3, fig.width=7, fig.height=3.5------------------------------------
par(mfrow=c(1,2),mgp=c(1.75,0.75,0),mar=c(3,3,1,1))
graph_spiderInt = cgr(spider_glmInt)
plot(graph_spiderInt, vary.edge.lwd=TRUE)
graph_spiderX = cgr(spider_glmX, graph_spiderInt$all_graphs$lambda.opt)
plot(graph_spiderX, vary.edge.lwd=TRUE)
absCor = c( sum(abs(graph_spiderInt$best_graph$cov)),
  sum( abs(graph_spiderX$best_graph$cov)) ) - ncol(spider$abund)
c(absCor, 1-absCor[2]/absCor[1])

## ----ex17.4, fig.width=7, fig.height=3.5--------------------------------------
par(mfrow=c(1,2),mgp=c(1.75,0.75,0),mar=c(3,3,1,1))
spider_glmSoil = manyglm(spiderAbund~soil.dry,data=spider$x)
ord_spiderSoil=cord(spider_glmSoil)
corrplot(ord_spiderInt$sigma[sortVars,sortVars],type="lower",diag=FALSE, method="square")
corrplot(ord_spiderSoil$sigma[sortVars,sortVars],type="lower",diag=FALSE, method="square")
ss = c(sum(ord_spiderInt$loadings^2), sum(ord_spiderSoil$loadings^2))
c(ss, 1-ss[2]/ss[1])
absCor = c( sum(abs(ord_spiderInt$sigma)),
sum( abs(ord_spiderSoil$sigma)) ) - ncol(spider$abund)
c(absCor, 1-absCor[2]/absCor[1])

