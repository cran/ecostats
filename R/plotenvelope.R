#' Diagnostic Plots for a Fitted Object with Simulation Envelopes
#'
#' Produces diagnostic plots of a fitted model \code{y}, and
#' adds global envelopes constructed by simulating new residuals, to see how
#' departures from expected trends compare to what might be expected if the 
#' fitted model were correct. Global envelopes are constructed using the
#' \code{GET} package (Myllymäki et al 2017) for simultaneous control of error rates over the
#' whole plot, by simulating new responses from the fitted model then recomputing residuals
#' (which can be computationally intensive), or alternatively, by simulating residuals directly from the (multivariate) normal distribution
#' (faster, but not always a smart move). Options for diagnostic plots to construct are a residual vs fits,
#' a normal quantile plot, or scale-location plot, along the lines of \code{\link{plot.lm}}.
#'
#' @param y is \emph{any} object that responds to \code{residuals}, \code{predict} and 
#' (if \code{sim.method="refit"}) \code{simulate} and \code{update}.
#' @param which a vector specifying the diagnostic plots to construct: \enumerate{
#' \item{residual vs fits, with a smoother}
#' \item{normal quantile plot}
#' \item{scale-location plot, with smoother}
#' }
#' These are the first three options in \code{\link{plot.lm}} and a little is borrowed 
#' from that code. A global envelope is included with each plot around where we expect to see
#' the data (or the smoother, if requested, for plots 1 and 3) when model assumptions are satisfied.
#' If not fully captured by the global envelope, there is some evidence that the model assumptions are not satisfied.
#' @param n.sim the number of simulated sets of residuals to be generated, to which
#'  the observed residuals will be compared. The default is 199 datasets.
#' @param conf.level the confidence level to use in constructing the envelope.
#' @param type the type of global envelope to construct, see 
#'  \code{\link[GET]{global_envelope_test}} for details. Default \code{"st"} uses 
#'  studentized envelope tests to protect for unequal variance, which has performed well
#'  in simulations in this context.
#' @param overlay logical. For multivariate data, determines whether residuals from 
#' different responses are overlaid on a single plot (default), or plotted separately.
#' @param transform a character vector pointing to a function that should be applied to both
#'  axes of the normal quantile plot. The most common use is to set \code{transform="pnorm"}
#'  for a PP-plot.
#' @param sim.method How should residuals be simulated? The default for most objects is \code{"refit"}, which constructs new responses (via \code{\link{simulate}}),
#' refits the model (via \code{\link{update}}), then recomputes residuals, often known as a \emph{parametric bootstrap}.
#' This is computationally intensive but gives a robust answer. This is the only suitable option if
#' residuals are not expected to be normal when assumptions are satisfied (like when using \code{\link[stats]{glm}} with a non-Gaussian family). 
#' Alternatively, \code{"norm"} simulates from a normal distribution, matches means and variances (and covariances for multivariate responses) to the observed residuals.
#' The \code{"stand.norm"} option sets means to zero and variances to one, which is appropriate when residuals
#' should be standard normal when assumptions are satisfied (as for any object fitted using the \code{mvabund}
#' package, for example). These options are faster but more approximate than \code{"refit"}, in fact \code{"stand.norm"} is 
#' used as the default for \code{\link[mvabund]{manyglm}} objects, for computational reasons.
#' @param main the plot title (if a plot is produced). A vector of three titles, one for each plot.
#'  If only one value is given that will be used for all plots.
#' @param xlab \code{x} axis label (if a plot is produced). A vector of three labels, one for each plot.
#'  If only one value is given that will be used for all plots.
#' @param ylab \code{y} axis label (if a plot is produced). A vector of three labels, one for each plot.
#'  If only one value is given that will be used for all plots.
#' @param col color of points
#' @param line.col a vector of length three containing the colors of the lines on the three diagnostic plots.
#' Defaults to "slateblue4" for smoothers and to "olivedrab" otherwise. Because it's cool.
#' @param envelope.col color of the global envelope around the expected trend. All data points should always stay within this envelope
#' (and will for a proportion \code{conf.level} of datasets satisfying model assumptions). If a smoother has been used on
#' the residual vs fits or scale-location plot, the envelope is constructed around the smoother, that is, the smoother should always stay
#' within the envelope.
#' @param add.smooth logical defaults to \code{TRUE}, which adds a smoother to residual vs fits and
#' scale-location plots, and computes a global envelope around the \emph{smoother} rather than the data (\code{add.smooth=FALSE}). No smoother is added to the normal quantile plot.
#' @param plot.it logical. Should the result be plotted? If not, a list of analysis outputs is returned, see \emph{Value}.
#' @param do.qq.line logical. Should a line be included on the qq plot? Default is FALSE.
#' @param resFunction the function used to compute residuals for all diagnostic plots. Defaults
#' to \code{\link{cresiduals}} for multivariate linear models, \code{\link{rstandard}} for linear
#' models, or \code{\link{residuals}} in other cases.
#' @param predFunction the function used to compute predicted values in residual vs fits or
#' scale-location plots. Defaults to \code{\link{cpredict}} for multivariate linear models
#' and to \code{\link{cpredict}} otherwise.
#' @param fitMin the minimum fitted value to use in plots, any fitted value less than this will be truncated to
#' \code{fitMin}. This is useful for generalised linear models, where small fitted values correspond to
#' predictions that are numerically zero. The default is to set \code{fitMin} to \code{-6} for GLMs, otherwise no truncation (\code{-Inf}).
#' @param ... further arguments sent through to \code{plot}.
#' 
#' @details
#' A challenge when interpreting diagnostic plots is understanding the extent to which
#' deviations from the expected pattern could be due to random noise (sampling variation)
#' rather than actual assumption violations. This function is intended to assess this, 
#' by simulating multiple realizations of residuals (and fitted values) in situations where 
#' assumptions are satisfied, and plotting a global simulation envelope around these at level \code{conf.level}.
#' 
#' This function can take any fitted model, and construct any of three diagnostic plots, as determined by \code{which}:
#' \enumerate{
#' \item Residual vs fits plot (optionally, with a smoother)
#' \item Normal quantile plot
#' \item Scale-Location plot (optionally, with smoother)
#' }
#' and see if the trend is behaving as expected if the model were true. As long as 
#' the fitted model responds to \code{\link{residuals}} and \code{\link{predict}} 
#' (and when \code{sim.method="refit"}, \code{\link{simulate}} and \code{\link{update}}) then a simulation envelope
#' will be constructed for each plot.
#' 
#' Simulation envelopes are global, constructed using the \code{\link[GET]{GET-package}}, meaning that
#' (for example) a 95\% global envelope on a quantile plot should contain \emph{all} residuals for 95\% of datasets
#' that satisfy model assumptions. So if \emph{any} data points lie outside the
#' quantile plot's envelope we have evidence that assumptions of the fitted model are not satisfied. 
#' The \code{\link[GET]{GET-package}} was originally constructed to place envelopes around functions, motivated by
#' the problem of diagnostic testing of spatial processes (Myllymäki et al 2017), but it can equally
#' well be applied here, by treating the set of residuals (ordered according to the x-axis) as point-wise evaluations of a function.
#' For residual vs fits and scale-location plots, if \code{do.smooth=TRUE}, global envelopes are constructed for
#' the \emph{smoother}, not for the data, hence we are looking to see if the smoother
#' is wholly contained within the envelope. The smoother is constructed using \code{\link[mgcv]{gam}} from the \code{mgcv} 
#' package with maximum likelihood estimation (\code{method="ML"}).
#' 
#' Note that the global envelopes only tell you if there is evidence of violation of model
#' assumptions -- they do not tell you whether the violations are large enough to worry about. For example, in linear models,
#' violations of normality are usually much less important than violations of linearity or equal variance. And in all cases,
#' modest violations tend to only have modest effects on the validity of inferences, so you need to think about how big
#' observed violations are rather than just thinking about whether or not anything is outside its simulation envelope.
#' 
#' The method used to simulate data for the global envelopes is controlled by \code{sim.method}.
#' The default method (\code{sim.method="refit"}) uses a \emph{parametric bootstrap} approach: simulate 
#' new responses from the fitted model, refit the model and recompute residuals and fitted values. 
#' This directly assesses whether trends in observed trends depart from what would be expected if the fitted model
#' were correct, without any further assumptions. For complex models or large datasets this would however be super-slow.
#' A fast, more approximate alternative (\code{sim.method="norm"}) is to simulate new (multivariate) normal residuals 
#' repeatedly and use these to assess whether trends in the observed data depart from what would be expected
#' for independent (multivariate) normal residuals. If residuals are expected to be standard
#' normal, a more refined check is to simulate from the standard normal using (\code{sim.method="stand.norm"}).
#' This might for example be useful when diagnosing a model fitted using the \code{mvabund} package (Wang et al. 2012),
#' since this calculates Dunn-Smyth residuals (Dunn & Smyth 1996), which are approximately standard normal when assumptions are satisfied.  
#' If \code{y} is a \code{glm} with non-Gaussian family then residuals will not be normal and \code{"refit"} is the
#' only appropriate option, hence other choices will be overridden with a warning reporting that this
#' has been done. 
#' 
#' Note that for Multivariate Linear Models (\code{mlm}), \code{\link{cresiduals}} and \code{\link{cpredict}} 
#' are used to construct residuals and fitted values (respectively) from the \emph{full conditional models}
#' (that is, models constructed by regressing each response against all other responses
#' together with predictors). This is done because full conditionals are diagnostic of joint 
#' distributions, so \emph{any} violation of multivariate normality is expressed as a violation of 
#' linear model assumptions on full conditionals. Results for all responses are overlaid on a single plot,
#' future versions of this function may have an overlay option to separately plot each response.
#' 
#' The simulated data and subsequent analysis are also used to obtain a \emph{P}-value 
#' for the test that model assumptions are correct, for each plot. This tests if sample residuals or their smoothers
#' are unusually far from the values expected of them if model assumptions were satisfied. For details see
#' \code{\link[GET]{global_envelope_test}}.
#' 
#' @return Up to three diagnostic plots with simulation envelopes are returned, and additionally a list of 
#' three objects used in plotting, for plots 1-3 respectively. Each is a list with five components:\describe{
#' \item{\code{x}}{\emph{X}-values used for the envelope. In plots 1 and 3 this is the fitted values, or if \code{add.smooth=TRUE}, this is 500 equally spaced points covering
#' the range of fitted values. For plot 2, this is sorted normal quantiles corresponding to observed data.}
#' \item{\code{y}}{The \emph{Y}-values used for the envelope. In plots 1 and 3 this is the residuals, or with \code{add.smooth=TRUE}, this is the values of the smoother corresponding to \code{x}. For plot 2,
#' this is the sorted residuals.}
#' \item{\code{lo}}{The lower bound on the global envelope, for each value of \code{x}.}
#' \item{\code{hi}}{The upper bound on the global envelope, for each value of \code{x}.}
#' \item{\code{p.value}}{A \emph{P}-value for the test that observed smoother or data are consistent with what
#' would be expected if the fitted model were correct, computed in \code{\link[GET]{global_envelope_test}}.} 
#' }
#' 
#' @author David Warton <david.warton@@unsw.edu.au>
#' @references 
#' 
#' Dunn, P. K., & Smyth, G. K. (1996), Randomized quantile residuals. J. Comp. Graphical Stat. 5, 236-244. doi:10.1080/10618600.1996.10474708
#' 
#' Myllymäki, M., Mrkvička, T., Grabarnik, P., Seijo, H. and Hahn, U. (2017), Global envelope tests for spatial processes. J. R. Stat. Soc. B, 79: 381-404. doi:10.1111/rssb.12172
#' 
#' Wang, Y. I., Naumann, U., Wright, S. T., & Warton, D. I. (2012), \code{mvabund} - an \code{R} package for model-based analysis of multivariate abundance data. Methods in Ecology and Evolution, 3, 471-474. doi:10.1111/j.2041-210X.2012.00190.x
#' 
#' Warton DI (2022) Eco-Stats - Data Analysis in Ecology, from \emph{t}-tests to multivariate abundances. Springer, ISBN 978-3-030-88442-0
#'
#' @seealso \code{\link{cpredict}}, \code{\link{cresiduals}}, \code{\link{qqenvelope}}
#' @examples
#' # fit a Poisson regression to random data:
#' y = rpois(50,lambda=1)
#' x = 1:50
#' rpois_glm = glm(y~x,family=poisson())
#' plotenvelope(rpois_glm,n.sim=59)
#' 
#' # Fit a multivariate linear model to the iris dataset:
#' data(iris)
#' Y = with(iris, cbind(Sepal.Length,Sepal.Width,Petal.Length,Petal.Width))
#' iris_mlm=lm(Y~Species,data=iris)
#' # check normality assumption:
#' plotenvelope(iris_mlm,n.sim=59,which=2)
#' 
#' # A few more plots, with envelopes around data not smoothers:
#' \dontrun{plotenvelope(iris_mlm, which=1:3, add.smooth=FALSE)
#' # Note minor violation on the scale/location plot.}
#' 
#' # Repeat but with smoothers and with separate plots for each response and 
#' # a multiple testing adjustment to sim envelopes:
#' \dontrun{plotenvelope(iris_mlm, which=1:3, overlay=FALSE, conf.level=1-0.05/4)}
#' 
#' @importFrom grDevices adjustcolor 
#' @importFrom methods .hasSlot
#' @import graphics
#' @import stats
#' @export

plotenvelope = function (y, which = 1:2, sim.method="refit", 
                       n.sim=199, conf.level=0.95, type="st", overlay=TRUE, transform = NULL, 
                       main = c("Residuals vs Fitted Values", "Normal Quantile Plot", "Scale-Location Plot"), xlab = c("Fitted values", "Theoretical Quantiles", "Fitted Values"),
                       ylab = c("Residuals", "Residuals", expression(sqrt("|Residuals|"))), col=NULL, 
                       line.col=if(add.smooth) c("slateblue4","olivedrab","slateblue4") else rep("olivedrab",3), 
                       envelope.col = adjustcolor(line.col, 0.1), add.smooth=TRUE,
                       plot.it = TRUE, do.qq.line = FALSE, resFunction=NULL, predFunction=NULL, fitMin=if(inherits(y,"glm")|inherits(y,"manyglm")) -6 else -Inf, ...) 
  {
  ### which plots to construct? And clean up arguments
  show = rep(FALSE, 6)
  show[which] = TRUE
  if(length(main)==1) main = rep(main,3)
  if(length(xlab)==1) xlab = rep(xlab,3)
  if(length(ylab)==1) ylab = rep(ylab,3)
  
  ### first work out what object we have and get observed residuals and fits ###
  
  # return error if y is data, otherwise store model as object and data as yResp
  if(is.numeric(y))
    stop("y must be an object obtained by fitting a model to data, it cannot be the dataset itself")
  else
  {
    object=y
    yResp     = model.response(model.frame(object))
  }
  
  # unless told otherwise, define residual function to be cresiduals for mlm, rstandard for lm, otherwise residuals
  if(is.null(resFunction))
  {
    resFunction = 
    if(inherits(object,"mlm")) 
        cresiduals
    else
    {
      if(all(class(object)=="lm")) 
        rstandard
      else 
        residuals
    } 
  }
  # unless told otherwise, define predict function to be cpredict for mlm, otherwise predict
  if(is.null(predFunction))
  {
    prFunction = if(inherits(object,"mlm")) 
      function(obj, xMin){ pmax(cpredict(obj), xMin) } #try using unconditional fitted values
    else 
      function(obj, xMin){ pmax(predict(obj), xMin) }
  }  
  else
    prFunction = function(obj,xMin){pmax(predFunction(obj), xMin)}
  
  y = resFunction(object)
  x = prFunction(object, xMin=fitMin)
  
  y[y==Inf] = 2*max(y[y<Inf]) #deal with any probs with infinite resids
  
  # set defaults for sim.method ("stand.norm" for manyglm, otherwise "refit")
  if(all(sim.method==c("refit","norm","stand.norm"))) 
    sim.method = if(inherits(object,"manyglm")) "stand.norm" else "refit"
  
  # if it's a non-Gaussian GLM, change sim.method to "refit" with warning if required
  if(inherits(object,"glm")) 
  {
    if(object$family$family!="gaussian")
    {
      if(sim.method[1]!="refit")
        warning("A GLM for non-Gaussian data has been detected. Residuals will not be Gaussian when the model is correct, so sim.method has been changed to refit")
      sim.method="refit"
    }
  } 
  
  # check if it is multivariate data, if so set a flag for later and vectorise res
  if(length(dim(y))>1)
  {
    n.resp = dim(y)[2]
    n.rows = dim(y)[1]
    if(n.resp>1)
      is.mva = TRUE
    else
      is.mva = FALSE
    mu     = switch(sim.method[1], "stand.norm" = rep(0, n.resp), colMeans(y) )
    Sigma  = switch(sim.method[1], "stand.norm" = cor(y), var(y) ) 
    if(is.null(col))
    {
      col  = rep(1:n.resp,each=n.rows)
    }
    else
    if(length(col)==n.resp) # if a vector of length n.resp was given, expand to required size
      col = rep(col, each=n.rows)
  }
  else
  {
    if(is.null(col))
      col=1  
    is.mva = FALSE
    n.resp = 1
    mu     = switch(sim.method[1],"stand.norm" = 0, mean(y))
    sigma  = switch(sim.method[1],"stand.norm" = 1, sd(y))
  }
  
  ## if envelope.col or line.col is scalar, make a vector
  if(length(line.col)<3) line.col=rep(line.col,length=3)
  if(length(envelope.col)<3) envelope.col=rep(envelope.col,length=3)
  
  ### now simulate new residuals and get fitted values too ###
  n.obs=length(y)
  
  # simulate to get limits around these
  if(sim.method[1]=="refit")
  {
    # get a model frame with everything in it and update on original data.
    # This is done to set up the framework to use for simulating - all we would need to do then is change first element of mf (response).
    if( inherits(object,c("lmerMod","glmerMod")) )
    {
      mf <- match.call(call=object@call)
      if(.hasSlot(object,"data"))
        dat <- object@data
      else
        dat <- NULL
    }
    else
    {
      mf <- match.call(call=object$call)
      dat <- object$data
    }
    m <- match(c("formula", "data", "subset", 
                 "weights", "na.action", "etastart", 
                 "mustart", "offset"), names(mf), 0L)
    mf <- mf[c(1L, m)]
    #    mf$drop.unused.levels <- TRUE
    mf[[1L]] <- quote(stats::model.frame)

    
    # try to coerce to a data frame
    modelF <- try( eval(mf, parent.frame()), silent=TRUE )
    # if for some reason this didn't work (mgcv::gam objects cause grief) then just call model.frame on object:    
    # also, do this for lme4 because it is so not a team player 
    if(inherits(modelF, "try-error") | inherits(object,c("lmerMod","glmerMod","glmmTMB")))
      modelF <- model.frame(object)

    respName <- names(model.frame(object))[1]
    whichResp <- 1
    # if data object available, add stuff to modelF that is not there... hack fix for offsets that can't be found by model.frame
    if(is.null(dat)==FALSE)
      if(is.list(dat)) # only try this when a list or data frame is provided
      {  
        whichAdd = which( names(dat) %in% names(modelF) == FALSE)
        if(length(whichAdd)>0)
          for (iAdd in whichAdd)
            if(is.list(dat[[iAdd]])==FALSE) #stuff added can't be a data frame!
              modelF[[names(dat)[iAdd]]] = dat[[iAdd]]
      }
    
    # if response has brackets in its name, it is some sort of expression,
    # put quotes around it so it works (?)
    if(regexpr("(",respName,fixed=TRUE)>0)
    {
      newResp    <- sprintf("`%s`", respName) #putting quotes around response name
      fm.update  <- reformulate(".", response = newResp)
    }
    else
      fm.update  <- reformulate(".")
    
    # if there is an offset, add it, as a separate argument when updating
    offs=NULL
    modelF$offs=try(model.offset(modelF))
    if(inherits(modelF$offs,"try-error") | is.null(modelF$offs))
      objectY = update(object, formula = fm.update, data=modelF)
    else
      objectY = update(object, formula = fm.update, data=modelF,offset=offs)
    
    
    # simulate new data as a matrix
    yNew   = simulate(objectY,n.sim)
    if(is.mva) # for multivariate data, vectorise res for each sim dataset
      yNew = apply(yNew,3,cbind)
    resids = fits = matrix(NA,nrow(yNew),ncol(yNew))
    for(i.sim in 1:n.sim)
    {
      if(is.mva)
        modelF[[whichResp]] = matrix(yNew[,i.sim],ncol=n.resp,dimnames=dimnames(yResp))
      else
        modelF[[whichResp]] = yNew[,i.sim]
      if(inherits(modelF$offs,"try-error") | is.null(modelF$offs))
        newFit         = try(update(objectY, formula=fm.update, data=modelF))
      else
        newFit         = try(update(objectY, formula=fm.update, data=modelF, offset=offs))
      resids[,i.sim] = resFunction(newFit)
      ftt   = try(prFunction(newFit, xMin=fitMin)) #using try for fitted values because eel data occasionally failed(!?)
      if(inherits(ftt,"try-error"))
        fits[,i.sim] = x
      else
        fits[,i.sim] = ftt

      if(inherits(fits[,i.sim],"try-error"))
        # if no variation in preds, add v small random noise to avoid error later
        if(var(fits[,i.sim])<1.e-8) fits[,i.sim] = fits[,i.sim] + 1.e-6*rnorm(n.obs)
    }
  }
  else  
  {
    if(is.mva) # for multivariate data, generate multivariate normal and resize to long format
    {
      rnormMat    = mvtnorm::rmvnorm(n.rows*n.sim, mean=mu, sigma=Sigma)
      resids      = array(rnormMat,c(n.rows,n.sim,n.resp)) #stack as an array, which defaults to sims second, response last
      rm(rnormMat)
      resids      = aperm(resids,c(1,3,2)) #switch so response is second, sims last
      dim(resids) = c(n.rows*n.resp,n.sim) #make a matrix with sims in columns and multivariate data vectorised ("long format")
    }
    else
      resids = matrix(rnorm(n.obs*n.sim,mean=mu,sd=sigma),ncol=n.sim)
    fits = matrix(rep(x,n.sim),ncol=n.sim)
  }
  
  
  out=vector("list",3)
  
  ### plot 1 - residuals vs fits plot ###
  
  if (show[1L])
  {
    if(overlay==TRUE || is.mva==FALSE)
      out[[1]] = resFitEnvelope(x,y,fits, resids, n.obs=n.obs, conf.level=conf.level, type=type, n.sim=n.sim,
                                plot.it=plot.it, main=main, ylab=ylab, xlab=xlab, col=col,
                                line.col=line.col, envelope.col=envelope.col, 
                                add.smooth=add.smooth, nXs=length(unique(x)), nPred=500, ...)
    else
    {
      out[[1]] = vector("list",n.resp)
      names(out[[1]])=colnames(y)
      for(i.resp in 1:n.resp)
      {
        whichRows = (i.resp-1)*n.rows + 1:n.rows
        out[[1]][[i.resp]] = resFitEnvelope(x[,i.resp],y[,i.resp],fits[whichRows,], resids[whichRows,],
                                            n.obs=n.rows, conf.level=conf.level, type=type, n.sim=n.sim,
                                            plot.it=plot.it, main=paste0(main, ": ", colnames(y)[i.resp]), ylab=ylab, xlab=xlab, col=col,
                                            line.col=line.col, envelope.col=envelope.col, do.qq.line=do.qq.line,
                                            add.smooth=add.smooth, nXs = length(unique(x[,i.resp])), nPred=500, ...)  
      }
    }
  }
  
  ### plot 2 - normal quantile plot ###
  
  if (show[2L]) 
  {
    if(overlay==TRUE || is.mva==FALSE)
      out[[2]] = qqnormEnvelope(y, resids, n.obs=n.obs, transform=transform, conf.level=conf.level, type=type, sim.method=sim.method,
                                plot.it=plot.it, main=main, ylab=ylab, xlab=xlab, col=col, line.col=line.col, envelope.col=envelope.col, do.qq.line=do.qq.line, ...)
    else
    {
      out[[2]] = vector("list",n.resp)
      names(out[[2]])=colnames(y)
      for(i.resp in 1:n.resp)
      {
        whichRows = (i.resp-1)*n.rows + 1:n.rows
        out[[2]][[i.resp]] = qqnormEnvelope(y[,i.resp], resids[whichRows,], n.obs=n.rows, transform=transform, conf.level=conf.level, type=type, sim.method=sim.method,
                                            plot.it=plot.it, main=paste0(main, ": ", colnames(y)[i.resp]), ylab=ylab, xlab=xlab, col=col, 
                                            line.col=line.col, envelope.col=envelope.col, do.qq.line=do.qq.line, ...)
      }
    }
  }
  
  ### plot 3 - scale-location plot ###
  
  if (show[3L])
  {
    yAbs    = sqrt(abs(y))
    residsAbs = sqrt(abs(resids))
    if(overlay==TRUE || is.mva==FALSE)
      out[[3]] = scaleLocationEnvelope(x,yAbs,fits, residsAbs, n.obs=n.obs, conf.level=conf.level, type=type, n.sim=n.sim,
                                       plot.it=plot.it, main=main, ylab=ylab, xlab=xlab, col=col,
                                       line.col=line.col, envelope.col=envelope.col, 
                                       add.smooth=add.smooth, nXs=length(unique(x)), nPred=500, ...)
    else
    {
      out[[3]] = vector("list",n.resp)
      names(out[[3]])=colnames(y)
      for(i.resp in 1:n.resp)
      {
        whichRows = (i.resp-1)*n.rows + 1:n.rows
        out[[3]][[i.resp]] = scaleLocationEnvelope(x[,i.resp],yAbs[,i.resp],fits[whichRows,], residsAbs[whichRows,],
                                                   n.obs=n.rows, conf.level=conf.level, type=type, n.sim=n.sim,
                                                   plot.it=plot.it, main=paste0(main, ": ", colnames(y)[i.resp]), ylab=ylab, xlab=xlab, col=col,
                                                   line.col=line.col, envelope.col=envelope.col, 
                                                   add.smooth=add.smooth, nXs = length(unique(x[,i.resp])), nPred=500, ...)  
      }
    }
  }
  
  invisible(out)
}

resFitEnvelope = function(x,y,fits, resids, n.obs, conf.level=0.95, type="st",n.sim=n.sim,
                          plot.it=TRUE, main, ylab, xlab, col, line.col, envelope.col, add.smooth, 
                          nXs=nXs, nPred=500, ...)
{
  # get observed smoother
  if(add.smooth)
  {
    # for smoothers, check df is not larger than the model permits
    kStart = if(nXs<11) max(nXs-3,1) else -1 
    
    xPred = seq(min(x),max(x),length=nPred)
    # get smoother for observed data
    if(nXs>3) 
      gam.yx=mgcv::gam(c(y)~s(c(x),k=kStart),method="ML")
    if(nXs==3)
      gam.yx = lm(c(y)~c(x)+I(c(x^2)))
    if(nXs==2)  
      gam.yx=lm(c(y)~c(x))
    
    resObs=as.vector(predict(gam.yx,newdata=data.frame(x=xPred)))
    
    # get smoothers for simulated data
    residFn=matrix(NA,nPred,n.sim)
    for(i.sim in 1:n.sim)
    {
      xiSim=fits[,i.sim]
      if(var(xiSim)<1.e-8)
        xiSim = xiSim + rnorm(n.obs)*1.e-4 # to avoid an error for constant fitted value
      if(nXs>3)
        gam.yxi = mgcv::gam(resids[,i.sim]~s(xiSim,k=kStart),method="ML")
      if(nXs==3)
        gam.yxi = lm(resids[,i.sim]~xiSim+I(xiSim^2))
      if(nXs==2)
        gam.yxi = lm(resids[,i.sim]~xiSim)
      residFn[,i.sim]=predict(gam.yxi,newdata=data.frame(xiSim=xPred))
    }
  }
  else
  {
    # get observed smoother
    nPred=n.obs
    xSort = sort(x,index.return=TRUE)
    xPred=xSort$x
    resObs = y[xSort$ix]
    
    residFn=resids
    for(i.sim in 1:n.sim)
    {
      fitSort = sort(fits[,i.sim],index.return=TRUE)
      residFn[,i.sim]=resids[,i.sim][fitSort$ix]
    }
    
  }
  
  #use the Global Envelope Test package to get global envelope
  datCurves = GET::create_curve_set(list(obs=resObs, sim_m=residFn))
  cr = GET::global_envelope_test(datCurves, type=type, alpha=1-conf.level)
  
  if(plot.it==TRUE)      #do a res vs fits plot and add sim envelope
  {
    # make axes and scales as appropriate
    loPlot = cr$lo
    hiPlot = cr$hi
    if(add.smooth) #for smoother, keep ylim to data only
    {
      plot(x,y, main=main[1], 
           xlab=xlab[1], ylab=ylab[1], type="n", ...)
      eps=0.025*(max(y)-min(y)) # truncate envelope so it just goes a little bit outside the data
      loPlot[cr$lo<min(y)] = min(y)-eps
      hiPlot[cr$hi>max(y)] = max(y)+eps
    }
    else #otherwise ylim should cover envelope
    {
      plot(c(x,xPred,xPred), c(y,cr$lo,cr$hi), main=main[1], 
           xlab=xlab[1], ylab=ylab[1], type="n", ...)
    }
    # add envelope and line
    polygon(xPred[c(1:nPred,nPred:1)], c(loPlot,hiPlot[nPred:1]), 
            col=envelope.col[1], border=NA)
    # add smooth, if applicable
    if(add.smooth)
      lines(xPred,resObs,col=line.col[1])
    else
      lines(range(x),c(0,0),col=line.col[1],...)
    # add data
    points(x, y, col=col, ...)
  }
  # return a list with smoother predictions and limits
  return(list(x = xPred, y = resObs, lo=cr$lo, hi=cr$hi, p.value=attr(cr,"p")))
}

qqnormEnvelope = function(y, resids, n.obs, transform, conf.level=0.95, type="st", sim.method="refit",
                          plot.it=TRUE, main, ylab, xlab, col, line.col, envelope.col, do.qq.line=do.qq.line, ...)
{
  qq.x=qqnorm(as.vector(y),plot.it=FALSE)
  qqSort=apply(resids,2,sort)
  
  # if required, transform data
  if (is.null(transform)==FALSE)
  {
    qq.x$x = do.call(transform,list(qq.x$x))
    qq.x$y = do.call(transform,list(qq.x$y))
    qqSort = do.call(transform,list(qqSort))
    y      = do.call(transform,list(y))
  }
  
  #use the Global Envelope Test package to get global envelope
  ySort = sort(y, index.return=TRUE)
  datCurves = GET::create_curve_set(list(obs=ySort$x, sim_m=qqSort))
  cr = GET::global_envelope_test(datCurves, type=type, alpha=1-conf.level)
  
  if(plot.it==TRUE)      #do a qq plot and add sim envelope
  {
    # make qqplot
    plot(qq.x$x, qq.x$y, col=col, main=main[2], 
         xlab=xlab[2], ylab=ylab[2], type="n", ...)

    # add envelope
    #truncate limits outside of data range so polygon actually bloody plots 
    loPlot = cr$lo
    hiPlot = cr$hi
    eps=0.025*(max(y)-min(y))
    loPlot[cr$lo<min(y)] = min(y)-eps
    hiPlot[cr$hi>max(y)] = max(y)+eps
    polygon(sort(qq.x$x)[c(1:n.obs,n.obs:1)], c(loPlot,hiPlot[n.obs:1]), 
            col=envelope.col[2], border=NA)
    #        lines(range(qq.x$x),int+slope*range(qq.x$x),col=line.col[2])

    #add a qq line, if requested.
    # this is done as in qqline, but limited to range of data, unless sim.method="stand.norm"
    if(do.qq.line)
    {
      probs=c(0.25,0.75)
      qs=quantile(qq.x$y,probs)
      xs=qnorm(probs)
      if(sim.method[1]=="stand.norm")
      {
        slope = 1
        int = 0
      }
      else
      {
        slope=diff(qs)/diff(xs)
        int=qs[1]-slope*xs[1]
      }
      lines(range(qq.x$x), int+slope*range(qq.x$x), col=line.col[2])
    }

    # add data
    points(qq.x$x, qq.x$y, col=col, ...)
  }
  # return a list with qq values and limits, ordered same way as input data
  qqLo = qqHi = rep(NA,n.obs)
  qqLo[ySort$ix] = cr$lo
  qqHi[ySort$ix] = cr$hi
  
  return(list(x = qq.x$x, y = qq.x$y, lo=qqLo, hi=qqHi, p.value=attr(cr,"p")))
}



scaleLocationEnvelope = function(x,yAbs,fits, residsAbs, n.obs, conf.level=0.95, type="st", n.sim=n.sim,
                                 plot.it=TRUE, main, ylab, xlab, col, line.col, envelope.col, add.smooth, 
                                 nXs=nXs, nPred=500, ...)
{
  # get observed smoother
  if(add.smooth==TRUE)
  {
    # for smoothers, check df is not larger than the model permits
    kStart = if(nXs<11) max(nXs-3,1) else -1 
    
    xPred = seq(min(x),max(x),length=nPred)
    # get smoother for observed data
    if(nXs>3)
      gam.yx=mgcv::gam(c(yAbs)~s(c(x),k=kStart),method="ML")
    if(nXs==3)
      gam.yx=lm(c(yAbs)~c(x)+I(c(x^2)))
    if(nXs==2)
      gam.yx=lm(c(yAbs)~c(x))
    resObs=as.vector(predict(gam.yx,newdata=data.frame(x=xPred)))
    
    # get smoothers for simulated data
    residFn=matrix(NA,nPred,n.sim)
    for(i.sim in 1:n.sim)
    {
      xiSim            = fits[,i.sim]
      if(var(xiSim)<1.e-8)
        xiSim = xiSim + rnorm(n.obs)*1.e-4 # to avoid an error for constant fitted value
      if(nXs>3)
        gam.yxi        = mgcv::gam(residsAbs[,i.sim]~s(xiSim,k=kStart),method="ML")
      if(nXs==3)
        gam.yxi        = lm(residsAbs[,i.sim]~xiSim+I(xiSim^2))
      if(nXs==2)
        gam.yxi        = lm(residsAbs[,i.sim]~xiSim)
      residFn[,i.sim]  = predict(gam.yxi,newdata=data.frame(xiSim=xPred))
    }
  }
  else
  {
    # get observed smoother
    nPred=n.obs
    xSort = sort(x,index.return=TRUE)
    xPred=xSort$x
    resObs = yAbs[xSort$ix]
    
    residFn=residsAbs
    for(i.sim in 1:n.sim)
    {
      fitSort = sort(fits[,i.sim],index.return=TRUE)
      residFn[,i.sim]=residsAbs[,i.sim][fitSort$ix]
    }
  }
  
  #use the Global Envelope Test package to get global envelope
  datCurves = GET::create_curve_set(list(obs=resObs, sim_m=residFn))
  cr = GET::global_envelope_test(datCurves, type=type, alpha=1-conf.level)
  cr$lo=pmax(cr$lo,0) #ensure non-negative
  
  if(plot.it==TRUE)      #do a res vs fits plot and add sim envelope
  {
    # make axes and scales as appropriate
    loPlot = cr$lo
    hiPlot = cr$hi
    if(add.smooth==TRUE) #for smoother, keep ylim to data only
    {
      plot(x, yAbs, main=main[3], 
           xlab=xlab[3], ylab=ylab[3], type="n", ...)
      #truncate limits outside of data range so polygon actually bloody plots 
      eps=0.025*(max(yAbs)-min(yAbs))
      loPlot[cr$lo<min(yAbs)] = min(yAbs)-eps
      hiPlot[cr$hi>max(yAbs)] = max(yAbs)+eps
    }
    else #otherwise ylim should cover envelope
    {
      plot(c(x,xPred,xPred),c(yAbs,cr$lo,cr$hi), main=main[3], 
           xlab=xlab[3], ylab=ylab[3], type="n", ...)
    }
    # add envelope and line
    polygon(xPred[c(1:nPred,nPred:1)], c(loPlot,hiPlot[nPred:1]), 
            col=envelope.col[3], border=NA)
    #      lines(range(xObs),median(yAbs)*c(1,1),col=line.col,...)
    # add smooth, if applicable
    if(add.smooth==TRUE)
      lines(xPred,resObs,col=line.col[3],...)
    # add data
    points(x, yAbs, col=col, ...)
    
  }
  
  # return a list with smoother predictions and limits
  return(list(x = xPred, y = resObs, lo=cr$lo, hi=cr$hi, p.value=attr(cr,"p")))
}