#------------------------------------------------------------------------------
#      "An R package for Birnbaum-Saunders fixed and mixed models"
#   Victor Leiva, Michelli Barros, Cristian Villegas, Gilberto A. Paula
#------------------------------------------------------------------------------

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
## Datasets
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

#------------------------------------------------------------------------------
# TTT plot
#------------------------------------------------------------------------------

TTT <- function(x,
                expLine  = "FALSE",
                yLabel   = expression(W[n](u)),
                xLabel   = "u",
                entitled = "TTT plot"){
    n           <- length(x)
    k             <- seq(from = 1, to = n, by = 1)
    SortData    <- sort(x)
    accum         <- 0
    summ          <- 0
    accum2        <- 0
    sum2          <- 0
      for(i in 1:n){
          accum     <- SortData[i] + accum
          summ[i]   <- accum
      }
      for(i in 1:n){
         sum2[i]    <- summ[i] + (n - i) * SortData[i]
      }
    NewK            <- k / n
    NewSortData <- sum2 / sum2[n]
    plot(NewK, type        = "l",
         NewSortData,
         xlab        = xLabel,
         ylab        = yLabel,
         main        = entitled,
         lwd = 2.0)
   if(expLine == "TRUE"){
        cero <- c(0, 1)
        one  <- c(0, 1)
        lines(cero, one, col = "gray", lwd = 2.0)
    }
}

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
## Functions
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

mle <- function(x, t, v, kernel = "normal", status){

 x         <- as.matrix(x)
 t         <- as.matrix(t)
 y         <- as.matrix(log(t))
 status    <- as.matrix(status)
 fit       <- lm.fit(x, y)
 beta      <- c(fit$coef)
 k         <- length(beta)
 n         <- length(y)
 n1        <- sum(status)
 mu        <- x %*% beta
 alpha     <- sqrt((4 / n1 ) * sum((sinh((y - mu) / 2)) ^ 2))
 thetaStar <- c(beta,alpha)

## Log-likelihood function
loglik <- function(z){

 z1     <- z[1:k]
 z2     <- z[k + 1]
 mu     <- (x %*% z1)
 xi1    <- (2 / z2) * cosh((y - mu) / 2)
 xi2    <- (2 / z2) * sinh((y - mu) / 2)
 g      <- switch(kernel,
                 "normal" = exp(-(xi2 ^ 2) / 2),
                 "t"      = (v + (xi2 ^ 2)) ^ (-(v + 1) / 2)
                  )

 const  <- switch(kernel,
                 "normal" = 1 / (sqrt(2 * pi)),
                 "t"      = (gamma((v + 1) / 2) * v ^ (v / 2)) / (sqrt(pi) *
                             gamma(v / 2))
                 )

 result <- switch(kernel,
                  "normal" = sum(status * log(const / 2) + status * log(xi1) +
                             status * log(g) + (1 - status) *
                             log(1 - pnorm(xi2))),
                  "t"      = sum(status * log(const / 2) + status * log(xi1) +
                             status * log(g) + (1 - status) *
                             log(1 - pt(xi2, df = v)))
                  )
 return(result)

}

## AIC-BIC
AIC = - 2 * loglik + 2 * (length(coef) + 1)

BIC = - 2 * loglik + (length(coef) + 1)  * log(length(y))

## Score function
score <- function(z){

 z1        <-z[1:k]
 z2        <-z[k+1]
 mu        <-(x %*% z1)
 xi1    <- (2 / z2) * cosh((y - mu) / 2)
 xi2    <- (2 / z2) * sinh((y - mu) / 2)
 const     <- switch(kernel,
                     "normal" = 1 / (sqrt(2 * pi)),
                     "t"      = (gamma((v + 1.0) / 2) * v ^ (v / 2)) /
                                (sqrt(pi) * gamma(v / 2))
                    )

 derivativeF <- switch(kernel,
                     "normal" = -(const) * xi2 * exp(-(xi2 ^ 2) / 2),
                     "t"      = -(const) * (v + 1) * xi2 * ((v + (xi2 ^ 2)) ^
                                (-(v + 3) /2))
                    )

 h         <- switch(kernel,
                     "normal" = dnorm(xi2) / (1 - pnorm(xi2)),
                     "t"      = dt(xi2, df = v) / (1 - pt(xi2, df = v))
                    )

 wg        <- switch(kernel,
                     "normal" = -(1 / 2),
                     "t"      = -(v + 1) / (2 * (v + (xi2 ^ 2)))
                    )

 Ubeta     <- switch(kernel,
                     "normal" = -status * ((2 / (z2 ^ 2)) * sinh(y - mu) * wg +
                                (1 / 2) * tanh((y - mu) / 2)) +
                                ((1 - status) / 2) * xi1 * h,
                     "t"      = -status * ((2 / (z2 ^ 2)) * sinh(y - mu) * wg +
                                (1 / 2) * tanh((y - mu) / 2)) +
                                ((1 - status) / 2) * xi1 * h
                     )

 Ualpha     <- switch(kernel,
                     "normal" = -sum((status / z2) *
                                (((xi2 ^ 2) * 2 * wg) + 1) -
                                ((1 - status) / z2) * h * xi2),
                     "t"      = -sum((status / z2) * (((xi2 ^ 2) * 2 * wg) + 1) -
                                ((1 - status) / z2) * h * xi2)
                     )

 result     <- c(t(x) %*% Ubeta, Ualpha)

 return(result)
}

## Parameter estimation

 est        <- optim(thetaStar, loglik, score, method = "BFGS", hessian = TRUE,
                     control = list(fnscale = -1, maxit = 2000, reltol = 1e-12))

 if(est$conv != 0)
   warning("FUNCTION DID NOT CONVERGE!")

 coef       <-(est$par)[1:k]
 alphaest   <- est$par[k+1]
 muhat      <- x %*% coef
 muhat      <- as.vector(muhat)
 etahat     <- exp(muhat)

##  Results list
 result <- list(alphaHat = alphaest,
                betaHat  = coef,
                muHat    = muhat)
 return(result)
}

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
##  observed Fisher information matrix
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

obsFisInfMatr <- function(x, t, v, kernel = "normal", status){

 x         <- as.matrix(x)
 t         <- as.matrix(t)
 y         <- as.matrix(log(t))
 fit       <- lm.fit(x, y)
 status    <- as.matrix(status)
 beta      <- c(fit$coef)
 k         <- length(beta)
 n         <- length(y)
 muhat     <- switch(kernel,
                     "normal" =  mle(x, t, v, kernel = "normal", status)$muHat,
                     "t"      =  mle(x, t, v, kernel = "t", status)$muHat
                     )

 muhat     <- as.vector(muhat)
 alphaest  <-  switch(kernel,
                      "normal" =  mle(x, t, v, kernel = "normal",
                                  status)$alphaHat,
                      "t"      =  mle(x, t, v, kernel = "t", status)$alphaHat
                      )

 const     <- switch(kernel,
                     "normal" = 1 / (sqrt(2 * pi)),
                     "t"      = (gamma((v + 1.0) / 2) * v ^ (v / 2)) /
                                (sqrt(pi) * gamma(v / 2))
                     )

 xi1hat    <- (2 / alphaest) * cosh((y - muhat) / 2)
 xi2hat    <- (2 / alphaest) * sinh((y - muhat) / 2)
 sech      <- (cosh((y - muhat)/2)) ^ (-1)
 derivativeF <- switch(kernel,
                     "normal" = -(const) * xi2hat * exp(-(xi2hat ^ 2) / 2),
                     "t"      = -(const) * (v + 1) * xi2hat *
                               ((v + (xi2hat ^ 2)) ^ (-(v + 3) /2))
                    )

 g         <- switch(kernel,
                     "normal" = exp(-(xi2hat ^ 2) / 2),
                     "t"      = (v + (xi2hat ^ 2)) ^ (-(v + 1) / 2)
                    )

 g0        <- switch(kernel,
                     "normal" = 1,
                     "t"      = v  ^ (-( v + 1) / 2)
                    )

 wg        <- switch(kernel,
                     "normal" = -(1/2),
                     "t"      = -(v + 1) / (2 * (v + (xi2hat ^ 2)))
                    )

 dwg       <- switch(kernel,
                     "normal" = 0,
                     "t"      = (v + 1) / (2 * ((v + (xi2hat ^ 2)) ^ 2))
                    )

 h         <- switch(kernel,
                     "normal" = dnorm(xi2hat) / (1 - pnorm(xi2hat)),
                     "t"      = dt(xi2hat, df = v) / (1 - pt(xi2hat, df = v))
                    )

 dh        <- switch(kernel,
                     "normal" = (derivativeF / (1 - pnorm(xi2hat))) +
                                (((dnorm(xi2hat)) ^ 2) /
                                ((1 - pnorm(xi2hat)) ^ 2)),
                     "t"      = (derivativeF / (1 - pt(xi2hat, df = v))) +
                                ((dt(xi2hat, df = v) ^ 2) /
                                ((1 - pt(xi2hat, df = v)) ^ 2))
                    )

 S         <- switch(kernel,
                     "normal" = 1-pnorm(xi2hat),
                     "t"      = (1 - pt(xi2hat, df = v))
                    )

# Lalphaalpha

 Lalpha    <- -sum((1 / (alphaest ^ 2)) * status + (6 / (alphaest ^ 2)) *
              status * (xi2hat ^ 2) * wg + (4 / (alphaest ^ 2)) * status *
              (xi2hat ^ 4) * dwg - (2 / (alphaest ^ 2)) * (1 - status) *
              xi2hat * h - (1 / (alphaest ^ 2)) * (1 - status) * (xi2hat ^ 2) *
              dh)

# Lalphabeta

 ki        <- (4 / (alphaest ^ 3)) * status * sinh(y - muhat) *
              (wg + (xi2hat ^2) * dwg) - (1 / (2 * alphaest)) * (1 - status) *
               xi1hat * h - (1 / (alphaest ^ 3)) * (1 - status) *
               sinh(y - muhat) * dh
 Lalphabeta <- -t(x) %*% ki
 Lbetaalpha <- -t(ki) %*% x

#Lbetabeta

 vi         <- (1/4) * status * (sech ^ 2) + (2 / (alphaest ^ 2)) * status *
               cosh(y - muhat) * wg + (4 / (alphaest ^ 4)) * status *
               ((sinh(y - muhat)) ^ 2) * dwg - (1 / 4) * (1 - status) *
               (xi2hat * h + (xi1hat ^ 2) * dh)
 vec        <- as.vector(vi)
 V          <- diag(vec)
 Lbeta      <- -t(x) %*% V %*% x


# Observed Fisher information matrix

 coef              <- switch(kernel,
                             "normal" =  mle(x, t, v, kernel = "normal",
                                             status)$betaHat,
                             "t"      =  mle(x, t, v, kernel = "t",
                                             status)$betaHat
                            )

 L1                <- cbind(Lbeta, Lalphabeta)
 L2                <- cbind(Lbetaalpha, Lalpha)
 observmatrix      <- rbind(L1, L2)
 invobservmatrix   <- solve(observmatrix)
 stderrors         <- sqrt(diag(invobservmatrix))[1:k]
 stderroralpha     <- sqrt(diag(invobservmatrix))[k+1]
 zstats            <- coef / stderrors
 pvalues           <- 2 * (1 - pnorm(abs(coef / stderrors)))
 result            <- list(observMatrix    = observmatrix,
                           invObservMatrix = invobservmatrix,
                           stdErrors       = stderrors,
                           stdErrorsAlpha  = stderroralpha,
                           zStats          = zstats,
                           pValue          = pvalues,
                           S               = S,
                           Lalpha          = Lalpha,
                           Lbeta           = Lbeta
                           )

 return(result)
}

##  Results list
resultList <- function(x, t, v, kernel = "normal", status){

 alphaest      <- switch(kernel,
                         "normal" =  mle(x, t, v, kernel = "normal",
                                         status)$alphaHat,
                         "t"      =  mle(x, t, v, kernel = "t", status)$alphaHat
                         )

 coef          <- switch(kernel,
                         "normal" =  mle(x, t, v, kernel = "normal",
                                         status)$betaHat,
                         "t"      =  mle(x, t, v, kernel = "t", status)$betaHat
                         )

 muhat         <- switch(kernel,
                         "normal" =  mle(x, t, v, kernel = "normal",
                                         status)$muHat,
                         "t"      =  mle(x, t, v, kernel = "t", status)$muHat
                         )

 stderrors     <- switch(kernel,
                         "normal" =  obsFisInfMatr(x, t, v, kernel = "normal",
                                                   status)$stdErrors,
                         "t"      =  obsFisInfMatr(x, t, v, kernel = "t",
                                                   status)$stdErrors
                         )

 stderroralpha <- switch(kernel,
                         "normal" =  obsFisInfMatr(x, t, v, kernel = "normal",
                                                   status)$stdErrorsAlpha,
                         "t"      =  obsFisInfMatr(x, t, v, kernel = "t",
                                                   status)$stdErrorsAlpha
                         )

 zstats        <-  switch(kernel,
                          "normal" =  obsFisInfMatr(x, t, v, kernel = "normal",
                                                    status)$zStats,
                          "t"      =  obsFisInfMatr(x, t, v, kernel = "t",
                                                    status)$zStats
                         )

 pvalues       <- switch(kernel,
                         "normal" =  obsFisInfMatr(x, t, v, kernel = "normal",
                                                   status)$pValue,
                         "t"      =  obsFisInfMatr(x, t, v, kernel = "t",
                                                   status)$pValue
                         )

 AIC          <-  switch(kernel,
                         "normal" =  mle(x, t, v, kernel = "normal",
                                         status)$AIC,
                         "t"      =  mle(x, t, v, kernel = "t", status)$AIC
                         )

 BIC         <-  switch(kernel,
                         "normal" =  mle(x, t, v, kernel = "normal",
                                         status)$BIC,
                         "t"      =  mle(x, t, v, kernel = "t", status)$BIC
                         )

 result        <- list(alphaHat       = alphaest,
                       betaHat        = coef,
                       muHat          = muhat,
                       stdErrors      = stderrors,
                       stdErrorsAlpha = stderroralpha,
                       zStats         = zstats,
                       pValue         = pvalues,
                       AIC            = AIC,
                       BIC            = BIC)
 return(result)
}

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
##  Residual analysis
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

residualAnalysis <- function(x, t, v, kernel= "normal", status){

 x        <- as.matrix(x)
 t        <- as.matrix(t)
 y        <- as.matrix(log(t))
 muhat    <- switch(kernel,
                    "normal" = mle(x, t, v, kernel = "normal", status)$muHat,
                    "t"      = mle(x, t, v, kernel = "t", status)$muHat
                   )

 alphaest <- switch(kernel,
                    "normal" =  mle(x, t, v, kernel = "normal",
                                    status)$alphaHat,
                    "t"      =  mle(x, t, v, kernel = "t", status)$alphaHat
                    )

 xi1hat   <- (2 / alphaest) * cosh((y - muhat) / 2)
 xi2hat   <- (2 / alphaest) * sinh((y - muhat) / 2)
 g        <- switch(kernel,
                     "normal" = exp(-(xi2hat ^ 2) / 2),
                     "t"      = (v + (xi2hat ^ 2)) ^ (-(v + 1) / 2)
                    )

 g0        <- switch(kernel,
                     "normal" = 1,
                     "t"      = v  ^ (-( v + 1) / 2)
                     )

 coef      <- switch(kernel,
                     "normal" = mle(x, t, v, kernel = "normal", status)$betaHat,
                     "t"      = mle(x, t, v, kernel = "t", status)$betaHat
                     )

 S         <- switch(kernel,
                     "normal" = obsFisInfMatr(x, t, v, kernel = "normal",
                                              status)$S,
                     "t"      = obsFisInfMatr(x, t, v, kernel = "t", status)$S
                     )

 y         <- as.matrix(log(t))
 # Martingale residual
 rM        <-as.vector(status+log(S))

 # Martingale-type residual
 rMD       <-as.vector(sign(rM) * (-2 * (rM + status * log(status - rM))) ^
             (1 / 2))

 # Deviance Component Residual
 rDC       <- as.vector(sqrt(2) * sign(y - muhat) * (log(g0) * status - status *
              log(cosh((y - muhat) / 2)) - status * log(g)) ^ (1 / 2) +
              sign(y - muhat) * (-2 * (1 - status) * log(S)) ^ (1 / 2))


 result    <- list(rDC = rDC)
 return(result)
}

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
##  Residual plot
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

residualPlot <- function(x, t, v, kernel= "normal", status, identifyPoint = 0){

 k       <- identifyPoint
 muhat   <- switch(kernel,
                   "normal" = mle(x, t, v, kernel = "normal", status)$muHat,
                   "t"      = mle(x, t, v, kernel = "t", status)$muHat
                   )

 rDC     <- switch(kernel,
                   "normal" = residualAnalysis(x, t, v, kernel = "normal",
                                               status)$rDC,
                   "t"      = residualAnalysis(x, t, v, kernel = "t",
                                               status)$rDC
                   )

 if(kernel == "normal"){
  plot(muhat, rDC, pch = 16, cex = 0.7, xlab = "Fitted value",
       ylab = "Deviance component residual")
  title(sub = "(a)", cex = 1.0)
  identify(muhat, rDC, n = k)

 }

 else{
  plot(muhat, rDC, pch = 16, cex = 0.7, xlab = "Fitted value",
       ylab = "Deviance component residual", ylim=c(-3.5,3.5))
  title(sub = "(a)", cex=1.0)
  abline(h = 2.57, lty = 2)
  abline(h = -2.57, lty = 2)
  identify(muhat, rDC, n = k)
 }
}

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
##  Normal probability plot
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

normProbPlot <- function(x, t, v, kernel= "normal", status){
 bs  <- function(n, a, b){

    if(is.na(n))
         return(NA)
         z   <-  rnorm(n,0,1)
         b * (1 + ((a ^ 2 * z ^ 2) / 2) + a * z * sqrt(1 + ((a ^ 2 * z ^ 2) / 4)))
 }

 bst <- function(n, a, b, v){

    if(is.na(n))
       return(NA)
     z  <- rt(n, df = v)
     b * ( 1 + ((a ^ 2 * z ^ 2) / 2) + a * z * sqrt(1 + ((a ^ 2 * z ^ 2) / 4)))
 }

 r        <- switch(kernel,
                    "normal" = residualAnalysis(x, t, v, kernel= "normal",
                                                status)$rDC,
                    "t"      = residualAnalysis(x, t, v, kernel= "t",
                                                status)$rDC
                     )

 alphaest <- switch(kernel,
                    "normal" =  mle(x, t, v, kernel = "normal",
                                    status)$alphaHat,
                    "t"      =  mle(x, t, v, kernel = "t", status)$alphaHat
                    )

 muhat   <- switch(kernel,
                    "normal" = mle(x, t, v, kernel = "normal", status)$muHat,
                    "t"      = mle(x, t, v, kernel = "t", status)$muHat
                    )

 n        <- length(t)
 e        <- matrix(0,n,100)
 e1       <- numeric(n)
 e2       <- numeric(n)
 fail     <- matrix(0,1,100)
 eta      <- exp(muhat)

 for(i in 1:100){
      tgenerated   <- switch(kernel,
                            "normal" = bs(n, alphaest, eta),
                            "t"      = bst(n, alphaest, eta, v)
                            )

     C            <- rlnorm(n, 6.8, 1) # censoring times
     delta        <- (tgenerated <= C)
     fail[,i]     <- sum(delta)/n
     Z            <- (1- delta) * C + delta * tgenerated #Z_i=min(T_i, C_i)
     fit1         <- switch(kernel,
                            "normal" = residualAnalysis(x, Z, v,
                                                        kernel= "normal",
                                                        status)$rDC,
                            "t"      = residualAnalysis(x, Z, v,
                                                        kernel= "t",
                                                        status)$rDC
                            )

     e[,i]        <- sort(fit1)

 }

 for(i in 1:n){
     eo    <- sort(e[i,])
     e1[i] <- eo[5]
     e2[i] <- eo[95]

 }

 med      <- as.matrix(apply(e,1,mean))
 faixa    <- range(r, e1, e2, med)
 par(pty = "s")
 qqnorm(r, main="", ylim=faixa, ylab="Deviance component residual",
        xlab = "Standard normal quantile", cex = 0.7, pch = 16)
 par(new = T)
 qqnorm(e1, axes = F, type = "l", main = "", ylim = faixa, lty = 1,
        xlab = "", ylab = "")
 par(new = T)
 qqnorm(e2, axes = F, type = "l", main = "", ylim = faixa, lty = 1, xlab = "",
        ylab = "")
 par(new = T)
 qqnorm(med, axes = F, type = "l", main = "", ylim = faixa, lty = 2, xlab = "",
        ylab = "")
 title(sub = "(b)", cex = 0.7)
}

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
##  Influence diagnostics
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

casePerturbation <- function(x, t, v, kernel= "normal", status,
                             param = "alpha"){

# Case-weight perturbation scheme

# Matrix delta calculations

 x         <- as.matrix(x)
 t         <- as.matrix(t)
 y         <- as.matrix(log(t))
 status    <- as.matrix(status)
 alphaest   <- switch(kernel,
                      "normal" =  mle(x, t, v, kernel = "normal",
                                    status)$alphaHat,
                      "t"      =  mle(x, t, v, kernel = "t", status)$alphaHat
                      )

 muhat      <- switch(kernel,
                      "normal" = mle(x, t, v, kernel = "normal", status)$muHat,
                      "t"      = mle(x, t, v, kernel = "t", status)$muHat
                      )

 xi1hat      <- (2 / alphaest) * cosh((y - muhat) / 2)
 xi2hat      <- (2 / alphaest) * sinh((y - muhat) / 2)

 wg          <- switch(kernel,
                       "normal" = -(1/2),
                       "t"      = -(v + 1) / (2 * (v + (xi2hat ^ 2)))
                      )


 h           <- switch(kernel,
                       "normal" = dnorm(xi2hat) / (1 - pnorm(xi2hat)),
                       "t"      = dt(xi2hat, df = v) / (1 - pt(xi2hat, df = v))
                      )

 bi          <- -(status) * ((xi1hat * xi2hat * wg) + (xi2hat / (2 * xi1hat))) +
                ((1 - status) / 2) * xi1hat * h
 ai          <- -(status / alphaest) - 2 * (status / alphaest) * (xi2hat ^ 2) *
                 wg + ((1 - status) / alphaest) * xi2hat * h
 b           <- as.vector(bi)

 Deltabetapc <- t(x) %*% diag(b)
 I            <- switch(kernel,
                       "normal" = obsFisInfMatr(x, t, v, kernel = "normal",
                                                status)$invObservMatrix,
                       "t"      = obsFisInfMatr(x, t, v, kernel = "t",
                                                status)$invObservMatrix
                      )

 Deltaalphapc <- t(ai)
 Deltapc      <- rbind(Deltabetapc, Deltaalphapc)
 B            <- t(Deltapc) %*% I %*% Deltapc

 # Eigenvector corresponding to the largest eigenvalue of the matrix C_l(theta)

 Lmax         <- eigen(B, symmetric=TRUE)$val[1]
 dmax         <- eigen(B, symmetric=TRUE)$vec[,1]

 # Total local influence

 Ci           <- 2*abs(diag(B))

 # Cutoff point

 cut1        <- 2*mean(Ci)
 Lalpha        <- switch(kernel,
                         "normal" = obsFisInfMatr(x, t, v, kernel = "normal",
                                                status)$Lalpha,
                         "t"      = obsFisInfMatr(x, t, v, kernel = "t",
                                                status)$Lalpha
                      )

  Lbeta        <- switch(kernel,
                         "normal" = obsFisInfMatr(x, t, v, kernel = "normal",
                                                  status)$Lbeta,
                         "t"      = obsFisInfMatr(x, t, v, kernel = "t",
                                                  status)$Lbeta
                         )

 p             <- dim(x)[2]
 n             <- dim(x)[1]
 b11           <- cbind(matrix(0, p, p), matrix(0, p, 1))
 b12           <- cbind(matrix(0, 1, p), Lalpha ^ (-1))
 B1            <- rbind(b11, b12)
 Bbeta         <- t(Deltapc) %*% (I - B1) %*% Deltapc
 Cibeta        <- 2 * abs(diag(Bbeta))
 cut2        <- 2 * mean(Cibeta)
 b211          <- cbind(solve(Lbeta), matrix(0, p, 1))
 b212          <- cbind(matrix(0, 1, p), matrix(0, 1, 1))
 B2            <- rbind(b211,b212)
 Balpha        <- t(Deltapc) %*% (I - B2) %*% Deltapc

# Eigenvector corresponding to the largest eigenvalue of the matrix C_l(beta)

 Lmaxbeta     <- eigen(Bbeta,symmetric=TRUE)$val[1]
 dmaxbeta     <- eigen(Bbeta,symmetric=TRUE)$vec[,1]
 Lmaxalpha    <- eigen(Balpha,symmetric=TRUE)$val[1]
 dmaxalpha    <- eigen(Balpha,symmetric=TRUE)$vec[,1]

 result       <- list(Bbeta     = Bbeta,
                      Balpha    = Balpha,
                      dmaxbeta  = dmaxbeta,
                      dmaxalpha = dmaxalpha
                      )
 return(result)
}

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
##  Total Influence plot
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

totalInfluencePlot <- function(x, t, v, kernel= "normal", status,
                             param = "alpha", identifyPoint = 0){

 k       <- identifyPoint
 if(param == "alpha"){

 Balpha  <- switch(kernel,
                  "normal" = casePerturbation(x, t, v, kernel= "normal", status,
                             param = "alpha")$Balpha,
                  "t"      = casePerturbation(x, t, v, kernel= "t", status,
                             param = "alpha")$Balpha
                  )

 Cialpha <- 2 * abs(diag(Balpha))
 cut3    <- 2 * mean(Cialpha)

 plot(Cialpha, ylab=expression(paste(C[i](alpha))), type = "p", ylim = c(0,10),
     cex = 0.5, pch = 16)
 title(sub = "(b)", cex = 0.7)
 abline(h = cut3, lty = 2)
 identify(Cialpha, n = k)
 }

 else{
  Bbeta  <- switch(kernel,
                  "normal" = casePerturbation(x, t, v, kernel= "normal", status,
                             param = "beta")$Bbeta,
                  "t"      = casePerturbation(x, t, v, kernel= "t", status,
                             param = "beta")$Bbeta
                  )
 Cibeta <- 2 * abs(diag(Bbeta))
 cut2   <- 2 * mean(Cibeta)
 plot(Cibeta,  ylab=expression(paste(C[i](beta))), type = "p", ylim = c(0,10),
      cex = 0.5, pch = 16)
 title(sub = "(a)",cex = 0.7)
 abline(h = cut2, lty = 2)
 identify(Cibeta, n = k)
 }
}

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
##  Case perurbation plot
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

casePerturbationPlot <- function(x, t, v, kernel= "normal", status,
                                 param = "alpha", identifyPoint = 0){

 k        <- identifyPoint

 if(param == "beta"){
 dmaxbeta <- switch(kernel,
                    "normal" = casePerturbation(x, t, v, kernel= "normal",
                                               status, param = "beta")$dmaxbeta,
                    "t"      = casePerturbation(x, t, v, kernel= "t", status,
                                               param = "beta")$dmaxbeta
                    )

 plot(abs(dmaxbeta), ylab = "|lmax|", type = "p", ylim = c(0, 1), cex = 0.5,
      pch = 16)
 title(sub = "(a)", cex = 0.7)

 identify(abs(dmaxbeta), n = k)
 }

 else{
  dmaxalpha <- switch(kernel,
                      "normal" = casePerturbation(x, t, v, kernel= "normal",
                                             status, param = "alpha")$dmaxalpha,
                      "t"      = casePerturbation(x, t, v, kernel= "t", status,
                                               param = "alpha")$dmaxalpha
                     )

 plot(abs(dmaxalpha),ylab="|lmax|",type="p", ylim=c(0,1), cex=0.5, pch=16)
 title(sub="(b)",cex=0.7)

 identify(abs(dmaxalpha), n = k)
 }
}

#------------------------------------------------------------------------------
# Function for computing the relative changes on the MLE
#------------------------------------------------------------------------------

relativeChange  <- function(x, t, v, kernel = "normal", status,
                            casesRemoved = NULL){

 alphaest   <- switch(kernel,
                      "normal" =  mle(x, t, v, kernel = "normal",
                                    status)$alphaHat,
                      "t"      =  mle(x, t, v, kernel = "t", status)$alphaHat
                      )

 coef       <- switch(kernel,
                         "normal" =  mle(x, t, v, kernel = "normal",
                                         status)$betaHat,
                         "t"      =  mle(x, t, v, kernel = "t", status)$betaHat
                         )

 deleted      <- x[casesRemoved,]
 newdata    <- x[-as.matrix(casesRemoved),]
 newt       <- t[-as.vector(casesRemoved)]
 newstatus  <- status[-as.vector(casesRemoved)]
 newalpha   <- switch(kernel,
                      "normal" =  mle(newdata, newt, v, kernel = "normal",
                                      newstatus)$alphaHat,
                      "t"      =  mle(newdata, newt, v, kernel = "t",
                                      newstatus)$alphaHat
                      )

 newbeta    <- switch(kernel,
                      "normal" =  mle(newdata, newt, v, kernel = "normal",
                                      newstatus)$betaHat,
                      "t"      =  mle(newdata, newt, v, kernel = "t",
                                      newstatus)$betaHat
                      )

 rcalpha      <- round(((alphaest - newalpha) / alphaest) * 100, 2)
 rcbeta       <- round((( coef - newbeta) / coef) * 100, 2)
 results      <- list(casesRemoved          = deleted,
                    alphaRelativeChanges  = rcalpha,
                    betaRelativeChanges   = rcbeta
                    )
 return(results)
}
