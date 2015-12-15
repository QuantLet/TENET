#######################################################################
### All rights are reserved by the authors.
### Authors: Youjuan Li, Ji Zhu, University of Michigan (jizhu@umich.edu)
### Date:    07/01/2006
#######################################################################
#Compute the entire L1-norm Path
#Plot the Fitted Coefficients vs. s
qrL1 <- function(x, y, a, max.steps,eps1=10^(-10), eps2=10^(-6), trace=T) {
### y in Real set 
### a_th quantile
### eps1: numerical
### eps2: lambda limit
### residual: y-f
### indL: index
### indV: index
### indE: index
### lambda save the -lambde values for all steps
  call <- match.call()
### To guarantee distinct y's even for tied data
  tmpy <- unique(y)
  if (length(tmpy) < length(y)) {
    dif <- min(diff(sort(tmpy)))
    y <- y + rnorm(length(y), 0, dif/100)
  }
### Can also transfer x to x=h(x),arbitrary basis functions
  
  if(trace)
    #cat("LASSO sequence\n")
  
  n <- dim(x)[1]
  m <- dim(x)[2]
  maxvars <- min(m, n - 1)
  indm <- seq(m)
  indn <- seq(n)
  indR <- indE <- NULL
  indL <- seq(n)
  ###z <- y*x
  
  ini <- qrL1Ini(x, y, a)
  indV <- ini$indV
  indE <- ini$indE
  indL <- ini$indL
  indR <- ini$indR
  u <- ini$u
  u0 <- ini$u0
  residual <- ini$residual
  
  if(missing(max.steps))
    max.steps <- n * maxvars 
  ###first column is for s=0, 2nd column is for the 1st event
  beta <- matrix(0, max.steps + 1, m)
  beta0 <- rep(0, max.steps + 1)
  Cgacv <- double(max.steps + 1)
  Csic <- double(max.steps + 1)
  fit <- matrix(0, max.steps + 1, n)
  checkf <- rep(0, max.steps + 1)
  lambda <- rep(0, max.steps + 1)
  lamb.the <- rep(0, max.steps + 1)
  sdistance <- rep(0, max.steps + 1)
  Elbow.list <- as.list(seq(max.steps+1))
  V.list <- as.list(seq(max.steps+1))
  theta.g <- matrix(0, max.steps + 1, n)
  beta0[1] <- ini$quant
  lambda[1] <- ini$lambda
  Elbow.list[[1]] <- NULL
  V.list[[1]] <- NULL
  ### Calculate criteria SIC and GACV
  fit[1,] <- beta0[1] 
  checkf[1] <- pf(fit[1,], y, a)
  trHat <- 0
  Cgacv[1] <- checkf[1] / (n-trHat)
  Csic[1] <- log(checkf[1]/n) + (log(n)/(2*n))*trHat
  theta.g[1,][indL] <- -(1-a)
  theta.g[1,][indR] <- a
  theta.g[1,][indE] <- a
  #cat("Initial variables:", indV, "\n")
  #cat("Initial obs at elbow:", indE, "\n")
 
  drop <- F # to do with "lasso"

  k <- 0
### Main loop for path
  while(k < max.steps) {
    k <- k + 1
    #cat("step:",k,"\n")
        
    ### Check how far to go in u
    ### Consider a point hits elbow
    gam <-  u0 + x[-indE,indV,drop=F] %*% u
    delta1 <- residual[-indE]/gam
    if (length(delta1[delta1<=0])==length(delta1)){
    delta <- Inf
    } else {
    delta.add <- delta1[delta1>0]
    delta <- min(delta.add[delta.add > eps1],na.rm=T)
    }

    ####For situation that beta may leave from V when k>1
    if(k > 1) {
      delta2 <- -beta[k, indV]/u
      if (length(delta2[delta2<=0])==length(delta2)){
      tmpz.remove <- Inf
      } else {
      tmpz <- delta2[delta2>0]
      tmpz.remove <- min(tmpz[tmpz > eps1], na.rm=T)
      }
      if (tmpz.remove < delta) {
        drop <- T
        delta <- tmpz.remove
      } else {
        drop <- F
      }  
    }
    #cat("Move distance:", delta, "\n")
       
    if (delta == Inf) {
      #cat("Path stops here.\n")
      break
    }
    
    sdistance[k+1] <- sdistance[k]+delta
    
    if (drop == T) {
      tmpdelta <- delta2[delta2 > eps1]
      tmpind <- indV[delta2 > eps1]    
      j1 <- tmpind[which.min(tmpdelta)]
      j2 <- which(indV == j1)
      #cat("Var:", j1, "removed\n")
    } else {
    
    ### next point hits elbow when the path go in u, denote it as istar
      tmpind <- indn[-indE]
      tmpdelta <- delta1[delta1 > eps1]
      tmpind <- tmpind[delta1 > eps1]
      istar <- tmpind[which.min(tmpdelta)]
      #cat("Obs:", istar, "hits elbow ")
      #if(match(istar, indL, FALSE))
        #cat("from left.\n")
      #else
        #cat("from right.\n")
    }
    
    ###Update parameters
    beta[k+1,] <- beta[k,]
    beta[k+1,indV] <- beta[k+1,indV] + delta * u
    beta0[k+1] <- beta0[k] + delta * u0
    residual[-indE] <- residual[-indE] - delta*gam
    Elbow.list[[k+1]] <- indE
    V.list[[k+1]] <- indV
    
    ### Calculate criteria SIC and GACV
    fit[k+1,] <- beta0[k+1] + beta[k+1,] %*% t(x)
    checkf[k+1] <- pf(fit[k+1,], y, a)
    trHat <- length(V.list[[k+1]])
    Cgacv[k+1] <- checkf[k+1] / (n-trHat)
    Csic[k+1] <- log(checkf[k+1]/n) + (log(n)/(2*n))*trHat    
   
    if (length(indV) != length(indE))
    warning("No. var != No. obs at elbow.\n")
 
    if (sum(indL)+sum(indR) == 1 && drop == F) {
      lambda[k] <- 0
      #cat("No point on left or right.\n")
      break
    }

  ### check which event occurs
    ### Add a new variable to indV, we have already known istar hitting elbow
    ### lambdvar and lambdaobs are -lambda
    if (length(indV) == m){
      lambdavar <- Inf
    } else {
      inactive <- indm[ - indV ]
      tmpE <- indE
      tmpL <- indL
      tmpR <- indR
      if (drop  == T){
        indV <- indV[ - j2 ]
      } else {
        tmpE <- c(tmpE, istar)
        if(match(istar, indL, FALSE)){
          tmpL<-setdiff(indL,istar)
        } else {
          tmpR<-setdiff(indR,istar)
        }
      }
      tmp <- rbind(c(0, sign(beta[k+1, indV])), cbind(1, x[tmpE, indV]))
      tmpb <- c(1, rep(0, length(indV)+1))
      tmplvar <- length(tmpb)
      uvar <- matrix(0, nrow=tmplvar, ncol=length(inactive))
      lambdavar <- rep(Inf, length(inactive))
      ###Start j loop
      for (j in 1:length(inactive)) {
        jstar <- inactive[j]
        tmpA1 <- cbind(tmp, c(1, x[tmpE,jstar]))
        tmpqr <- qr(tmpA1)
        if (tmpqr$rank < tmplvar)
          tmplam1 <- Inf
        else {
          tmpu1 <- qr.solve(tmpqr, tmpb)
          if (tmpu1[tmplvar] > 0) {
            tmpV <- c(indV, jstar)
            tmplam1 <- (1-a)*sum(cbind(rep(1,length(tmpL)), x[tmpL,tmpV,drop=F]) %*% tmpu1)-a*sum(cbind(rep(1,length(tmpR)), x[tmpR,tmpV,drop=F]) %*% tmpu1) 
          }
          else
            tmplam1 <- Inf
        }
        tmpA2 <- cbind(tmp, c(-1, x[tmpE,jstar]))
        tmpqr <- qr(tmpA2)
        if (tmpqr$rank < tmplvar)
          tmplam2 <- Inf
        else {
          tmpu2 <- qr.solve(tmpqr, tmpb)
          if (tmpu2[tmplvar] < 0) {
            tmpV <- c(indV, jstar)
            tmplam2 <- (1-a)*sum(cbind(rep(1,length(tmpL)), x[tmpL,tmpV,drop=F]) %*% tmpu2)-a*sum(cbind(rep(1,length(tmpR)), x[tmpR,tmpV,drop=F]) %*% tmpu2)
          }
          else
            tmplam2 <- Inf
        }
        if (tmplam1 == Inf && tmplam2 == Inf)
          lambdavar[j] <- Inf
        else if (tmplam1 < tmplam2) {
          lambdavar[j] <- tmplam1
          uvar[,j] <- tmpu1
        }
        else if (tmplam1 > tmplam2) {
          lambdavar[j] <- tmplam2
          uvar[,j] <- tmpu2
        }
        else {
          lambdavar[j] <- tmplam1
          uvar[,j] <- tmpu1
          warning("tmplam1 == tmplam2 \n")
          #cat("Tie in variable:", jstar, "\n")
        }
      }
      ###end of j loop
    }
    
                     
 ### Remove an observation from indE
    tmp <- rbind(c(0, sign(beta[k+1, indV])), cbind(1, x[tmpE, indV]))
    tmpb <- c(1, rep(0, length(indV)))
    tmplobs <- length(tmpb)
    uobs <- matrix(0, nrow=length(indV)+1, ncol=length(tmpE))
    lambdaobs <- rep(0, length(tmpE))
    leftobs <- rep(F, length(tmpE))
    ###begin i loop
    for (i in 1:length(tmpE)) {
      tmpA <- tmp[-(i+1),]
      tmpqr <- qr(tmpA)
      if (tmpqr$rank < tmplobs) {
        tmplam <- Inf
      } else {
        tmpu <- qr.solve(tmpqr, tmpb)
        uobs[,i] <- tmpu
        tmplam <- (1-a)*sum(cbind(rep(1,length(tmpL)), x[tmpL,indV,drop=F]) %*% tmpu)-a*sum(cbind(rep(1,length(tmpR)), x[tmpR,indV,drop=F]) %*% tmpu)                 
        tmpi <- tmpE[i]
        tmpyf <- c(1, x[tmpi,indV,drop=F]) %*% tmpu
        if (tmpyf > 0) {
        ###y_i move to Left
          tmplam <- tmplam + (1-a)*tmpyf
          leftobs[i] <- T
        } else {
        ###y_i move to Right
          tmplam <- tmplam - a*tmpyf
        }
      }
      lambdaobs[i] <- tmplam
    }
    ##end i loop
    
### Compare var and obs
    lam1 <- min(lambdavar)
    lam2 <- min(lambdaobs)
    if (lam1 < lam2 &&  lam1 < 0) {
    ### one variable not in |V| is added into |V|
      tmpj <- which.min(lambdavar)
      jstar <- inactive[tmpj]
      indV <- c(indV, jstar)
      ### If drop==T, on change for E
      if (drop == F) {
        indE <- c(indE, istar)
        if(match(istar, indL, FALSE))
           indL <- setdiff(indL,istar)
        else
           indR <- setdiff(indR,istar)
      }
      u0 <- uvar[1,tmpj]
      u <- uvar[2:tmplvar,tmpj]
      lambda[k+1] <- lam1
      #cat("Variable:", jstar, "added\n")
    } else if (lam2 < lam1 && lam2 < 0) {
    ### remove a point from indE 
      if (drop == F) {
      ## istar stay on E, no change for V  
        if(match(istar, indL, FALSE)){
           indL <- setdiff(indL,istar)
        } else{
           indR <- setdiff(indR,istar)
        }   
      }
      tmpi <- which.min(lambdaobs)
      ### update E
      istar <- tmpE[tmpi]
      u0 <- uobs[1,tmpi]
      u <- uobs[2:tmplobs,tmpi]
      lambda[k+1] <- lam2
      #cat("Observation:", istar,"removed ")
      if (leftobs[tmpi] == F) {
        #cat("to right.\n")
        indR <- c(indR,istar)
      } else {
        #cat("to left.\n")
        indL <- c(indL,istar)
      }
      indE <- tmpE[-tmpi]
    } else {
      #cat("lam1:", lam1, "\n")
      #cat("lam2:", lam2, "\n")
      #cat("No descent!\n")
      break
    }     
    if (abs(lambda[k+1]) < eps2) {
      #cat("Descent too small.\n")
      break
    }
    drop <- F   
  }  
  ###end of main loop for path
  
  beta <- beta[seq(k+1),]
  beta0 <- beta0[seq(k+1)]
  lambda <- lambda[seq(k+1)]
  #lamb.the <- lamb.the[seq(k+1)]
    
  object <- list(call = call, beta0 = beta0, beta = beta, Elbow = Elbow.list[seq(k+1)], V = V.list[seq(k+1)], lambda=lambda, s=sdistance[seq(k+1)], Csic=Csic[seq(k+1)], Cgacv=Cgacv[seq(k+1)])
  object
}
qrL1Ini <- function(x, y, a, eps=10^(-10)) {
### y in Real set
### > eps is defined as nonzero
### a_th quantile
### lambda is "-lambda"
  n <- dim(x)[1]
  m <- dim(x)[2]
  yr <- sort(y)
  ### Two cases 
  quant <- yr[ floor(n*a)+1 ]
  index <- match(quant, y)
  indm <- seq(m)
  indE <- index
  indR <- seq(y)[y > y[index]]
  indL <- seq(y)[y < y[index]]
  indV <- NULL
  beta0<-quant
  beta <- rep(0,m)
  ###current f=beta0
  residual <- y-beta0
  inactive <- indm
  tmpE <- indE
  tmpL <- indL
  tmpR <- indR
  tmpb <- c(1,0)
  tmplvar <- length(tmpb)
  uvar <- matrix(0, nrow=tmplvar, ncol=length(inactive))
  lambdavar <- rep(Inf, length(inactive))
  ###beginning of loop j
      for (j in 1:length(inactive)) {
        jstar <- inactive[j]
        tmpA1 <- cbind(c(0,1), c(1, x[tmpE,jstar]))
        tmpqr <- qr(tmpA1)
        if (tmpqr$rank < tmplvar)
          tmplam1 <- Inf
        else { 
        tmpu1 <- qr.solve(tmpqr, tmpb)
        if (tmpu1[tmplvar] > 0) {
                   tmpV <- jstar
                   tmplam1 <- (1-a)*sum(cbind(rep(1,length(tmpL)), x[tmpL,tmpV,drop=F]) %*% tmpu1)-a*sum(cbind(rep(1,length(tmpR)), x[tmpR,tmpV,drop=F]) %*% tmpu1) 
                   } else tmplam1 <- Inf
        }
        tmpA2 <- cbind(c(0,1), c(-1, x[tmpE,jstar]))
        tmpqr <- qr(tmpA2)
        if (tmpqr$rank < tmplvar)
          tmplam2 <- Inf
        else { tmpu2 <- qr.solve(tmpqr, tmpb)
               if (tmpu2[tmplvar] < 0) {
               tmpV <- jstar
               tmplam2 <- (1-a)*sum(cbind(rep(1,length(tmpL)), x[tmpL,tmpV,drop=F]) %*% tmpu2)-a*sum(cbind(rep(1,length(tmpR)), x[tmpR,tmpV,drop=F]) %*% tmpu2)
               }
             else tmplam2 <- Inf
        }
        
        if (tmplam1 == Inf && tmplam2 == Inf)
          lambdavar[j] <- Inf
        else if (tmplam1 < tmplam2) {
          lambdavar[j] <- tmplam1
          uvar[,j] <- tmpu1
        }
        else if (tmplam1 > tmplam2) {
          lambdavar[j] <- tmplam2
          uvar[,j] <- tmpu2
        }
        else {
          lambdavar[j] <- tmplam1
          uvar[,j] <- tmpu1
          warning("tmplam1 == tmplam2 \n")
          #cat("Tie in variable:", jstar, "\n")
        }
      }
     ###end of loop j 
  lambda <- min(lambdavar)
  tmpj <- which.min(lambdavar)
  jstar <- inactive[tmpj]
  indV <- jstar
  u0 <- uvar[1,tmpj]
  u <- uvar[2:tmplvar,tmpj]
  ####################Not updat right now
  ###Compute how far to go in u
  #gamL <- (1-a)*( u0 + x[indL,indV,drop=F] %*% u)
  #gamR <- -a*( u0 + x[indR,indV,drop=F] %*% u) 
  #delta1L <- residual[indL]/gamL
  #delta1R <- residual[indR]/gamR
  #delta1 <- c(delta1L,delta1R)
  #delta <- min(delta1[delta1 > eps], na.rm=T)
  #cat("Move distance:", delta, "\n")
  #beta[indV] <- delta * u
  #beta0 <- beta0 + delta * u0
  #residual[indL] <- residual[indL] - delta*gamL
  #residual[indR] <- residual[indR] - delta*gamR
  #residual [indE] <- 0 
  ########################
  return(list(beta=beta, beta0=beta0, u=u, u0=u0, quant=quant,lambda = lambda,
              indV=indV, indE=indE, indR=indR, indL=indL, residual=residual))
}

qrL1.predict <- function(obj, x, y=NULL) {
### obj: output of svmL1
### y \in {1,-1}
  N <- length(y)
  f <- obj$beta %*% t(x) + obj$beta0
  predict <- sign(f)
  if (is.null(y))
    return(list(f=f, error=NULL))
  error <- apply(apply(predict, 1, FUN="!=", y), 2, sum)/N
  return(list(f=f, error=error))
}
                            
###this defines the check function
pf <- function(beta0, y0, tau) {
  tmp <- y0 - beta0
  sum(tau*tmp[tmp>0]) - sum((1-tau)*tmp[tmp<=0])  
}

##Get path of coefficients
"plot.L1qr" <-
  function(x, breaks = TRUE, eps = 1e-10, ...)
{
  object <- x  
  coef1 <- object$beta  ### Get rid of many zero coefficients
  ##coef1 <- scale(coef1, FALSE, 1/object$normx)
  ##coef1[,4066] <- 0 
    c1 <- drop(rep(1, nrow(coef1)) %*% abs(coef1))
    nonzeros <- c1 > eps
    cnums <- seq(nonzeros)[nonzeros]
    coef1 <- coef1[, nonzeros]
  
  s1 <- apply(abs(coef1), 1, sum)
  s1 <- s1/max(s1)
 
  #xname<-"|beta|/max|beta|"                
  xname<-"s" 
      matplot(s1, coef1, xlab = xname, ..., type = "b", lty=rep(1,dim(coef1)[2]),lwd=2,
              pch = "*", ylab = "Coefficients", cex.lab=1.5, cex.axis=1.5)
      #title("Quantile Lasso Path",line=2.5)
      abline(h = 0, lty = 3)
      axis(4, at = coef1[nrow(coef1),  ], label = paste(cnums
                                            ), cex = 2, cex.lab=2, cex.axis=1.5, adj = 1)
      if(breaks) {
        #axis(3, at = s1, labels = paste(seq(s1)-1),cex=.8)
        abline(v = s1,lwd=0.5,col="grey")
      }

  invisible()
}