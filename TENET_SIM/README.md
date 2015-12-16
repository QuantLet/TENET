
[<img src="https://github.com/QuantLet/Styleguide-and-Validation-procedure/blob/master/pictures/banner.png" alt="Visit QuantNet">](http://quantlet.de/index.php?p=info)

## [<img src="https://github.com/QuantLet/Styleguide-and-Validation-procedure/blob/master/pictures/qloqo.png" alt="Visit QuantNet">](http://quantlet.de/) **TENET_SIM** [<img src="https://github.com/QuantLet/Styleguide-and-Validation-procedure/blob/master/pictures/QN2.png" width="60" alt="Visit QuantNet 2.0">](http://quantlet.de/d3/ia)

```yaml
﻿
Name of QuantLet: TENET_SIM

Published in: TENET

Description: 'estimates Conditional Value at Risk (CoVaR) of 100 financial
institutions by using Single-Index Model with variable selection. The 110 covariates
include log returns of 99 firms (except for firm k) 7 macro state variables and
4 firm k’s characteristics. The data is not publicly published.'

Keywords: 'tail, quantile regression, CoVaR, systemic Risk, variable selection,
dimension reduction, risk, bic'

See also: quantilelasso, SIMqrL1, TENET_Linear

Author: Weining Wang, Lining Yu

Submitted:

Datafile: '100_firms_returns_and_macro_2015-04-15.csv, Bal_sheet.csv,
VaR_movingwindows_20150617.csv'

Input: 
- yw : (ws+1) response vector
- xxw : px(ws+1) covariate matrix
- tau : scalar quantile level
- VaRM_est : p estimated VaR and macro variables

Output:
- lambda_sim[l] : scalar estimated penalization parameter
- beta_sim[l, ] : p estimated coefficients
- CoVaR_sim[l] : scalar estimated CoVaR
- first_der[l] : scalar estimated first derivative
- partial_der[l, ] : p estimated partial derivatives

```


```r

# clear all variables
rm(list = ls(all = TRUE))
graphics.off()
# set the working directory 
# setwd('C:/...')
# install and load packages
libraries = c("quantreg", "KernSmooth", "SparseM", "MASS")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
  install.packages(x)
})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)
source("TENET_SIMqrL1.r")
source("quantilelasso.r")

# read the file which includes log returns of 100 firms and 7 macro state
# variables
x0 = read.csv("100_firms_returns_and_macro_2015-04-15.csv", header = TRUE)
# all firms' characteristics from balance sheet information of 100 firms
Bal_sheet_full = read.csv(file = "Bal_sheet.csv")
# 7 macro state variables
m  = as.matrix(x0[, 102:108])
# estimated Value at Risk of 100 firms
VaR = as.matrix(read.csv("VaR_movingwindows_20150617.csv")[-1])[, 1:100]
# log returns of 100 firms
xx0 = x0[, 2:101]
# start the linear quantile lasso estimation for each firm
for (k in 1:100) {
  cat("Firm:", k)
  # log return of firm k
  y   = as.matrix(xx0[, k])
  # log returns of firms except firm k
  xx1 = as.matrix(xx0[, -k])
  # 4 firm characteristics from balance sheet informaiton of firm k
  BS  = Bal_sheet_full[, (4 * k - 3):(4 * k)]
  # combine macro state variables and 4 firm characteristics
  MB  = cbind(m, BS)
  # number of rows of log return
  n   = nrow(xx1)
  # number of covariates
  p   = ncol(xx1) + ncol(MB)
  # estimated Value at Risk of firms except firm k
  V   = as.matrix(VaR[, -k])
  # quantile level
  tau = 0.05
  # moving window size equals 48 corresponds to one year weekly data
  ws  = 48
  
  lambda_sim  = matrix(0, (n - ws), 1)
  beta_sim    = matrix(0, (n - ws), p)
  covar_sim   = matrix(0, (n - ws), 1)
  first_der   = matrix(0, (n - ws), 1)
  partial_der = matrix(0, (n - ws), p)
  
  for (l in 1:(n - ws)) {
    print(l)
    yw  = y[l:(l + ws)]
    MBw = MB[l:(l + ws), ]
    mb  = matrix(0, ws + 1, ncol(MB))
    # standardize macro state variables and 4 firm characteristics
    for (j in 1:ncol(MB)) {
      mb[, j] = (MBw[, j] - min(MBw[, j]))/(max(MBw[, j]) - min(MBw[, j]))
    }
    mb[is.na(mb)]   = 0
    MBw[is.na(MBw)] = 0
    xx       = xx1[l:(l + ws), ]
    # all the independent variables
    xxw      = cbind(xx, mb)
    VaRM_est = as.numeric(c(V[l, ], mb[(ws + 1), ]))
    fit      = sim(yw, xxw, tau, Qmaxiter = 2, l, k, LVaRest = VaRM_est)
    beta_sim[l, ]    = fit$beta_final
    lambda_sim[l]    = fit$lambda.fi
    # the final estimated CoVaR
    covar_sim[l]     = fit$a.fi
    first_der[l]     = fit$b.fi
    # the estimated partial derivatives
    partial_der[l, ] = fit$c.fi
  }
} 

```
