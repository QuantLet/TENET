# clear all variables
rm(list = ls(all = TRUE))
graphics.off()
# set the working directory
#setwd("C:/...")

# install and load packages
libraries = c("quantreg")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
    install.packages(x)
})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)
source("TENET_LqrL1.r")
source("quantilelasso.r")

# read the file which includes log returns of 100 firms and 7 macro state
# variables
x0  = read.csv("100_firms_returns_and_macro_2015-04-15.csv", header = TRUE)
# full balance sheet information of 100 firms
Bal_sheet_full = read.csv(file = "Bal_sheet.csv")
# 7 macro state variables
m   = as.matrix(x0[, 102:108])
# Value at Risk of 100 firms
VaR = as.matrix(read.csv("VaR_movingwindows_20150617.csv")[-1])[, 1:100]
# log returns of 100 firms
xx0 = x0[, 2:101]
# start the linear quantile lasso estimation for each firm
for (k in 1:100) {
  cat("Firm:", k)
  # log return of firm k
  y              = as.matrix(xx0[, k])
  # log returns of firms except firm k
  xx1            = as.matrix(xx0[, -k])  
  # balance sheet informaiton of firm k which includes 4 firm characteristics
  Bal_sheet      = Bal_sheet_full[, (4 * k - 3):(4 * k)]
  # combine macro state variables and 4 firm characteristics
  MB             = cbind(m, Bal_sheet)
  # number of rows of log return
  n              = nrow(xx1)
  # number of covariates
  p              = ncol(xx1) + ncol(MB)
  # Value at Risk of firms except firm k
  V              = as.matrix(VaR[, -k])
  # quantile level
  tau            = 0.05
  # moving window size
  ws             = 48
  
  # lambda calculated from linear quantile lasso
  lambda_l       = matrix(0, (n - ws), 1)
  # coefficients betas calculated from linear quantile lasso
  beta_l         = matrix(0, (n - ws), p)
  # estimated Conditional Value at Risk from linear quantile lasso
  CoVaR_l        = matrix(0, (n - ws), 1)
  
  # start the moving window estimation
  for (i in 1:(n - ws)) {
    print(i)
    yw  = y[i:(i + ws)]
    MBw = MB[i:(i + ws), ]
    mb  = matrix(0, ws + 1, ncol(MB))
    # standardize macro state variables and 4 firm characteristics
    for (j in 1:ncol(MB)) {
      mb[, j] = (MBw[, j] - min(MBw[, j]))/(max(MBw[, j]) - min(MBw[, j]))
    }
    mb[is.na(mb)] <- 0
    xx          = xx1[i:(i + ws), ]
    # all the independent variables
    xxw         = cbind(xx, mb)
    VaRM_est    = as.numeric(c(V[i, ], mb[(ws+1), ]))
    fit         = linear(yw, xxw, tau, i, k, LVaRest = VaRM_est)
    lambda_l[i] = fit$lambda.in
    beta_l[i, ] = fit$beta_in
    CoVaR_l[i]  = fit$pre_est
  }
  write.csv(beta_l, file   = paste("beta_L", k, ".csv", sep = ""))
  write.csv(CoVaR_l, file  = paste("CoVaR_L", k, ".csv", sep = ""))
  write.csv(lambda_l, file = paste("lambda_L", k, ".csv", sep = ""))
} 
