
[<img src="https://github.com/QuantLet/Styleguide-and-Validation-procedure/blob/master/pictures/banner.png" alt="Visit QuantNet">](http://quantlet.de/index.php?p=info)

## [<img src="https://github.com/QuantLet/Styleguide-and-Validation-procedure/blob/master/pictures/qloqo.png" alt="Visit QuantNet">](http://quantlet.de/) **TENET_SIM** [<img src="https://github.com/QuantLet/Styleguide-and-Validation-procedure/blob/master/pictures/QN2.png" width="60" alt="Visit QuantNet 2.0">](http://quantlet.de/d3/ia)

```yaml
﻿
Name of QuantLet: TENET_SIM

Published in: TENET

Description: 'estimates Conditional Value at Risk (CoVaR) of 100 financial
institutions by using Single-Index Model with variable selection. The 110
covariates include log returns of 99 firms (except for firm k) 7 macro state
variables and 4 firm k’s characteristics. Then generates the necessory files for
other TENET quantlets. The data is not publicly published.'

Keywords: 'tail, quantile regression, CoVaR, systemic Risk, variable selection,
dimension reduction, risk, bic'

See also: 'quantilelasso, SIMqrL1, TENET_Linear, TENET_total_connectedness,
TENET_total_in_out_groups, TENET_group_network, TENET_total_in_out_individual,
TENET_SIFIs, TENET_VaR_CoVaR'

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


# Step 1: The main code of TENET based on quantile regression for Single-Index
# Model with Variable selection technique

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
write.csv(lambda_sim,  file = paste("lambda_sim_", k, ".csv", sep = ""))
write.csv(beta_sim,    file = paste("beta_sim_", k, ".csv", sep = ""))
write.csv(covar_sim,   file = paste("covar_sim_", k, ".csv", sep = ""))
write.csv(first_der,   file = paste("first_der_", k, ".csv", sep = ""))
write.csv(partial_der, file = paste("partial_der_", k, ".csv", sep = ""))
} 

# Step 2: generate the necessory csv files for other TENET quantlets

# number of columns in each partial derivative matrix
cpd = 100
# number of rows in each partial derivative matrix
rpd = (n - ws)

library(miscTools)
# since each firm does not regress on itself, we need to insert a zero column
# vector in the position of every firms' partial derivative matrix
vec_zero = matrix(0, rpd, 1)
der.c = array(0, dim = c(rpd, cpd, cpd))
for (i in 1:100) {
  der.c[, , i] = insertCol(as.matrix(read.csv(file = paste("partial_der_", i, ".csv", 
  sep = "")))[, 2:100], i, vec_zero)
}

# generate the connnectedness matrix
con = array(0, dim = c(cpd, cpd, rpd))
for (i in 1:rpd) {
  con.v = rep(0, 100)
  for (j in 1:cpd) {
    con.v = rbind(con.v, der.c[i, , j])
  }
  con[, , i] = con.v[-1, ]
}

# the date for the data files
dt        = as.Date(x0[, 1], format = "%d/%m/%Y")[(ws + 1):314]
Date      = strptime(as.character(dt), "%Y-%m-%d")
Date1     = format(Date, "%d/%m/%Y")
dt        = as.data.frame(Date1)
names(dt) = "Date"

# the total connectedness
total.c = rep(0, rpd)
for (i in 1:rpd) {
  total.c[i] = sum(abs(con[, , i]))
}

# the average lambda series
full.lambda = matrix(0, rpd, cpd)
for (j in 1:100) {
  lambda.firm      = read.csv(file = paste("lambda_sim_", j, ".csv", sep = ""))
  full.lambda[, j] = as.matrix(lambda.firm)[, 2]
}
average_lambda = 1/cpd * (rowSums(full.lambda))
tc_l           = cbind(dt, total.c, average_lambda)
# generate the necessory file for the quantlet TENET_total_connectedness
write.csv(tc_l, file = "total_connectedness_and_averaged_lambda.csv", row.names = FALSE)

# total in groups 
# in bank
total.in.b = matrix(0, 266, 1)
for (i in 1:rpd) {
  total.in.b[i] = sum(abs(con[, , i])[1:25, ])
}

# in insurance
total.in.ins = matrix(0, 266, 1)
for (i in 1:rpd) {
  total.in.ins[i] = sum(abs(con[, , i])[26:50, ])
}

# in broker dealer
total.in.d = matrix(0, 266, 1)
for (i in 1:rpd) {
  total.in.d[i] = sum(abs(con[, , i])[51:75, ])
}

# in others
total.in.o = matrix(0, 266, 1)
for (i in 1:rpd) {
  total.in.o[i] = sum(abs(con[, , i])[76:100, ])
}
tc_in = cbind(total.in.b, total.in.ins, total.in.d, total.in.o)
colnames(tc_in) = c("Depositories_in", "Insurers_in", "Broker-Dealers_in", "Others_in")

# total out groups 
# out bank
total.out.b = matrix(0, 266, 1)
for (i in 1:rpd) {
  total.out.b[i] = sum(abs(con[, , i])[, 1:25])
}

# out insurance
total.out.ins = matrix(0, 266, 1)
for (i in 1:rpd) {
  total.out.ins[i] = sum(abs(con[, , i])[, 26:50])
}

# out broker dealer
total.out.d = matrix(0, 266, 1)
for (i in 1:rpd) {
  total.out.d[i] = sum(abs(con[, , i])[, 51:75])
}

# out others
total.out.o = matrix(0, 266, 1)
for (i in 1:rpd) {
  total.out.o[i] = sum(abs(con[, , i])[, 76:100])
}
tc_out = cbind(total.out.b, total.out.ins, total.out.d, total.out.o)
colnames(tc_out) = c("Depositories_out", "Insurers_out", "Broker-Dealers_out", "Others_out")
tc_group = cbind(dt, tc_in, tc_out)
# generate the necessory file for the quantlet TENET_total_in_out_groups
write.csv(tc_group, file = "total_in_and_out.csv", row.names = FALSE)

# the connectedness matrix on the date 2009-06-12
i = 80
totc_t_80 = t(con[, , i])
# generate the necessory file for the quantlet TENET_group_network
write.csv(totc_t_80, file = "totc_JPM_t_80.csv")

# the total connecteness matrix aggregated over windows
tot.ct = matrix(0, cpd, cpd)
for (i in 1:rpd) {
  tot.ct = tot.ct + abs(con[, , i])
}
colnames(tot.ct) = colnames(xx0)
# generate the necessory file for the quantlets TENET_total_in_out_individual and
# TENET_SIFIs
write.csv(tot.ct, file = "tot_c_overtime.csv", row.names = FALSE) 


```
