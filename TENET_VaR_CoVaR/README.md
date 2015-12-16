
[<img src="https://github.com/QuantLet/Styleguide-and-Validation-procedure/blob/master/pictures/banner.png" alt="Visit QuantNet">](http://quantlet.de/index.php?p=info)

## [<img src="https://github.com/QuantLet/Styleguide-and-Validation-procedure/blob/master/pictures/qloqo.png" alt="Visit QuantNet">](http://quantlet.de/) **TENET_VaR_CoVaR** [<img src="https://github.com/QuantLet/Styleguide-and-Validation-procedure/blob/master/pictures/QN2.png" width="60" alt="Visit QuantNet 2.0">](http://quantlet.de/d3/ia)

```yaml
Name of QuantLet: TENET_VaR_CoVaR

Published in: TENET

Description: 'Estimates the Value at Risks (VaRs) of 100 financial institutions using moving window
estimation based on seven macro state variables. The plot shows the log returns of JP Morgan (black
points) and VaR of JP Morgan (red line) as an example. The estimated CoVaR by using Single-Index
Model with LASSO (blue line) and estimated CoVaR using linear quantile LASSO model(green line) are
also plotted as comparison.'

Keywords: tail, quantile regression, VaR, estimation, risk

Author: Lining Yu

Submitted:

Datafile: 100_firms_returns_and_macro.csv, CoVaR_sim_l_JPM.csv

Input: log returns of 100 financial institutions and 7 macro state variables

Output: estimated VaRs for 100 financial institutions

Example: 'Plot of the log returns of JP Morgan (black points), its estimated VaR (red line), its
estimated CoVaR by using Single-Index Model with LASSO (blue line), and its estimated CoVaR by
using linear quantile LASSO model (green line).'
```

![Picture1](TENET_VaR_CoVaR.png)


```r

# clear all variables
rm(list = ls(all = TRUE))
graphics.off()

# set the working directory
#setwd("C:/...")

# install and load packages
libraries  = c("quantreg")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
    install.packages(x)
})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)

data       = read.csv("100_firms_returns_and_macro.csv", header = TRUE)
dt         = as.Date(data[, 1], format = "%d/%m/%Y")

# read the macro state variables
data_m     = as.matrix(data[, 102:108])

# read the log returns of 100 financial institutions
data_y     = as.matrix(data[, 2:101])

# set the quantile level
tau        = 0.05

nncol      = ncol(data_y)
nnrow      = nrow(data_y)
lengthfull = nnrow

# the moving window size is 48
winsize    = 48
VaR        = matrix(0, ncol = nncol, nrow = (lengthfull - winsize))

# start the moving window VaR prediction, store the predict values
for (j in 1:nncol) {
  for (i in 1:(lengthfull - winsize)) {
    ycut   = data_y[i:(i + winsize), j]
    xcut   = data_m[i:(i + winsize), ]
    xxcut  = matrix(0, nrow(xcut), ncol(xcut))
    # standardize macro state variables
    for (k in 1:ncol(xcut)) {
      xxcut[, k] = (xcut[, k] - min(xcut[, k]))/(max(xcut[, k]) - min(xcut[, k]))
    }
    fit       = rq(ycut ~ xxcut, tau)
    pre       = predict(fit, quantiles = tau)
    VaR[i, j] = pre[length(pre)]
  }
}
VaR         = round(VaR, digits = 9)
write.csv(VaR, file = "VaR_movingwindows.csv")

# plot log returns of JP morgan
i         = 2
dt_est    = dt[(winsize + 1):(winsize + (lengthfull - winsize))]
plot(dt_est, data_y[(winsize + 1):(winsize + (lengthfull - winsize)), i], 
type = "p", col = "grey2", lwd = 8, ylab = "", xlab = "", cex.axis = 2.5, 
font.axis = 2.5, ylim = range(-1.1, 1.1))

# plot the estimated CoVaR of JP morgan by using Single-Index model with lasso
CoVaR_sim = as.vector(read.csv("CoVaR_sim_l_JPM.csv", header = TRUE)[, 1])
lines(dt_est, CoVaR_sim, lwd = 8, col = "blue")

# plot the estimated CoVaR of JP morgan by using linear quantile lasso
CoVaR_l   = as.vector(read.csv("CoVaR_sim_l_JPM.csv", header = TRUE)[, 2])
lines(dt_est, CoVaR_l, lwd = 3, col = "green4")

# plot the estimated VaR of JP morgan by using moving window estimation
lines(dt_est, VaR[, i], lwd = 3, col = "red")

```
