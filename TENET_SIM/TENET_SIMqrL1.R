sim = function(Qy, Qxx, Qp, Qmaxiter, Qi = l, Qj = k, LVaRest) {
  # local polynomial regression quantile which is used in the iteration
  lprq0 = function(x, y, h, tau, x0) {
    require(quantreg)
    fv = x0
    dv = x0
    z  = x - x0
    wx = dnorm(z/h)
    r  = rq(y ~ z, weights = wx, tau = tau, ci = FALSE)
    fv = r$coef[1]
    dv = r$coef[2]
    list(x0 = x0, fv = fv, dv = dv)
  }
  
  # set the critical value to control the iteration
  crit  = 0.01
  
  fit   = qrL1(Qxx, Qy, Qp, 50)
  # choose the number which has the minimal value of generalized approximate cross
  # validation criterion
  isnum = which(fit$Cgacv == min(fit$Cgacv))
  # the initial estimated betas
  beta.in   = fit$beta[isnum, ]
  # the initial estimated penalization parameter
  lambda_in = (-fit$lambda[isnum])
  print(lambda_in)
  d = length(beta.in)
  n = NROW(Qy)
  # standardize the initial betas
  beta.in  = beta.in/sqrt(sum(beta.in^2))
  # initialize the new estimated betas in the iterations
  beta.new = rep(0, d)
  # initialize the number of iteration
  iter = 1
  # initialize the link function 'a' and its first derivative 'b'
  a.in = rep(0, n)
  b.in = rep(0, n)
  
  # start the iteration
  while ((iter < Qmaxiter) & (sum((beta.new - beta.in)^2) > crit)) {
    print(iter)
    if (iter > 1) {
      beta.in = beta.new
    }
    # step 1: compute a.in and b.in
    index_x = rowSums(t(t(Qxx) * beta.in))
    yorder  = Qy[order(index_x)]
    xorder  = sort(index_x)
    hm = dpill(xorder[2:(length(index_x) - 2)], yorder[2:(length(index_x) - 2)])
    hm[is.na(hm)] = 0.08
    hp = 10 * hm * (Qp * (1 - Qp)/(dnorm(qnorm(Qp)))^2)^0.2
    x0 = 0
    for (j in 1:n) {
      x0  = index_x[j]
      fit = lprq0(index_x, Qy, hp, Qp, x0)
      a.in[j] = fit$fv
      b.in[j] = fit$dv
    }
    # step 2: compute beta.new
    ynew = rep(0, n^2)
    xnew = rep(0, n^2 * d)
    xnew = matrix(xnew, ncol = d)
    ynew = (rep(Qy, each = n) - rep(a.in, times = n))
    xnew = rep(b.in, times = n) * (Qxx[rep(1:n, each = n), ] - Qxx[rep(1:n, 
    times = n), ])
    wts  = dnorm(rowSums(t(t(xnew) * beta.in))/hp)
    ynew = sqrt(wts) * ynew
    wtss = replicate(d, wts)
    xnew = xnew * sqrt(wtss)
    fit  = qrL1(xnew, ynew, Qp, 200)
    # choose the number which minimizes Bayesian information criterion (BIC, or
    # SIC)
    isnum      = which(fit$Csic == min(fit$Csic))
    beta.new   = fit$beta[isnum, ]
    beta.new   = beta.new/sqrt(sum(beta.new^2))
    lambda_new = (-fit$lambda[isnum])
    iter       = iter + 1
  }
  print(lambda_new)
  print(which(beta.new != 0))
  index_final  = rowSums(t(t(Qxx) * beta.new))
  value_x      = seq(min(index_final), max(index_final), length = length(Qy))
  linkfunest   = matrix(0, length(value_x), 1)
  for (i in 1:length(value_x)) {
    fit2          = lprq0(index_final, Qy, hp, Qp, value_x[i])
    linkfunest[i] = fit2$fv
  }
  index_est = LVaRest %*% beta.new
  fit1      = lprq0(index_final, Qy, hp, Qp, index_est)
  # a.fi is the estimated g(), i.e. estimated CoVaR
  a.fi = fit1$fv
  # b.fi is the estimated g'()
  b.fi = fit1$dv
  # c.fi is the estimated betas multiply g'(), i.e. the estimated partial
  # derivatives
  c.fi = b.fi * beta.new
  
  finalresults            = list() 
  finalresults$beta_final = beta.new
  finalresults$lambda.fi  = lambda_new
  finalresults$a.fi = a.fi
  finalresults$b.fi = b.fi
  finalresults$c.fi = c.fi
  return(finalresults)
}



 
