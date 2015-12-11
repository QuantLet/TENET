
linear      = function(Qy, Qxx, Qp, Qi, Qj, LVaRest) {
  fit       = qrL1(Qxx, Qy, Qp, 50)
  isnum     = which(fit$Cgacv == min(fit$Cgacv))
  beta.in   = fit$beta[isnum, ]
  lambda_in = (-fit$lambda[isnum])
  print(lambda_in)
  beta0     = fit$beta0[isnum]
  print(which(beta.in != 0))
  beta.new  = c(beta0, beta.in)
  beta.new  = beta.new/sqrt(sum(beta.new^2))
  Lest      = c(1, LVaRest)
  pre_est   = Lest %*% beta.new
  
  finalresults = list()
  finalresults$lambda.in = lambda_in
  finalresults$beta_in   = beta.new[-1]
  finalresults$pre_est   = pre_est
  return(finalresults)
}

 
