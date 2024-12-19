<div style="margin: 0; padding: 0; text-align: center; border: none;">
<a href="https://quantlet.com" target="_blank" style="text-decoration: none; border: none;">
<img src="https://github.com/StefanGam/test-repo/blob/main/quantlet_design.png?raw=true" alt="Header Image" width="100%" style="margin: 0; padding: 0; display: block; border: none;" />
</a>
</div>

```
Name of QuantLet: TENET_SIM

Published in: TENET

Description: 'estimates Conditional Value at Risk (CoVaR) of 100 financial

Keywords: 'tail, quantile regression, CoVaR, systemic Risk, variable selection,

See also: 'quantilelasso, SIMqrL1, TENET_Linear, TENET_total_connectedness,

Author: Weining Wang, Lining Yu

Submitted: 

Datafile: '100_firms_returns_and_macro_2015-04-15.csv, Bal_sheet.csv,

Input: 

- yw: (ws+1) response vector

- xxw: px(ws+1) covariate matrix

- tau: scalar quantile level

- VaRM_est: p estimated VaR and macro variables

Output: 

- lambda_sim[l]: scalar estimated penalization parameter

- beta_sim[l, ]: p estimated coefficients

- CoVaR_sim[l]: scalar estimated CoVaR

- first_der[l]: scalar estimated first derivative

- partial_der[l, ]: p estimated partial derivatives

```
