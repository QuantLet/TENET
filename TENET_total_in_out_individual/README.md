
[<img src="https://github.com/QuantLet/Styleguide-and-Validation-procedure/blob/master/pictures/banner.png" alt="Visit QuantNet">](http://quantlet.de/index.php?p=info)

## [<img src="https://github.com/QuantLet/Styleguide-and-Validation-procedure/blob/master/pictures/qloqo.png" alt="Visit QuantNet">](http://quantlet.de/) **TENET_total_in_out_individual** [<img src="https://github.com/QuantLet/Styleguide-and-Validation-procedure/blob/master/pictures/QN2.png" width="60" alt="Visit QuantNet 2.0">](http://quantlet.de/d3/ia)

```yaml

Name of QuantLet: TENET_total_in_out_individual

Published in: TENET

Description: 'Ranks the total incoming and outgoing links for each individual 
firm, and lists the first three most influential firms with respect to incoming 
and outgoing links for each firm'

Keywords: 'tail, quantile regression, CoVaR, systemic Risk, variable selection,
dimension reduction'

Author: Lining Yu

Submitted:

Datafile: tot_c_overtime.csv

```


```r

# clear all variables
rm(list = ls(all = TRUE))
graphics.off()
# sets the working directory setwd('C:/...')

# reads the total connecteness matrix aggregated over windows
tot.ct = as.matrix(read.csv("tot_c_overtime.csv"))

# Part 1: the individual incoming links 

# the total incoming links for each firm
in_tot = rowSums(tot.ct)
names(in_tot) = colnames(tot.ct)
# sorts the total incoming links for each firm
sort(in_tot, decreasing = TRUE)

# the first 3 most influential incoming links for each firm
for (i in 1:100) {
  print(i)
  in_firms = tot.ct[i, ]
  names(in_firms) = colnames(tot.ct)
  # each firm receives the links from others
  print(sort(in_firms, decreasing = TRUE)[1:3])
}

# Part 2: the individual outgoing links 

# the total outgoing links for each firm
out_tot = colSums(tot.ct)
names(out_tot) = colnames(tot.ct)
# sorts the total outgoing links for each firm
sort(out_tot, decreasing = TRUE)

# the first 3 most influential outgoing links for each firm
for (i in 1:100) {
  print(i)
  out_firms = tot.ct[, i]
  names(out_firms) = colnames(tot.ct)
  # each firm emits the links to others
  print(sort(out_firms, decreasing = TRUE)[1:3])
}

```
