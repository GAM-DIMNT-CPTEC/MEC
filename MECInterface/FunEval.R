# Function for CI of pearsons' correlation r are replicated from pkg psychometric
# library(psychometric)
# The following command calculates lower and upper 95% CI (level)
# assuming sample size (n) is 100 # and the obtained correlation (r) is 0.90
# CIr(r=.9, n = 100, level = .95)

#r=.8; n=100; level=.95
f.CIr = function(r, n, level=.95) {
  # Step 1) Convert r to z (normally distributed)
  # Fisher's transformation (Fisher, 1921) Assuming a bivariate normal population with
  # population correlation ρ, the transformation of the sample product moment correlation from r to z
  # Bivariate: correlation of fct and obs, p.e.
  z = 0.5 * log((1 + r)/(1 - r))
  
  # Step 2) Standard error of z (z-critical)
  SEz = 1 / sqrt(n - 3)
  
  # Step 3) CI limites of z
  noma <- 1 - level
  zs <- -qnorm(noma/2)
  MEz <- zs * SEz  # z's mean error
  low_z <- z - MEz
  upp_z <- z + MEz
  
  # Step 4) CI limites of z back to r
  low_r = (exp(2 * low_z) - 1)/(exp(2 * low_z) + 1)
  upp_r = (exp(2 * upp_z) - 1)/(exp(2 * upp_z) + 1)
  c(low_r, upp_r)
}
#f.CIr(.8, 100)
#CIr(.8, 100)
