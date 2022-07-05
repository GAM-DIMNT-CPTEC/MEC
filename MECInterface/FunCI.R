

# =======================================================================================================
# qt()   - The Student t Distribution (small sample sizes, n < 30)
# The function below computes the CI based on the t distribution, it returns a data frame containing descriptive measures and the CI.
# https://www.r-bloggers.com/2021/04/calculating-confidence-interval-in-r/
# =======================================================================================================
# OU t.test(x)
f.CI_t <- function (x, ci = 0.95) {
  standard_deviation <- sd(x)
  Margin_Error <- qt(ci + (1 - ci)/2, df = length(x) - 1) * standard_deviation/sqrt(length(x))
  df_out <- data.frame( sample_size=length(x), Mean=mean(x), sd=standard_deviation,
                        Margin_Error=Margin_Error,
                        'CI lower limit'=(mean(x) - Margin_Error),
                        'CI Upper limit'=(mean(x) + Margin_Error))
  return(df_out)
}

# =======================================================================================================
# qnorm() - The Normal Distribution (for normal distribs and n > 30)
# The function below computes the CI based on the Z distribution, it also returns a data frame containing descriptive measures and the CI.
# https://www.r-bloggers.com/2021/04/calculating-confidence-interval-in-r/
# =======================================================================================================
f.CI_z <- function (x, ci = 0.95) {
  standard_deviation <- sd(x)
  sample_size <- length(x)
  Margin_Error <- abs(qnorm((1-ci)/2)) * standard_deviation/sqrt(sample_size)
  df_out <- data.frame( sample_size=length(x), Mean=mean(x), sd=standard_deviation,
                        Margin_Error=Margin_Error,
                        'CI lower limit'=(mean(x) - Margin_Error),
                        'CI Upper limit'=(mean(x) + Margin_Error))
  return(df_out)
}

# =======================================================================================================
# qchisq - The (non-central) Chi-Squared Distribution (X2)
# Chi square based CI
# http://daniellakens.blogspot.com/2019/07/calculating-confidence-intervals-around.html
# https://faculty.elgin.edu/dkernler/statistics/ch09/9-3.html
# =======================================================================================================
f.CI_c <- function (x, ci = 0.95) {
  standard_deviation = sd(x)
  alpha_level = 1 - ci #0.05 #set alpha level
  n = length(x) #set number of observations

  # calculate lower and upper critical values c_l and c_u
  # RETURNS THE AREA TO THE RIGHT OF CRITICAL VALUE?
  c_l <- sqrt((n - 1)/qchisq(alpha_level/2, n-1, lower.tail = FALSE))
  c_u <- sqrt((n - 1)/qchisq(alpha_level/2, n-1, lower.tail = TRUE))
  
  Margin_Error <- NA
  df_out <- data.frame( sample_size=n, Mean=mean(x), sd=standard_deviation,
                        Margin_Error=Margin_Error,
                        'CI lower limit'=(mean(x) * c_l),
                        'CI Upper limit'=(mean(x) * c_u))
  return(df_out)
}

#x=rnorm(100); ci=.95
#print(CI_z(x))
#t.test(x)$conf.int[1:2]
#print(CI_t(x))
#print(CI_c(x))

# http://www-stat.wharton.upenn.edu/~stine/stat621/lecture3.621.pdf
# Prediction intervals used to measure accuracy of predictions
# – Formed as predicted value (from equation) with margin for error set as
#   plus or minus twice the SD of the residuals, (predicted value) +/– 2 RMSE (in-sample only!)


# Under what assumptions does the sum of squared errors follow a generalized t distribution 
# (unintuitive to me, I feel like it should follow a ${\chi}^2$ distribution?
# from scipy import stats
# confidence = 0.95
# squared_errors = (final_predictions - y_test) ** 2 #y_test is real values, final_predictions is predicted values of y
# ci = np.sqrt(stats.t.interval(confidence, len(squared_errors) - 1, loc=squared_errors.mean(), scale=stats.sem(squared_errors)))

# RMSE is the mean of the residuals (that has been squared in order to account for negative values) and to obtain its CI 
# you need to use the distribution of residuals. You can get the residuals by subtracting the modelled values from the actual values. 
# You can then calculate the mean of the squared residual and the square root of that mean should yield the RMSE. 
# On this basis you can proceed to calculate the sd of residuals.
# I assume that the population variance of residuals is not known and thus your formula for calculating the CI would look like this:
# mean + t * sd/sqrt(df), where t is the upper (1-C)/2 critical value for the t distribution with n-1 degrees of freedom, t(n-1).
# So you have to look up your t-value, and include all other values in he formula and you are done.

# =======================================================================================================
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


