# ==============================================================================
# Functions to evaluates statistics about weather forecast models.
# All functions are based on two vectors: vFCT=forecast and vOBS=observation.
# Follows WMO's recommendation.
# Author: ZR Garcia / DIMNT / CGCT / INPE
# ==============================================================================


# ==============================================================================
# Evaluates the Quantitative Precipitation Forecasts (QPFs) for continuous
# variables
# ------------------------------------------------------------------------------
# WMO HIGHLY RECOMMENDED: Mean obs and fct, sdev obs and fct, conditional median, 
#                         BIAS (ME), RMSE 
# WMO RECOMMENDED.......: MAE, MSE 
# All statistics followed by 95% CI
# ON-THE-FLY:
# . (Product moment) correlation coefficient (r) (***)
# . Spearman rank correlation coefficient (rs) (**)
# . MAE skill score (**) and MSE skill score (**)
# ==============================================================================
f.EvalCont = function(vOBS, vFCT) {
  res = list() # List of results

  # ------------------------------------------------------------------------------
  # Scores da observação
  #-------------------------------------------------------------------------------
  score = "OBS_AVG" ; res[[score]] = mean(vOBS)  # Average
  score = "OBS_SD"  ; res[[score]] = sd(vOBS)    # Standard deviation
  score = "OBS_VAR" ; res[[score]] = var(vOBS)   # Variance
  
  # ------------------------------------------------------------------------------
  # Avaliação da previsão
  # ------------------------------------------------------------------------------
  score = "FCT_AVG"; res[[score]] = mean(vFCT)   # Average
  
  # BIAS (additive) or ME (Mean Error)
  # -INF:+INF (perfect=0)
  score = "FCT_BIAS"
  res[[score]] = mean(vFCT - vOBS)  # igual a mean(vFCT) - mean(vOBS)
  
  # MAE (Mean Absolute Error) or additive bias
  # 0:+INF (perfect=0)
  score = "FCT_MAE"
  res[[score]] = mean(abs(vFCT - vOBS))
  
  # MSE (Mean Squared Error)
  # 0:+INF (perfect=0)
  score = "FCT_MSE"
  res[[score]] = mean((vFCT - vOBS)^2)
  
  # RMSE (Root Mean Squared Error)
  # 0:+INF (perfect=0)
  score = "FCT_RMSE"
  res[[score]] = sqrt(mean((vFCT - vOBS)^2))
  
  score = "FCT_SD"  ; res[[score]] = sd(vFCT)   # Standard Deviation

  # URMSE* (Unbiased Root Mean Squared Error normalized)
  score = "FCT_URMSE"
  if (sd(vOBS) == 0) {
    res[[score]] <-  sqrt((res$FCT_RMSE^2)-(res$FCT_BIAS^2))          # não normaliza por SD
    #cat("(1:", res[[score]], ")")
  } else {
    res[[score]] <-  sqrt((res$FCT_RMSE^2)-(res$FCT_BIAS^2))/sd(vOBS) # Normaliza
    #cat("(2:", res[[score]], ")")
  }
  score = "FCT_VAR" ; res[[score]] = var(vFCT)  # Variance
  res
}

# ==============================================================================
# Eval scores for spatial consolidated metrics (monthly, seasonally, ...)
# Does it an element-wise way so they are spatial metrics (per GP)
# ==============================================================================
f.EvalContCons = function(vOBS, vFCT) {
  res = list() # List of results
  # SBIAS (additive) or ME (Mean Error)
  # -INF:+INF (perfect=0)
  score = "SBIAS"
  res[[score]] = mean(vFCT - vOBS, na.rm=T)  # like mean(vFCT) - mean(vOBS)

  # COR (CORRELATION) 
  # 0:+INF (perfect=0)
  score = "SCORR"
  # Too much sd=0 occurs, can't calculate correlation
  res[[score]] = suppressWarnings({cor(vFCT, vOBS, use="pairwise.complete.obs")})
  
  # RMSE (Root Mean Squared Error)
  # 0:+INF (perfect=0)
  score = "SRMSE"
  res[[score]] = sqrt(mean((vFCT - vOBS)^2, na.rm=T))

  res
}

# ==============================================================================
# Gennerates the contingency table values
# Scores should be calculated on-the-fly
# ------------------------------------------------------------------------------
f.EvalCatCT = function(vOBS, vFCT, thres) {
  res = list() # List of results
    
  # Criar vFCT e vOBS antes
  fct.cat <- vFCT >= thres
  obs.cat <- vOBS >= thres

  res[["hits"]]    <- sum(fct.cat & obs.cat)    # a
  res[["fal_al"]]  <- sum(fct.cat & !obs.cat)   # b
  res[["miss"]]    <- sum(!fct.cat & obs.cat)   # c
  res[["cor_neg"]] <- sum(!fct.cat & !obs.cat)  # d
  tot = res$hits + res$fal_al + res$miss + res$cor_neg
  stopifnot(tot==length(fct.cat))

  # Ajusta os elementos zerados da TC para conterem 1
  # Retira do elemento maior
  TC = c(res$hits, res$fal_al, res$miss, res$cor_neg)
  if (length(which(TC==0))>0) {
     TC = f.TCAdj(TC)
     res[["hits"]]    = TC[1]
     res[["fal_al"]]  = TC[2]
     res[["miss"]]    = TC[3]
     res[["cor_neg"]] = TC[4]
     #cat("\n(", thres, "==>", hits, " ", fal_al, " ", miss, " ", cor_neg, ")", sep="")
  }
  res
}


# ==============================================================================
# Evaluates the Quantitative Precipitation Forecasts (QPFs) for categories
# For each threshold creates the contingency table and calculates the score.
# Follows WMO's recommendations.
# ------------------------------------------------------------------------------
# WMO HIGHLY RECOMMENDED: PC, FBIAS, POD, FAR, ETS    ==> all with 95% CI
# WMO RECOMMENDED.......: POFD, TS, HK, HSS, OR, ORSS ==> all with 95% CI
# Rain occurrence ≥ specific thresholds:
# - Rainfall thresholds = 1, 2, 5, 10, 15, 20, 50, 80, 120, 150, 200, 250
# - OBS: Recommended + customized

# Probability forecasts of rain meeting or exceeding specified thresholds 
# (e.g., 24h precipitation accumulation > 1.0 mm), the suggested scores are:
# BSS***, Reliability diagram***, ROC***, ROCA***, BS**
# ------------------------------------------------------------------------------
# They appear in alphabetical order
# Returns a list of the scores calculated for the threshold
# ==============================================================================
f.EvalCat = function(vOBS, vFCT, thres) {
  res = list() # List of results
    
  # Criar vFCT e vOBS antes
  fct.cat <- vFCT >= thres
  obs.cat <- vOBS >= thres

  hits    <- sum(fct.cat & obs.cat)    # a
  fal_al  <- sum(fct.cat & !obs.cat)   # b
  miss    <- sum(!fct.cat & obs.cat)   # c
  cor_neg <- sum(!fct.cat & !obs.cat)  # d
  tot     <- hits + fal_al + miss + cor_neg
  stopifnot(tot==length(fct.cat))

  # Ajusta os elementos zerados da TC para conterem 1
  # Retira do elemento maior
  TC = c(hits, fal_al, miss, cor_neg)
  if (length(which(TC==0))>0) {
     TC = f.TCAdj(TC)
     hits = TC[1]
     fal_al = TC[2]
     miss = TC[3]
     cor_neg = TC[4]
     #cat("\n(", thres, "==>", hits, " ", fal_al, " ", miss, " ", cor_neg, ")", sep="")
  }

  # ------------------------------------------------------------------------------
  # ETS (Equitable Threat Score) or GSS (Gilbert Skill Score)
  # ------------------------------------------------------------------------------
  # Shows: How well the forecast "yes" events correspond to the observed "yes" events 
  # (accounting for hits due to chance)?
  # Range: -1/3 to 1, 0 indicates no skill.  Perfect score: 1.
  Hrand = ((hits + miss) * 1.0 * (hits + fal_al)) / (1.0 * tot) # Hits random
  score = "ETS"
  res[[score]] = (hits-Hrand) / (hits+miss+fal_al-Hrand)

  # ------------------------------------------------------------------------------
  # FAR (False Alarme Ratio)
  # ------------------------------------------------------------------------------
  # Shows: Fraction of the predicted "yes" events that did not occur (false alarms)
  # Range: 0 to 1.  Perfect score: 0.
  score = "FAR"
  res[[score]] = fal_al / (hits + fal_al)

  # ------------------------------------------------------------------------------
  # BIAS SCORE (FREQUENCY BIAS)
  # ------------------------------------------------------------------------------
  # Shows: Fct "yes" compared to observed "yes"
  # Range: 0 to ∞.  Perfect score: 1.
  score = "FBIAS"
  res[[score]] = (hits + fal_al) / (hits + miss)

  # ------------------------------------------------------------------------------
  # HK (Hanssen and Kuipers discriminant) or 
  # TSS (True Skill Statistics) or
  # PSS (Peirce's Skill Score)
  # ------------------------------------------------------------------------------
  # Shows: How well the forecast separates the "yes" events from the "no" events
  # Range: -1 to 1, 0 indicates no skill. Perfect score: 1.
  #score = "HK"
  #res[[score]] = (hits/(hits+miss)) - (fal_al / (1.0 * (fal_al+cor_neg)))

  # ------------------------------------------------------------------------------
  # HSS (Heidke Skill Score)
  # ------------------------------------------------------------------------------
  # Shows: The correspondence between the accuracy of the forecast and to that of random chance
  # Range: -1 to 1, 0 indicates no skill.  Perfect score: 1.
  # Expected correct random = observed yes X forecast yes + observed no X forecast no
  #score = "HSS"
  #obs_yes = 1.0 * (hits + miss)
  #obs_no  = 1.0 * (fal_al + cor_neg)
  #fct_yes = 1.0 * (hits + fal_al)
  #fct_no  = 1.0 * (miss + cor_neg)
  #ExpCorRand = ((1.0 * obs_yes * fct_yes) + (1.0 * obs_no * fct_no)) / (1.0 * tot)
  #res[[score]] = (hits + cor_neg - ExpCorRand) / (tot - ExpCorRand)

  # ------------------------------------------------------------------------------
  # OR (Odds Ratio)
  # ------------------------------------------------------------------------------
  # Shows: The ratio of the odds of a correct "yes" forecast, to the odds of a wrong "yes" forecast
  # Odds ratio - Range: 0 to ∞, 1 indicates no skill. Perfect score: ∞
  #score = "OR"
  #res[[score]] = (hits * 1.0 * cor_neg) / (miss * 1.0 * fal_al)

  # ------------------------------------------------------------------------------
  # ORSS (Odds Ratio Skill Score)
  # ------------------------------------------------------------------------------
  # Shows: How was the improvement of the forecast over random chance
  # Range: -1 to 1, 0 indicates no skill. Perfect score: 1
  #score = "ORSS"
  #res[[score]] = ((hits * 1.0 * cor_neg) - (miss * 1.0 * fal_al)) / ((hits * 1.0 * cor_neg) + (miss * 1.0 * fal_al))

  # ------------------------------------------------------------------------------
  # PERCENT CORRECT (FRACTION CORRECT or ACCURACY)
  # ------------------------------------------------------------------------------
  # Shows: Overall fraction of the correct forecasts
  # Range: 0 to 1.  Perfect score: 1.
  score = "PC"
  res[[score]] = (hits + cor_neg) / tot

  # ------------------------------------------------------------------------------
  # POD (Prob of Detection) or HR (Hit Rate) or H
  # ------------------------------------------------------------------------------
  # Shows: Fraction of the observed "yes" events that were correctly forecast
  # Range: 0 to 1.  Perfect score: 1.
  score = "POD"
  res[[score]] = hits / (hits + miss)

  # ------------------------------------------------------------------------------
  # POFD (Prob of False Detection) or false alarme rate or F
  # ------------------------------------------------------------------------------
  # Shows: Fraction of the observed "no" events that were incorrectly forecast as "yes"
  # Range: 0 to 1.  Perfect score: 0.
  #score = "POFD"
  #res[[score]] = fal_al / (cor_neg + fal_al)

  # ------------------------------------------------------------------------------
  # SR (Success Ratio)
  # ------------------------------------------------------------------------------
  # Shows: Fraction of the forecast "yes" events that were correctly observed
  # Range: 0 to 1.  Perfect score: 1.
  score = "SR"
  res[[score]] = hits / (hits + fal_al)

  # ------------------------------------------------------------------------------
  # TS (Threat Score) or CSI (Critical Success Index)
  # ------------------------------------------------------------------------------
  # Shows: How well the forecast "yes" events correspond to the observed "yes" events
  # Range: 0 to 1, 0 indicates no skill. Perfect score: 1
  #score = "TS"
  #res[[score]] = hits / (hits + miss + fal_al)

  res
}


# ==============================================================================
# Functions to compute verification statistics for continuous and categorical
# to be applied within a bootstrap function.
# Arguments: "d" a data frame with columns named "OBS" and "FCT".
#            "i" indexes used with "boot()" indicating which sample points to 
#                include in a given replicate sample.
# ==============================================================================

# ------------------------------------------------------------------------------
# Booter for continuos variables
# Value returned: A numeric vector of the statistics for OBS and FCT
# ------------------------------------------------------------------------------
cont_booter <- function(d, i) {
  A <- f.EvalCont(d[i,"OBS"], d[i,"FCT"])
  res = c(A$OBS_AVG, A$OBS_SD, A$OBS_VAR, 
            A$FCT_AVG, A$FCT_BIAS, A$FCT_MAE, A$FCT_MSE, A$FCT_RMSE, A$FCT_SD, A$FCT_URMSE, A$FCT_VAR)
  names(res) = names(A)
  res
}

# Consolidated spatial version (monthly, seasonally, etc)
cont_booter_cons <- function(d, i) {
  A <- f.EvalContCons(d[i,"OBS"], d[i,"FCT"])
  res = c(A$SBIAS, A$SCORR, A$SRMSE)
  names(res) = names(A)
  res
}

# ------------------------------------------------------------------------------
# Booter for categorical variables
# Value returned: A numeric vector of the categorical statistics
# ------------------------------------------------------------------------------
cat_booter <- function(d, i, threshold) {
  A <- f.EvalCat(d[i,"OBS"], d[i,"FCT"], threshold)
  res = c(A$ETS, A$FAR, A$FBIAS, A$HK, A$HSS, A$OR, A$ORSS, A$PC, A$POD, A$POFD, A$SR, A$TS)
  names(res) = names(A)
  res
}

# ------------------------------------------------------------------------------
# Ajusta os elementos zerados da TC para conterem 1
# Retira do elemento maior
# ------------------------------------------------------------------------------
f.TCAdj = function(TC) {
  while (length(which(TC==0))>0) {
    elem_in = which(TC==0)[1]
    elem_out = which.max(TC)[1]
    TC[elem_in] = TC[elem_in]+1
    TC[elem_out] = TC[elem_out]-1
  }
  TC
}


# ==============================================================================
# This procedure implements David Olive's simple median confidence interval
# http://exploringdatablog.blogspot.com/2012/04/david-olives-median-confidence-interval.html
# Default to calculate 95% CI
# ==============================================================================
DOliveCIproc <- function(x, ci_level=95) {
  alpha = (100-ci_level)/100/2  # If ci_level == 90, alpha = 0.05

  # First, compute the median
  n = length(x)
  ysort = sort(x)
  nhalf = floor(n/2)
  if (2*nhalf < n) {          # n odd
    med = ysort[nhalf + 1]
  } else {                    # n even
    med = (ysort[nhalf] + ysort[nhalf+1])/2
  }

  # Next, compute Olive’s standard error for the median
  Ln = nhalf - ceiling(sqrt(n/4))
  Un = n - Ln
  SE = 0.5*(ysort[Un] - ysort[Ln+1])

  #  Compute the confidence interval based on Student’s t-distribution
  #  The degrees of freedom parameter p is discussed in Olive’s paper
  p = Un - Ln - 1
  t = qt(p = 1 - alpha/2, df = p)
  medLCI = med - t * SE
  medUCI = med + t * SE

  #  Finally, return a data frame with all of the results computed here
  RES  = data.frame(Median = med, LCI = medLCI, UCI = medUCI,
                        N = n, dof = p, tmedian = t, 
                        SEmedian = SE)
  RES
}

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

