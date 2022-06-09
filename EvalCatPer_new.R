# ------------------------------------------------------------------------------
# CATEGORICAL EVALUATION uses raw FCT/OBS values and not the mean
# ***** use either CT or score version  ******
# ------------------------------------------------------------------------------
thres=thresholds[1]
for (thres in thresholds) {
  # ============================================================================
  # Contingency table version (add up all a, b, c and d of CT and save)
  # Calculus must be done in the UI (less disk usage, more time to show)
  # ============================================================================
  EV_CAT = f.EvalCatCT(vOBS, vFCT, thres)

  # Persisting the statistics of the QPF evaluation
  EVAL.CAT.PER[.(reg, mask, thres), c("HITS","FAL_AL","MISS","COR_NEG") := as.list(EV_CAT)]
  if (is.na(EVAL.CAT.PER[.(reg, mask, thres), HITS]) ||
        is.infinite(EVAL.CAT.PER[.(reg, mask, thres), HITS])) {
      cat("\n* ERR EvalCatPer_new.R:  metric=", st_name, "  OBS=", mean(vOBS), "  FCT=", mean(vFCT))
  }

  # ============================================================================
  # Score version (computes pre-established metrics and save)
  # UI must obly handle how to show (more disk usage, less time to show)
  # ============================================================================
  #EV_CAT = f.EvalCat(vOBS, vFCT, thres)
  #st_names = names(EV_CAT)
  #st_name=st_names[1]

  # Persisting the statistics of the QPF evaluation
  # TODO: less code buyt not working
  # EVAL.CAT[.(reg, mask, CFG$dt, CFG$hh, thres), (st_names) := as.numeric(EV_CAT)]
  # TRY 1: http://brooksandrew.github.io/simpleblog/articles/advanced-data-table/

  #for (st_name in st_names) {
  #  EVAL.CAT[.(reg, mask, CFG$dt, CFG$hh, thres)][[st_name]] = EV_CAT[[st_name]]
  #  if (is.na(EVAL.CAT[.(reg, mask, CFG$dt, CFG$hh, thres)][[st_name]]) ||
  #        is.infinite(EVAL.CAT[.(reg, mask, CFG$dt, CFG$hh, thres)][[st_name]])) {
  #      cat("\n* ERR EvalCat.R:  metric=", st_name, "  OBS=", mean(vOBS), "  FCT=", mean(vFCT))
  #  }
  #}
}

