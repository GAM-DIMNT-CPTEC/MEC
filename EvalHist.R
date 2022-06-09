# ------------------------------------------------------------------------------
# HISTOGRAMS
# ------------------------------------------------------------------------------

# breaks with "higher" values in both sides
# In order to be stored, there are lenght(my.breaks)-1 bins
# expressed in the variable my.bins


# TYPE 1 OF CATEGORICAL VARIABLES: ---------------------------------------------
# "Exceeding specified thresholds", fct and obs >= one thresholds
# qtd intervals (bins) = qtd thresholds - 1
bin.counts = unlist(lapply(my.bins, function(x) sum(vFCT>=x)))
HIST[.(CFG$model, reg, mask, my.bins), FREQ := bin.counts]
HIST[.(CFG$model, reg, mask, my.bins), PERC := prop.table(bin.counts)*100]

bin.counts = unlist(lapply(my.bins, function(x) sum(vOBS>=x)))
HIST[.('OBS', reg, mask, my.bins), FREQ := bin.counts]
HIST[.('OBS', reg, mask, my.bins), PERC := prop.table(bin.counts)*100]

# TYPE 2 OF CATEGORICAL VARIABLES: ---------------------------------------------
# "Forecast of rain meeting", fct and obs within two thresholds 
# qtd intervals (bins) = qtd thresholds - 1
#
# Aborted here, I'll try to implement it via interface because I
# have to do COUNTS_BIN[i] - COUNTS_BIN[i+1], it saves file system
# space but overcharge processing and response
#
#bin.counts = hist(vFCT, breaks=my.breaks, right=F, plot=F)$counts
#HIST[.(CFG$model, reg, mask, my.bins), FREQ := bin.counts]
#HIST[.(CFG$model, reg, mask, my.bins), PERC := prop.table(bin.counts)*100]
#bin.counts = hist(vOBS, breaks=my.breaks, right=F, plot=F)$counts
#HIST[.('OBS', reg, mask, my.bins), FREQ := bin.counts]
#HIST[.('OBS', reg, mask, my.bins), PERC := prop.table(bin.counts)*100]


