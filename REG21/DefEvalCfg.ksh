MEC_models=( BAM_20km BRAMS_08km Eta_08km GFS_30km WRF_07km WRFG_07km )  # Model names, to be evaluated
MEC_models_cmp=( BAM_20km BRAMS_08km Eta_08km WRF_07km )      # Model names, to be compared in SpBestOf
MEC_var=PREC                        # Variable to be evaluated
MEC_init_time=00                    # Model's initialization time, e.g. 00 | 12

MEC_valid_date1=20210122            # Valid dates (target) of the forecast, in format YYYYMMDD
MEC_valid_date2=20210131 

#MEC_grid_res=LOWEST                # Automatically chooses the lowest res between model x obs or
MEC_grid_res=MERGE_10.ctl           # a CTL filename (must be in ./TEMPLATE)

MEC_sp_domain=MCD  # Max Common Domain       # Spatial domain

