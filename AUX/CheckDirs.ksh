#
# Put here directories that are dependent on the valid date, only!
#

MEC_D_dat_model_grib=${MEC_D_root}/DATA_MODEL/GRIB/${MEC_val_yea}${MEC_val_mon}/${MEC_init_time}
MEC_D_dat_model_ncdf=${MEC_D_root}/DATA_MODEL/NCDF/${MEC_val_yea}${MEC_val_mon}/${MEC_init_time}
MEC_D_dat_obs_grib=${MEC_D_root}/DATA_OBS/GRIB/${MEC_val_yea}${MEC_val_mon}
MEC_D_dat_obs_ncdf=${MEC_D_root}/DATA_OBS/NCDF/${MEC_val_yea}${MEC_val_mon}
mkdir -p ${MEC_D_dat_obs_grib} ${MEC_D_dat_model_grib} ${MEC_D_dat_obs_ncdf} ${MEC_D_dat_model_ncdf}

