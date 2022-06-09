# ==============================================================================
# JRM Garcia (roberto.garcia@inpe.br)
# DIMNT / CGCT / INPE / MCTI
# ------------------------------------------------------------------------------
# Preprocess SAMeT (download, regrid and adjust spatial domain)
# ==============================================================================
MEC_st_get_obs=0
MEC_fname_obs=SAMeT_${MEC_var}_${MEC_valid_date}
MEC_fname_obs_ori=SAMeT_CPTEC_${MEC_var}_${MEC_valid_date}

echo -n "* OBS: ${MEC_fname_obs_ori}: downloading, " 

wget -q -nc ftp.cptec.inpe.br/modelos/tempo/SAMeT/DAILY/${MEC_var}/${MEC_val_yea}/${MEC_val_mon}/${MEC_fname_obs_ori}.nc
if [[ ! -f ${MEC_fname_obs_ori}.nc ]]; then
  echo "  *** PROBLEMAS NO DOWLOAD DO NETCDF DO ${MEC_obs} ***  "
  MEC_st_get_obs=1
  return ${MEC_st_get_obs}
fi

MEC_var_lower=`echo "${MEC_var}" | tr '[:upper:]' '[:lower:]'`

echo -n "extracting var, "
cdo -s -selvar,${MEC_var_lower}  ${MEC_fname_obs_ori}.nc  ${MEC_fname_obs_ori}_adj.nc > ${MEC_D_log}/selvar_${MEC_obs}.log

echo -n "converting lon, "
ncap2 -O -s 'where(lon<0) lon=360+lon'  ${MEC_fname_obs_ori}_adj.nc  ${MEC_fname_obs_ori}_adj2.nc > ${MEC_D_log}/ncap2_${MEC_obs}.log

echo -n "regriding, "
cdo -s remapbil,${MEC_D_templ}/DEF_GRID_MERGE_10km.txt ${MEC_fname_obs_ori}_adj2.nc ${MEC_fname_obs_ori}_adj3.nc > ${MEC_D_log}/remapbil_${MEC_obs}.log

echo -n "renaming var, "
MEC_fname_obs="${MEC_fname_obs}00"
ncrename -O -v ${MEC_var_lower},${MEC_var} ${MEC_fname_obs_ori}_adj3.nc ${MEC_D_dat_obs_ncdf}/${MEC_fname_obs}.nc > ${MEC_D_log}/ncrename_${MEC_obs}.log

rm -f ${MEC_fname_obs_ori}*.nc

cd ${MEC_D_mec}
#echo "  ${MEC_fname_obs}.grib2 converted to ${MEC_fname_obs}.nc"
echo "Ok!"


