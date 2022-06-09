# ==============================================================================
# JRM Garcia (roberto.garcia@inpe.br)
# DIMNT / CGCT / INPE / MCTI
# ------------------------------------------------------------------------------
# Preprocess MERGE (download, regrid and adjust spatial domain)
# ==============================================================================
MEC_st_get_obs=0
MEC_fname_obs=MERGE_${MEC_var}_${MEC_valid_date}
MEC_fname_obs_ori=MERGE_CPTEC_${MEC_valid_date}

echo -n "* OBS: ${MEC_fname_obs_ori}: downloading, " 

wget -q -nc  ftp.cptec.inpe.br/modelos/tempo/MERGE/GPM/DAILY/${MEC_val_yea}/${MEC_val_mon}/${MEC_fname_obs_ori}.grib2
if [[ ! -f ${MEC_fname_obs_ori}.grib2 ]]; then
  echo "  *** PROBLEMAS NO DOWLOAD DO GRIB DO ${MEC_obs} ***  "
  MEC_st_get_obs=1
  return ${MEC_st_get_obs}
fi
wget -q -nc  ftp.cptec.inpe.br/modelos/tempo/MERGE/GPM/DAILY/${MEC_val_yea}/${MEC_val_mon}/${MEC_fname_obs_ori}.ctl
if [[ ! -f ${MEC_fname_obs_ori}.ctl ]]; then
  echo "  *** PROBLEMAS NO DOWLOAD DO CTL DO ${MEC_obs} ***  "
  MEC_st_get_obs=2
  return ${MEC_st_get_obs}
fi

# Cria um CTL temporario para acessar o dado original
echo -n "reggriding, "
cat ${MEC_fname_obs_ori}.ctl | sed "s@\^@@g" | sed "s@\-120.05@239.95@g" > ${MEC_fname_obs}.ctl
gribmap -i ${MEC_fname_obs}.ctl > ${MEC_D_log}/gribmap_merge_ori.log    # Cria IDX do MERGE original
rm ${MEC_fname_obs_ori}.ctl
mv ${MEC_fname_obs}.*     ${MEC_D_temp}/
mv ${MEC_fname_obs_ori}.* ${MEC_D_temp}/

sed -e "s:CTL_MERGE:${MEC_fname_obs}.ctl:g"  -e "s:DIR_OUT:.:g"                  \
    -e "s:LON:${MEC_MCD_lon}:g"              -e "s:LAT:${MEC_MCD_lat}:g"         \
    -e "s:OBS_NEW:${MEC_fname_obs}12:g"      -e "s:dt_obs:${MEC_valid_date}12:g" \
    -e "s:FCTNEW:V${MEC_var}:g"                                                   \
    ${MEC_D_grads}/GS_RecortaMERGE_template.gs > ${MEC_D_temp}/GS_RecortaMERGE.gs

# Adding valid time to observed
MEC_fname_obs=${MEC_fname_obs}12
MEC_ctl_obs=${MEC_D_temp}/${MEC_fname_obs}.ctl

cd ${MEC_D_temp}
grads -blc "run GS_RecortaMERGE.gs" > ${MEC_D_log}/grads_RecortaMERGE.log
./g2ctl ${MEC_fname_obs}.grib2 > ${MEC_fname_obs}.ctl                     # Cria CTL do MERGE recortado
gribmap -i ${MEC_fname_obs}.ctl > ${MEC_D_log}/gribmap_merge_rec.log      # Cria IDX do MERGE recortado

rm -f ${MEC_fname_obs_ori}.*

echo -n "converting to NetCDF, "
# Convert to NetCDF
cdo -s -f nc -selvar,prec ${MEC_fname_obs}.grib2 ${MEC_fname_obs}.nc
ncrename -O -v prec,PREC ${MEC_fname_obs}.nc ${MEC_D_dat_obs_ncdf}/${MEC_fname_obs}.nc > ${MEC_D_log}/ncrename_${MEC_obs}.log
rm -f ${MEC_fname_obs}.nc

cd ${MEC_D_mec}
#echo "  ${MEC_fname_obs}.grib2 converted to ${MEC_fname_obs}.nc"
echo "Ok!"

