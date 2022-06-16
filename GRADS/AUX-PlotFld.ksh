#!/bin/ksh
eval_name=${1}
model=${2}
var=${3}
dt=${4}
hs=${5}

if [[ ${var} == "PREC" ]]; then
  SRC=GRIB
  EXT=grib2
  mod_grb=`find /dados/dmdpesq/roberto.garcia/MEC_DATA/${eval_name}/DATA_MODEL/${SRC}/ -name "*${model}*${var}*${hs}*${dt}*.${EXT}" | head -1`
else
  SRC=NCDF
  EXT=nc
  mod_grb=""
fi

mod_ncdf=`find /dados/dmdpesq/roberto.garcia/MEC_DATA/${eval_name}/DATA_MODEL/NCDF/ -name "*${model}*${var}*${hs}*${dt}*.nc" | head -1`
obs=`find /dados/dmdpesq/roberto.garcia/MEC_DATA/${eval_name}/DATA_OBS/${SRC}/ -name "*${var}*${dt}*.${EXT}" | head -1`

rm -f OBSV.png MODG.png MODN.png

## --------------------------------------------------------------------------
## Plota o observado
## --------------------------------------------------------------------------
cp ${obs} .
obs=`basename ${obs}`
if [[ -z "${mod_grb}" ]]; then
  comm=sdfopen
  lin_dim=5
  lin_var=10
  wrd_var=4
  ctl=${obs}
else
  ctl="${obs%.*}.ctl"
  ./g2ctl ${obs} > ${ctl}   # Cria CTL
  gribmap -i ${ctl}         # Cria IDX
  comm=open
  lin_dim=4
  lin_var=9
  wrd_var=1
fi
src="${obs%.*}"
sed -e "s:CTL_FNAME:${ctl}:g"   -e "s:SRC:OBSV:g"            -e "s:open:${comm}:g"          \
    -e "s:LIN_DIM:${lin_dim}:g" -e "s:LIN_VAR:${lin_var}:g"  -e "s:WRD_VAR:${wrd_var}:g"    \
    -e "s:TITLE:${obs}:g"                                                                   \
     GS_PlotFld_template.gs > GS_PlotFld.gs

grads -blc "run GS_PlotFld.gs"     # gera a img
rm -f ${src}*.ctl ${src}*.grib2 ${src}*.grib2.idx ${src}*.nc

# --------------------------------------------------------------------------
# Plota o campo do modelo acumulado em grib 
# --------------------------------------------------------------------------
if [[ ! -z "${mod_grb}" ]]; then
  cp ${mod_grb} .
  mod_grb=`basename ${mod_grb}`
  ctl="${mod_grb%.*}.ctl"
  src="${mod_grb%.*}"
  ./g2ctl ${mod_grb} > ${ctl}   # Cria CTL
  gribmap -i ${ctl}         # Cria IDX
  sed -e "s:CTL_FNAME:${ctl}:g"   -e "s:SRC:MODG:g"            -e "s:open:${comm}:g"          \
      -e "s:LIN_DIM:${lin_dim}:g" -e "s:LIN_VAR:${lin_var}:g"  -e "s:WRD_VAR:${wrd_var}:g"    \
      -e "s:TITLE:${mod_grb}:g"                                                               \
  GS_PlotFld_template.gs > GS_PlotFld.gs
  grads -blc "run GS_PlotFld.gs"     # gera a img
  rm -f ${src}*.ctl ${src}*.grib2 ${src}*.grib2.idx
fi

# --------------------------------------------------------------------------
# Plota o campo do modelo acumulado em netcdf
# --------------------------------------------------------------------------
comm=sdfopen
lin_dim=5
lin_var=10
wrd_var=4
cp ${mod_ncdf} .
mod_ncdf=`basename ${mod_ncdf}`
src="${mod_ncdf%.*}"
sed -e "s:CTL_FNAME:${mod_ncdf}:g"  -e "s:SRC:MODN:g"            -e "s:open:${comm}:g"          \
    -e "s:LIN_DIM:${lin_dim}:g"     -e "s:LIN_VAR:${lin_var}:g"  -e "s:WRD_VAR:${wrd_var}:g"    \
    -e "s:TITLE:${mod_ncdf}:g"                                                                  \
GS_PlotFld_template.gs > GS_PlotFld.gs
grads -blc "run GS_PlotFld.gs"     # gera a img
rm -f ${mod_ncdf}

#montage -size 400x300 -tile 3x ????.png -geometry +1+1 -frame 1 COMP.jpeg; display COMP.jpeg
montage OBSV.png MOD?.png -geometry 500x600+1+1 -frame 1 COMP.jpeg; display COMP.jpeg

	   
