./GetREG21.ksh
#cp REG21P/IMG/* REG21/IMG
#cp REG21T/IMG/* REG21/IMG
#cp REG21UVP/IMG/* REG21/IMG
Rscript JoinREG21.R

cd REG21
rsync -ruv --files-from=<(find . -name "*WRF*") ./ ../MEC_WRF
rsync -ruv --files-from=<(find . -name "*BAM*") ./ ../MEC_WRF
rsync -ruv --files-from=<(find . -name "*GFS*") ./ ../MEC_WRF
cd ..

# Apaga para não consumir espaço no deploy
find REG21 -name "*WRFG*" -delete
find REG21 -name "*D-?_*" -delete

