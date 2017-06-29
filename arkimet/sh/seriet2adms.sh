#/bin/bash
#
# Legge un file seriet e lo riscrive nel formato per ADMS
#
function write_help
{
#       123456789012345678901234567890123456789012345678901234567890123456789012345
echo "Uso: seriet2adms.sh filein fileout [-c] [-shf] [-h]"
echo "Legge un file seriet (LAMAZ) e lo riscrive nel formato per ADMS; l'estrazione"
echo "deve rispettare esattamente i templates"
echo "-c costruisce i templates per il progetto di estrazione (adms.akq e "
echo "   gacsv2seriet.nml) e termina"
echo "-shf: aggiunge all'output il campo SHF (default: T, UV, RAD)"

}
# Parametri da riga comando
demo="N"
shf="N"
mand_par=0
if [ $# -eq 0 ] ; then
  write_help
  exit 1
fi
while [ $# -ge 1 ] ; do
  if [ $1 = '-h' ] ; then
    write_help
    exit 1
  elif [ $1 = "-c" ] ; then
    demo="Y"
    shift
  elif [ $1 = "-shf" ] ; then
    shf="Y"
    shift
  elif [ $mand_par -eq 0 ] ; then
    filein=$1
    mand_par=1
    shift
  elif [ $mand_par -eq 1 ] ; then
    fileout=$1
    mand_par=2
    shift
  fi
done

# Costruisco i template per l'estrazione arkimet
if [ $demo = "Y" ] ; then
  cat <<EOF > adms.akq
reftime: =2016
product: swbudg or shf or alb
level: g00
#
! Parametri vicino alla superficie
reftime: =2016
product: t or u or v
level: g02 or g10
EOF

  cat <<EOF > gacsv2seriet.nml
&param
out_form     = 1,
out_ndec     = -2
qcont        = 2,
libsim       = T,
step_yy_mm   = T,
lab3d        = 2,
lab3ddec     = 1,
xls_dec_sep  = '.',
dir_int      = T,
tem_cel      = T,
mo_rec       = F,
sw_down      = T,
flx_rev      = F,
cc_fract     = F,
sca_ini      = F,
dos          = F
/
EOF
  exit
fi
  
# Tolgo header al file di input
rm -f tmp1.txt tmp2.txt tmp3.txt
rm -f yy.txt mm.txt day.txt jul.txt hh.txt tt.txt ff.txt dir.txt shf.txt rad.txt
tail -n +7 $filein > tmp1.txt

# Scrivo i parametri meteo in file separati
cut -c 55-61 tmp1.txt > tt.txt
cut -c 77-83 tmp1.txt > ff.txt
cut -c 66-72 tmp1.txt > dir.txt
cut -c 44-50 tmp1.txt > shf.txt
cut -c 33-39 tmp1.txt > rad.txt

# Costuisco la olonna del giorno giuliano
cut -c 7-10 tmp1.txt > yy.txt
cut -c 4-5 tmp1.txt > mm.txt
cut -c 1-2 tmp1.txt > day.txt
cut -c 12-13 tmp1.txt > hh.txt
paste -d : yy.txt mm.txt day.txt | sed 's/://g' > yyyymmdd.txt
while read date ; do
  date -d $date +%j >> jul.txt
done < yyyymmdd.txt

# Costruisco l'header di fileout 
if [ $shf = "N" ] ; then
  cat <<EOF > $fileout
VARIABLES:
7
YEAR
TDAY
THOUR
T0C
U
PHI
SOLAR RAD
DATA:
EOF

else
  cat <<EOF > $fileout
VARIABLES:
8
YEAR
TDAY
THOUR
T0C
U
PHI
SOLAR RAD
FTHETA0
DATA:
EOF

fi

# Costruisco il corpo di fileout
if [ $shf = "N" ] ; then
  paste -d , yy.txt jul.txt hh.txt tt.txt ff.txt dir.txt rad.txt > tmp2.txt
else
  paste -d , yy.txt jul.txt hh.txt tt.txt ff.txt dir.txt rad.txt shf.txt > tmp2.txt
fi

cat tmp2.txt | sed 's/-9999./ -999./g' > tmp3.txt
cat tmp3.txt >> $fileout
