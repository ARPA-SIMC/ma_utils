#/bin/bash
#----------------------------------------------------------------------------------
# Legge un file seriet e lo riscrive nel formato per ADMS
#
#                                                 Versione 2.2.0, Enrico 07/06/2018
#----------------------------------------------------------------------------------
#set -x
function write_help
{
#       123456789012345678901234567890123456789012345678901234567890123456789012345
echo "Uso: seriet2adms.sh filein fileout [-h] [-c] [-shf] [-prc filep]"
echo "Legge un file seriet (LAMAZ) e lo riscrive nel formato per ADMS; l'estrazione"
echo "deve rispettare esattamente i templates"
echo "-c costruisce i templates per il progetto di estrazione (adms.akq e "
echo "   gacsv2seriet.nml) e termina"
echo "-shf: aggiunge all'output il campo SHF (default: T, UV, RAD)"
echo "-prc filep: aggiunge la pioggia, estratta da una stazione"

}
# Parametri da riga comando
demo="N"
shf="N"
prc="N"
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
  elif [ $1 = "-prc" ] ; then
    prc="Y"
    shift
    filep=$1
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
rm -f tmp?.txt
rm -f yy.txt mm.txt day.txt jul.txt hh.txt tt.txt ff.txt dir.txt shf.txt rad.txt prc.txt
tail -n +7 $filein > tmp1.txt
nrec1=$()

# Scrivo i parametri meteo in file separati
cut -c 55-61 tmp1.txt > tt.txt
cut -c 77-83 tmp1.txt > ff.txt
cut -c 66-72 tmp1.txt > dir.txt
cut -c 44-50 tmp1.txt > shf.txt
cut -c 33-39 tmp1.txt > rad.txt

# Costuisco la colonna del giorno giuliano
cut -c 7-10 tmp1.txt > yy.txt
cut -c 4-5 tmp1.txt > mm.txt
cut -c 1-2 tmp1.txt > day.txt
cut -c 12-13 tmp1.txt > hh.txt
paste -d : yy.txt mm.txt day.txt | sed 's/://g' > yyyymmdd.txt
while read date ; do
  date -d $date +%j >> jul.txt
done < yyyymmdd.txt

# # Converto la temperatura da K a C (non necessario usando gacsv.nml!)
# mv tt.txt tmpt.txt
# while read tk ; do
#   if [ $tk != -9999.0 ] ; then
#     tc=$(echo $tk'-273.1' | bc)
#   else
#     tc=-9999.
#   fi
#   echo $tc >> tt.txt
# done < tmpt.txt

# Aggiungo uno zero decimale alla driezione del vento (ADMS non ama numeri come 201.)
rm -f tmpd1.txt tmpd2.txt dir.txt.org
mv dir.txt dir.txt.org
cut -c 2-6 dir.txt.org > tmpd1.txt
while read ff ; do
  echo "0" >> tmpd2.txt
done < dir.txt.org
paste -d . tmpd1.txt tmpd2.txt > dir.txt

# Se richiesto, tolgo le intestazioni dal file della pioggia
if [ $prc = "Y" ] ; then
  tail -n +4 $filep > tmp2.txt
  cut -c 18-24 tmp2.txt > prc.txt
  nrec_met=$(wc -l tmp1.txt | awk '{print $1}')
  nrec_prc=$(wc -l tmp2.txt | awk '{print $1}')
  if [ $nrec_met -ne $nrec_prc ] ; then
    echo "File meteo e prc hanno numero di record diversi! $nrec_met $nrec_prc"
    exit 2
  fi
fi
  
# Costruisco l'header di fileout 
nvar=7
if [ $shf = "Y" ] ; then
  nvar=$[$nvar+1]
fi
if [ $prc = "Y" ] ; then
  nvar=$[$nvar+1]
fi

cat <<EOF > $fileout
VARIABLES:
$nvar
YEAR
TDAY
THOUR
T0C
U
PHI
SOLAR RAD
EOF

if [ $shf = "Y" ] ; then
  echo "FTHETA0" >> $fileout
fi
if [ $prc = "Y" ] ; then
  echo "P" >> $fileout
fi
echo "DATA:" >> $fileout

# Costruisco il corpo di fileout
paste -d , yy.txt jul.txt hh.txt tt.txt ff.txt dir.txt rad.txt > tmp3.txt
if [ $shf = "Y" ] ; then
  paste -d , tmp3.txt shf.txt > tmp4.txt
else
  ln -s tmp3.txt tmp4.txt
fi
if [ $prc = "Y" ] ; then
  paste -d , tmp4.txt prc.txt > tmp5.txt
else
  ln -s tmp4.txt tmp5.txt
fi

cat tmp5.txt | sed 's/-9999./ -999./g' > tmp6.txt
cat tmp6.txt >> $fileout
