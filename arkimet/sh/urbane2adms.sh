#/bin/bash
#----------------------------------------------------------------------------------
# Legge un file di dati osservati relativi a una stazione urbana e lo riscrive nel
# formato per ADMS
#
#                                                 Versione 2.0.1, Enrico 07/06/2018
#----------------------------------------------------------------------------------
#set -x
function write_help
{
#       123456789012345678901234567890123456789012345678901234567890123456789012345
echo "Uso: urbane2adms.sh filein fileout [-h] [-c] [-prc]"
echo "Legge un file di dati osservati relativi a una stazione urbana e lo riscrive"
echo "nel formato per ADMS; l'estrazione deve rispettare esattamente il template"
echo "-c visualizza il comando di estrazione e termina"
echo "-prc: include la pioggia nel file di ouput"

}
# Parametri da riga comando
demo="N"
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
  elif [ $1 = "-prc" ] ; then
    prc="Y"
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

# Visualizza un esmepio di comando di estrazione
if [ $demo = "Y" ] ; then
  echo "/usr/libexec/ma_utils/estra_oss 20160101 20161231 H06034 158,165,166,160,409"
  echo ""
  echo "Codici delle stazioni urbane:"
  echo "H06030: Piacenza"
  echo "H06031: Parma"
  echo "H06032: Reggio nell'Emilia"
  echo "H06033: Modena"
  echo "H06034: Bologna"
  echo "H06035: Ferrara"
  echo "H06036: Ravenna"
  echo "H06037: Forli'"
  echo "H06038: Cesena"
  echo "H06039: Rimini"
  exit
fi
  
# Tolgo l'header al file di input
rm -f tmp?.txt
rm -f yy.txt mm.txt day.txt jul.txt hh.txt tt.txt ff.txt dir.txt rad.txt prc.txt
tail -n +4 $filein > tmp1.txt

# Scrivo i parametri meteo in file separati
cut -c 18-24 tmp1.txt > tt.txt
cut -c 29-35 tmp1.txt > dir.txt
cut -c 40-46 tmp1.txt > ff.txt
cut -c 51-57 tmp1.txt > prc.txt
cut -c 62-68 tmp1.txt > rad.txt

# Costuisco la colonna del giorno giuliano
cut -c 1-4 tmp1.txt > yy.txt
cut -c 6-7 tmp1.txt > mm.txt
cut -c 9-10 tmp1.txt > day.txt
cut -c 12-13 tmp1.txt > hh.txt
paste -d : yy.txt mm.txt day.txt | sed 's/://g' > yyyymmdd.txt
while read date ; do
  date -d $date +%j >> jul.txt
done < yyyymmdd.txt

# Converto la temperatura da K a C
mv tt.txt tmpt.txt
while read tk ; do
  if [ $tk != -9999.0 ] ; then
    tc=$(echo $tk'-273.1' | bc)
  else
    tc=-9999.
  fi
  echo $tc >> tt.txt
done < tmpt.txt

# Costruisco l'header di fileout 
nvar=7
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

if [ $prc = "Y" ] ; then
  echo "P" >> $fileout
fi
echo "DATA:" >> $fileout

# Costruisco il corpo di fileout
paste -d , yy.txt jul.txt hh.txt tt.txt ff.txt dir.txt rad.txt > tmp3.txt
if [ $prc = "Y" ] ; then
  paste -d , tmp3.txt prc.txt > tmp4.txt
else
  ln -s tmp3.txt tmp4.txt
fi

cat tmp4.txt | sed 's/-9999./ -999./g' > tmp5.txt
cat tmp5.txt >> $fileout
