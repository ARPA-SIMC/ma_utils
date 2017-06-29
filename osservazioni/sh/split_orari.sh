#/bin/bash
#-------------------------------------------------------------------------------
# Divide un file con serie temporali in segmenti, secondo un criterio (annuali,
# mensili, stagionali)
# Utile ad esempio per studiare l'andamento annuale delle rose dei ventia
#
#                                              Versione 1.0.0, Enrico 05/06/2017
#-------------------------------------------------------------------------------
#set -x
#-------------------------------------------------------------------------------
# Scrive a schermo l'help della procedura
function write_help
{
#       123456789012345678901234567890123456789012345678901234567890123456789012345
  echo "Uso: split_orari.sh filein formato periodo"
  echo "formato (input): s (seriet), h (oss. orarie), d (oss. giornaliere)"
  echo "perido (output): y (anno), m (mese)"
}

# Parametri da riga comando
if [ $# -eq 0 ] ; then
  write_help
  exit 1
fi

mand_par=0
while [ $# -ge 1 ] ; do
  if [ $1 = '-h' ] ; then
    write_help
    exit 1
  elif [ $mand_par -eq 0 ] ; then
    filein=$1
    mand_par=1
    shift
  elif [ $mand_par -eq 1 ] ; then
    format=$1
    mand_par=2
    shift
  elif [ $mand_par -eq 2 ] ; then
    period=$1
    mand_par=3
    shift
  fi
done 

if [ $mand_par -ne 3 -o \
     \( $format != "s" -a $format != "h" -a $format != "d" \) -o \
     \( $period != "y" -a $period != "m" \) ] ; then
  write_help
  exit 1
fi

# Variaibli per il ritaglio del file
if [ $format = "s" ] ; then
  nhead=6
  if [ $period = "y" ]  ; then
    ch1=7 ; ch2=10
  elif [ $period = "m" ]  ; then
    ch1=4 ; ch2=5
  fi
  
elif [ $format = "h" -o $format = "d" ] ; then
  nhead=3
  if [ $period = "y" ]  ; then
    ch1=1 ; ch2=4
  elif [ $period = "m" ]  ; then
    ch1=6 ; ch2=7
  fi

fi

nheadp1=$[$nhead+1]
ch1m1=$[$ch1-1]

# Suddivido header e dati
rm -f tmp.dat header.dat ${filein}.#*
head -n $nhead $filein > header.dat
tail -n +$nheadp1 $filein > tmp.dat

# Trovo la lista dei periodi presenti nel file
plist=$(cut -c ${ch1}-${ch2} tmp.dat | uniq)

# Scrivo un file per ciascun periodo
for period in $plist ; do
  fileout=${filein}.#${period}
  gex=^.{$ch1m1}${period}
  cp header.dat $fileout
  grep -E $gex tmp.dat >> $fileout
  echo "Scritto file "$fileout
done
