#/bin/bash
#-------------------------------------------------------------------------------
# Divide un file con serie temporali in segmenti progressivi, ciascuno dei quali
# contiene i dati di una stagione.
# Utile ad esempio per studiare l'andamento stagionale delle rose dei venti
#
#                                              Verisone 1.0.0, Enrico 05/06/2017
#-------------------------------------------------------------------------------
#set -x
#-------------------------------------------------------------------------------
# Scrive a schermo l'help della procedura
function write_help
{
#       123456789012345678901234567890123456789012345678901234567890123456789012345
  echo "Uso: split_orari_sea.sh filein formato"
  echo "formato (input): s (seriet), h (oss. orarie), d (oss. giornaliere) "
  echo "Le stagioni sono attribuite all'anno in cui cominciano"
  echo "NOTA: formati s,h non testati! Il 29/02 viene escluso dai conti..."
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
  fi
done 

if [ $mand_par -ne 2 -o \
     \( $format != "s" -a $format != "h" -a $format != "d" \) ] ; then
  write_help
  exit 1
fi

# Variaibli per il ritaglio del file
if [ $format = "s" ] ; then
  nhead=6
  ch1=7 ; ch2=10
elif [ $format = "h" -o $format = "d" ] ; then
  nhead=3
  ch1=1 ; ch2=4
fi

nheadp1=$[$nhead+1]
ch1m1=$[$ch1-1]

# Suddivido header e dati
rm -f tmp.dat header.dat ${filein}.#*
head -n $nhead $filein > header.dat
tail -n +$nheadp1 $filein > tmp.dat

# Trovo la lista degli anni presenti nel file
plist=$(cut -c ${ch1}-${ch2} tmp.dat | uniq)

#
for year in $plist ; do
  for sea in mam jja son djf ; do
    if [ $sea = "mam" ] ; then
      mm1="03"
      mm2="05"
      dd2="31"
    elif [ $sea = "jja" ] ; then
      mm1="06"
      mm2="08"
      dd2="31"
    elif [ $sea = "son" ] ; then
      mm1="09"
      mm2="11"
      dd2="30"
    elif [ $sea = "djf" ] ; then
      mm1="12"
      mm2="02"
      dd2="28"
    fi
      
    if [ $format = "s" ] ; then
      data1="01/"$mm1"/"$year" 00"
      data2=$dd2"/"$mm1"/"$year" 00"
    elif [ $format = "h" ] ; then
      data1=$year" "$mm1" 01 00"
      data2=$year" "$mm1" 01 00"
    elif [ $format = "d" ] ; then
      data1=$year" "$mm1" 01"
    fi
	
  done
done 
