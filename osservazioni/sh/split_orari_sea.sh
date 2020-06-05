#/bin/bash
#-------------------------------------------------------------------------------
# Divide un file con serie temporali in segmenti progressivi, ciascuno dei quali
# contiene i dati di una stagione.
# Utile ad esempio per studiare l'andamento stagionale delle rose dei venti
#
#                                              Verisone 2.0.1, Enrico 25/03/2020
#-------------------------------------------------------------------------------
set -x
#-------------------------------------------------------------------------------
# Scrive a schermo l'help della procedura
function write_help
{
#       123456789012345678901234567890123456789012345678901234567890123456789012345
  echo "Uso: split_orari_sea.sh filein formato"
  echo "formato (input): s (seriet), h (oss. orarie), d (oss. giornaliere) "
  echo "Le stagioni sono attribuite all'anno in cui cominciano"
  echo "NOTA: formati d,h non testati!"
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

sea_str=( djf mam jja son )
file_root=${filein%.*}
ext=${filein##*.}

# Variaibli per il ritaglio del file
if [ $format = "s" ] ; then
  nhead=6
  grep_str[0]="^...12|^...01|^...02"
  grep_str[1]="^...03|^...04|^...05"
  grep_str[2]="^...06|^...07|^...08"
  grep_str[3]="^...09|^...10|^...11"

elif [ $format = "h" -o $format = "d" ] ; then
  nhead=3
  grep_str[0]="^.....12|^.....01|^.....02"
  grep_str[1]="^.....03|^.....04|^.....05"
  grep_str[2]="^.....06|^.....07|^.....08"
  grep_str[3]="^.....09|^.....10|^.....11"
fi

nheadp1=$[$nhead+1]

# Separo header e dati
rm -f dati.dat header.dat
head -n $nhead $filein > header.dat
tail -n +$nheadp1 $filein > dati.dat

for id in 0 1 2 3 ; do
  fileout=${file_root}_${sea_str[$id]}.${ext}
  cp header.dat $fileout
  grep -E ${grep_str[$id]} dati.dat >> $fileout
done

exit
