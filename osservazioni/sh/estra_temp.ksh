#/bin/ksh
#-------------------------------------------------------------------------------
# Estrae una serie di radiosondaggi, relativi a una o piu' stazioni; converte i
# dati nel vecchio formato estra_temp.
#
#                                              Versione 1.0.0, Enrico 06/08/2014
#-------------------------------------------------------------------------------
#set -x

#-------------------------------------------------------------------------------
# Scrive a schermo l'help della procedura
function write_help
{
#       123456789012345678901234567890123456789012345678901234567890123456789012345
  echo "Uso: estra_temp.ksh data_ini data_end idstaz/ -lst filelst"
  echo "data_ini: prima data da estrarre  (AAAAMMGG)"
  echo "data_end: ultima data da estrarre (AAAAMMGG)"
  echo "idstaz:   id stazione da estrarre (es: 16144)"
  echo "filelst:  file con la lista delle stazioni da estrarre"
#       123456789012345678901234567890123456789012345678901234567890123456789012345
  return
}

#===============================================================================
# 1) Preliminari

#-------------------------------------------------------------------------------
# 1.1) Path e utility

akurl="http://arkimet.metarpa:8090"
dataset="gts_temp"

#-------------------------------------------------------------------------------
# 1.2 Parametri da riga comando

input=id
mand_par=0
if [ $# -eq 0 ] ; then
  write_help
  exit 1
fi
while [ $# -ge 1 ] ; do
  if [ $1 = '-h' ] ; then
    write_help
    exit 1
  elif [ $1 = '-list' ] ; then
    input="list"
    shift
    filelst=$1
    shift
  else
    if [ $mand_par -eq 0 ] ; then
      data1=$1
    elif [ $mand_par -eq 1 ] ; then
      data2=$1
    elif [ $mand_par -eq 2 ] ; then
      staz_list=$1
    fi
    mand_par=`expr $mand_par + 1`
    shift
  fi
done

if [ $input = "list" ] ; then
  if [ $mand_par != 2 ] ; then
    write_help
    exit 1
  fi
  staz_list=`cat $filelst`
  nstaz=`wc -l $filelst`

else
  if [ $mand_par != 3 ] ; then
    write_help
    exit 1
  fi
  nstaz=1
fi

# Altri preliminari
arki-mergeconf ${akurl}/dataset/${dataset} >> ${dataset}.conf

#===============================================================================
# 2) Elaborazioni (ciclo sulle stazioni)

echo "Elaboro i dati di "$nstaz" stazioni"

for idstaz in $staz_list ; do

  rm -f temp.query temp.bufr temp.csv

# Costruisco la query
  data1m1=`date -d "$data1 - 1day" +%Y%m%d`
  str_reftime=">="`date -d $data1m1 +%Y-%m-%d`" 22, <="`date -d $data2 +%Y-%m-%d`" 22"
  blo=`echo $idstaz | awk '{print substr($1,1,2)}'`
  sta=`echo $idstaz | awk '{print substr($1,3,3)}'`

  rm -f temp.query
  cat <<EOF > temp.query
  reftime: ${str_reftime}
  proddef: GRIB:blo=${blo},sta=${sta}
EOF

# Estraggo i dati e li converto nel formato estra_orari
  arki-query --data --config=${dataset}.conf --file=temp.query > temp.bufr
  dbamsg dump --type=bufr --csv temp.bufr > temp.csv

done

# grep -E "B01001| B01002|B05001|B06001|B07030|B04001|B04002|B04003|B04004|B07004|B10009|B12101|B12103|B11001|B11002" temp.csv > temp.csv.filter

