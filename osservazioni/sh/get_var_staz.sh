#/bin/bash
#-------------------------------------------------------------------------------
# Interroga arkioss e ritorna la lista dei parametri disponibli per una stazione
# Todo: rendere un po' piu' "user friendly" l'output (es. nome in chiaro della
# variabilie fisica)
#
#                                              Versione 1.0.0, Enrico 26/05/2015
#-------------------------------------------------------------------------------
#set -x

function write_help
{
#       123456789012345678901234567890123456789012345678901234567890123456789012345678
  echo "Interroga arkioss e ritorna la lista dei parametri disponibli per una stazione"
  echo "Uso: get_var_staz.sh idstaz [data_ini data_fin]"
  echo "idstaz: id oracle della stazione (vedi file anag_arkioss.csv)"
  echo "data1, data2: intervallo di date in cui cercare (default: ultima settimana)"
}

#===============================================================================

# Assegno l'ambiente ma_utils
if [ -z $MA_UTILS_DAT ] ; then
  anag_arkioss=/usr/share/ma_utils/anag_arkioss.csv
else
  echo "(crea_anag_arkioss.ksh) Tabelle ma_utils: copia di lavoro in "$MA_UTILS_DAT
  anag_arkioss=${MA_UTILS_DAT}/anag_arkioss.csv
fi
akmurl="http://arkioss.metarpa:8090"

# Parametri da riga comando
idstaz=0
today=$(date +%Y%m%d)
data1=$(date -d "$today - 7 day" +%Y-%m-%d)
data2=$(date -d "$today - 1 day" +%Y-%m-%d)

idp=0
if [ $# -eq 0 ] ; then
  write_help
  exit 1
fi
while [ $# -ge 1 ] ; do
  if [ $1 = -h ] ; then
    write_help
    exit 1
  else
    if [ $idp -eq 0 ] ; then
      id_staz=$1
    elif [ $idp -eq 1 ] ; then
      data1=$1
    elif [ $idp -eq 2 ] ; then
      data2=$1
    fi
    idp=$[idp+1]
    shift
  fi
done

# Costruisco la query
rm -f gvs.query gvs.yml
str_reftime=">="$(date -d $data1 +%Y-%m-%d)", <="$(date -d $data2 +%Y-%m-%d)
str_sta="area: VM2,${id_staz}"

cat <<EOF > gvs.query
reftime: ${str_reftime}
$str_sta
EOF

# Estraggo e visualizzo la lista dei parametri
net=$(grep ^${id_staz} $anag_arkioss | cut -d , -f 2)
arki-query --summary --yaml --file=gvs.query ${akmurl}/dataset/${net} > gvs.yml
grep Product gvs.yml | sort -t "," -k 2,3
