#/bin/bash
#-------------------------------------------------------------------------------
# Interroga arkioss e ritorna la lista dei parametri disponibli per una specifica
# stazione.
#
# Note:
# - Causa bug arkimet, le query con intervallo di date ritornano meno dati di 
#   quelle sull'intero dataset (non e' chiaro quale sia quella giusta...)
# - Questo scriupt dipende dal file anag_arkioss.scv, ma solo per trovare a 
#   quale dataset appartiene la stazione richiesta. Volendo potrebbe essere reso
#   piu' standard (ma piu' lento) cercando il codice stazione in tutti i dataset
#   disponibili.
# 
#                                              Versione 2.0.0, Enrico 03/06/2015
#-------------------------------------------------------------------------------
#set -x

function write_help
{
#       123456789012345678901234567890123456789012345678901234567890123456789012345678
  echo "Interroga arkioss e ritorna la lista dei parametri disponibli per una specifica"
  echo "stazione"
  echo "Uso: get_var_staz.sh id_staz [data_ini data_fin]"
  echo "id_staz: id oracle della stazione (vedi file anag_arkioss.csv in"
  echo "  /usr/share/ma_utils)"
  echo "data_ini, data_fin: intervallo di date in cui cercare (default: intero dataset)"
}

#===============================================================================
function parse_product
#
# Riformatta (da yaml a csv) la stringa "Product" estratta da arkioss e relativa
# a una stazione. 
#
# Note:
# - Questa funzione e' usata  anche da crea_anag_arkioss.sh; la funzione gemella
#   (parse_area) e' usata anche da get_staz_var.sh.
# - Per motivi misteriosi, sembra che il comando "print index" funzioni 
#   correttamente solo se la stringa $line vine riscritta (comando $line2)
#   In alternativa, sembra che funzioni anche modificando i comandi awk:
#   pp=$(echo $line | awk '{print index("'"${line2}"'","bcode=")}')

{

line2=$(echo $line | sed 's/"//g')

# id_var (Oracle)
id_var=$(echo $line|cut -d \( -f 2 | cut -d , -f 1 | cut -d \) -f 1)

# Bcode (codice parametro fisico)
pp=$(echo "" | awk '{print index("'"${line2}"'","bcode=")}')
if [ $pp -gt 0 ] ; then
  bcode=$(echo $line2 | cut -c $[pp+6]- | cut -d , -f 1)
else
  bcode="-9999"
fi

# Long Name (nome esteso variabile fisica, da dballe.txt)
bcode2=`echo $bcode | sed 's/B/0/'`
long_name=$(grep ^\ $bcode2 $dballe_txt | cut -c 9-72 | sed 's/ *$//g;s/,/-/g')

# Short Name (nome del parametro per output estra_orari, solo metamb)
str_dum=`grep ^$id_var $product_shortnames`
if [ $? -ne 0 ] ; then
  short_name="var"${id_var}
else
  short_name=$(echo $str_dum | cut -d , -f 3)
fi 

# Livello, timerange, unit
ltru=""
keys="l1 lt1 p1 p2 tr unit"
for key in $keys ; do
  idx=$(echo "" | awk '{print index("'"${line2}"'","'"${key}="'")}')
  if [ $idx -gt 0 ] ; then
    pp=$(expr length $key + $idx + 1)
    str_dum=$(echo $line | cut -c $pp-)
    ltru=${ltru}","$(echo $str_dum | cut -d , -f 1 | cut -d \) -f 1)
  else
    ltru=${ltru}","
  fi
done

# Compongo la stringa di output
str_product_csv=${id_var}","${bcode}","${long_name}${ltru}","${short_name}

}

################################################################################
# 1) Preliminari

# Parametri da riga comando
id_staz=0
today=$(date +%Y%m%d)
data1="nil"
data2="nil"

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

fileout=param_staz${id_staz}.csv

# URL dell'archivio arkioss
akurl="http://arkioss.metarpa:8090"

# Assegno l'ambiente ma_utils
if [ -z $MA_UTILS_DAT ] ; then
  product_shortnames=/usr/share/ma_utils/param_shortnames.csv
  anag_arkioss=/usr/share/ma_utils/anag_arkioss.csv
else
  echo "(get_var_staz.sh) Tabelle ma_utils: copia di lavoro in "$MA_UTILS_DAT
  product_shortnames=${MA_UTILS_DAT}/param_shortnames.csv
  anag_arkioss=${MA_UTILS_DAT}/anag_arkioss.csv
fi

# Assegno l'ambiente dballe
if [ -z $DBA_TABLES ] ; then
  dballe_txt=/usr/share/wreport/dballe.txt
else
  echo "(get_var_staz.sh) Tabelle dballe: copia di lavoro in "$DBA_TABLES
  dballe_txt=${DBA_TABLES}/dballe.txt
fi

# Costruisco la query arkioss
rm -f gvs.query
str_sta="area: VM2,${id_staz}"

if [ $data1 = "nil" -o $data2 = "nil" ] ; then
  echo "Cerco dati con reftime qualsiasi"
  cat <<EOF1 > gvs.query
  $str_sta
EOF1

else
  echo "Cerco dati con reftime tra "$data1" e "$data2
  str_reftime=">="$(date -d $data1 +%Y-%m-%d)", <="$(date -d $data2 +%Y-%m-%d)
  cat <<EOF2 > gvs.query
  reftime: ${str_reftime}
  $str_sta
EOF2

#===============================================================================
# 2) Elaborazioni

# Estraggo e visualizzo la lista dei parametri
rm -f $fileout tmp1.yml tmp2.yml
echo "id_var,bcode,long_name,l1,lt1,p1,p2,tr,unit,short_name" > \
  $fileout

net=$(grep ^${id_staz} $anag_arkioss | cut -d , -f 2)
arki-query --summary --yaml --file=gvs.query ${akurl}/dataset/${net} > tmp1.yml
grep Product tmp1.yml | sort -t "," -k 2,3 > tmp2.yml
while read line ; do
  parse_product
  echo $str_product_csv >> $fileout
done < tmp2.yml

nl=$(wc -l $fileout | awk '{print $1}')
echo "Totale parametri trovati: "$[$nl-1]
