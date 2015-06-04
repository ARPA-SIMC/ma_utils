#/bin/bash
#-------------------------------------------------------------------------------
# Interroga arkioss e ritorna la lista delle stazioni che misurano uno specifico 
# parametro.
#
# NOTE:
# - Causa bug arkimet, le query con intervallo di date ritornano meno dati di 
#   quelle sull'intero dataset (non e' chiaro quale sia quella giusta...)
#
#                                              Versione 2.0.0, Enrico 03/06/2015
#-------------------------------------------------------------------------------
#set -x

#===============================================================================
function write_help
{
#       123456789012345678901234567890123456789012345678901234567890123456789012345678
  echo "Interroga arkioss e ritorna la lista delle stazioni che misurano uno specifico"
  echo "parametro"
  echo "Uso: get_var_staz.sh id_var [data_ini data_fin]"
  echo "id_var: id oracle del parametro (vedi files param_arkioss.csv e "
  echo "  param_shortnames.csv, in /usr/share/ma_utils)"
  echo "data_ini, data_fin: intervallo di date in cui cercare (default: intero dataset)"
}

#===============================================================================
function intfill
{
#----------------------------------------------------------------------
# USO: intfill str_in len_req
# Ritorna la variabile d'ambiente str_out, che contiene len_req
# caratteri, di cui i primi sono "0" e gli ulitmi conicidono con str_in
#----------------------------------------------------------------------

  str_in=$1
  len_req=$2

  len_in=`echo $str_in | awk '{print length($1)}'`
  nzeri=`expr $len_req - $len_in`

  str_out=""
  cnt_private=0
  while [ $cnt_private -lt $nzeri ] ; do
    str_out=${str_out}"0"
    cnt_private=`expr $cnt_private + 1`
  done
  str_out=${str_out}${str_in}
}
#===============================================================================
function parse_area
#
# Riformatta (da yaml a csv) la stringa "Area" estratta da arkioss e relativa
# a una stazione.
#
# Note:
# - Questa funzione e' usata  anche da crea_anag_arkioss.sh; la funzione gemella
#   (parse_product) e' usata anche da get_var_staz.sh.

{
line2=$(echo $line | sed 's/"//g')

# id_stax (Oracle)
pp=$(echo "" | awk '{print index("'"${line2}"'","(")}')
id_staz=$(echo $line2 | cut -c $[pp+1]- | cut -d , -f 1)

# Coordinate (in archvio in gradi*10^5)
pp=$(echo "" | awk '{print index("'"${line2}"'","lon=")}')
dummy=$(echo $line2 | cut -c $[pp+4]- | cut -d , -f 1)
intfill $dummy 7
lon=$(echo $str_out | awk '{print substr($1,1,2) "." substr($1,3,5)}')

pp=$(echo "" | awk '{print index("'"${line2}"'","lat=")}')
dummy=$(echo $line2 | cut -c $[pp+4]- | cut -d , -f 1)
intfill $dummy 7
lat=$(echo $str_out | awk '{print substr($1,1,2) "." substr($1,3,5)}')

# Quota (in archvio in dm)
pp=$(echo "" | awk '{print index("'"${line2}"'","B07030=")}')
dummy=$(echo $line2 | cut -c $[pp+7]- | cut -d , -f 1)
quo=$[$dummy/10]

# Nome 
pp=$(echo "" | awk '{print index("'"${line2}"'","B01019=")}')
nome=$(echo $line2 | cut -c $[pp+7]- | cut -d , -f 1)

str_area_csv=${id_staz},${net},${lon},${lat},${quo},${nome}
}

################################################################################
# 1) Preliminari

# Parametri da riga comando
id_var=0
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
      id_var=$1
    elif [ $idp -eq 1 ] ; then
      data1=$1
    elif [ $idp -eq 2 ] ; then
      data2=$1
    fi
    idp=$[idp+1]
    shift
  fi
done

fileout=anag_var${id_var}.csv

# URL dell'archivio arkioss
akurl="http://arkioss.metarpa:8090"

# Costruisco la query arkioss
rm -f gsv.query
str_var="product: VM2,${id_var}"

if [ $data1 = "nil" -o $data2 = "nil" ] ; then
  echo "Cerco dati con reftime qualsiasi"
  cat <<EOF1 > gsv.query
  $str_var
EOF1

else
  echo "Cerco dati con reftime tra "$data1" e "$data2
  str_reftime=">="$(date -d $data1 +%Y-%m-%d)", <="$(date -d $data2 +%Y-%m-%d)
  cat <<EOF2 > gsv.query
  reftime: ${str_reftime}
  $str_var
EOF2

fi

#===============================================================================
# 2) Elaborazioni

#-------------------------------------------------------------------------------
# 2.1 Trovo l'elenco dei dataset presenti su arkioss
rm -f tmp.ds ds.lst
unset http_proxy
curl $akurl 2>/dev/null > tmp.ds
grep /dataset/ tmp.ds | cut -d / -f 3 | cut -d \' -f 1 | grep -v error > ds.lst

#-------------------------------------------------------------------------------
# 2.2 Per ciasun dataset, trovo le stazioni che misurano il parametro richiesto

rm -f $fileout
echo "id_staz,dset,lon,lat,quota,nome" > $fileout

cnt_net=0
while read net ; do
  rm -f tmp1.yml tmp2.yml
  arki-query --summary --yaml --file=gsv.query ${akurl}/dataset/${net} > tmp1.yml
  nc=$(wc -m tmp1.yml | awk '{print $1}')
  if [ $nc -gt 1 ] ; then
    echo "Trovati dati nel dataset "$net
    cnt_net=$[$cnt_net+1]
    grep Area tmp1.yml | sort -u > tmp2.yml
    while read line ; do
      parse_area $line
      echo $str_area_csv >> $fileout
    done < tmp2.yml
  fi
done < ds.lst

nl=$(wc -l $fileout | awk '{print $1}')
echo "Totale stazioni trovate: "$[$nl-1]" in "$cnt_net" reti"
