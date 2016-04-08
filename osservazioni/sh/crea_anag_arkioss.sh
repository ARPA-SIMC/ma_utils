#/bin/bash
#-------------------------------------------------------------------------------
# Estre da arkioss le informazioni relative alle stazioni e ai prarametri 
# presenti in ciascuna rete, e le scrive in due files .csv
#
# NOTE:
# - Nei files di output, id_staz e' composto come: Hxxxxx, dove xxxxx e' il 
#   codice Oracle della stazione, sempre a 5 cifre; il carattere "H" serve a 
#   evitare conflitti con i codici WMO delle stazioni GTS.
# - Causa bug arkimet, le query con intervallo di date ritornano meno dati di 
#   quelle sull'intero dataset (non e' chiaro quale sia quella giusta...)
# - Estremi zoom: per BPA "6 43.5 14 47", per EmR "9.2 43.8 13 45.2"
#
#                                              Versione 3.0.0, Enrico 08/04/2016
#-------------------------------------------------------------------------------
#set -x

#===============================================================================
function write_help
{
#       123456789012345678901234567890123456789012345678901234567890123456789012345678
  echo "Costruisce la lsita completa di parametri e stazioni presenti in arkioss"
  echo "Uso: crea_anag_arkioss [-z xmin ymin xmax ymax] [-d data_ini data_fin]"
  echo "     [-h] [-ope] [-url URL]"
  echo "xmin ymin xmax ymax: estremi dell'aera geografica di ricerca (def: ovunque)"
  echo "data_ini, data_fin: intervallo di date in cui cercare (def: qualsiasi data)"
  echo "-ope: aggiorna l'anagrafica in /home/eminguzzi/svn/ma_utils/data"
  echo "URL: indirizzo del server arkioss (def: http://arkioss4.metarpa:8090)"
}

#===============================================================================
function intfill
{
#----------------------------------------------------------------------
# USO: intfill str_in len_req
# Ritorna la variabile d'ambiente str_out, che contiene len_req
# caratteri, di cui i primi sono "0" e gli ulitmi conicidonocon str_in
#----------------------------------------------------------------------

  str_in=$1
  len_req=$2

  len_in=`echo $str_in | awk '{print length($1)}'`
  nzeri=$[${len_req}-${len_in}]

  str_out=""
  cnt_private=0
  while [ $cnt_private -lt $nzeri ] ; do
    str_out=${str_out}"0"
    cnt_private=$[$cnt_private+1]
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
# - Funzione usata anche da get_staz_var.sh
# - Se il record di anagrafica e' incompleto, i campi mancanti sono messi a ""

{
line2=$(echo $line | sed 's/"//g')
id_staz=""
lon=""
lat=""
quo=""
nome=""

# id_staz (Oracle)
pp=$(echo "" | awk '{print index("'"${line2}"'","(")}')
if [ $pp -gt 0 ] ; then
  id_staz_row=$(echo $line2 | cut -c $[pp+1]- | cut -d , -f 1 | cut -d \) -f 1)
  intfill $id_staz_row 5
  id_staz="H"${str_out}
fi

# Coordinate (in archvio in gradi*10^5)
pp=$(echo "" | awk '{print index("'"${line2}"'","lon=")}')
if [ $pp -gt 0 ] ; then
  lon_raw=$(echo $line2 | cut -c $[pp+4]- | cut -d , -f 1 | cut -d \) -f 1)
  intfill $lon_raw 7
  lon=$(echo $str_out | awk '{print substr($1,1,2) "." substr($1,3,5)}')
fi

pp=$(echo "" | awk '{print index("'"${line2}"'","lat=")}')
if [ $pp -gt 0 ] ; then
  lat_raw=$(echo $line2 | cut -c $[pp+4]- | cut -d , -f 1 | cut -d \) -f 1)
  intfill $lat_raw 7
  lat=$(echo $str_out | awk '{print substr($1,1,2) "." substr($1,3,5)}')
fi

# Cerco la stazione in anag_meteozen.dat; verifico che le coordinate 
# corrispondano; leggo quota e nome stazione.

line3=$(grep ^:${id_staz_row}, anag_meteozen.dat)
if [ $? -eq 0 ] ; then
  line4=$(echo $line3 | sed 's/"//g')

# Verifico coordinate
  pp=$(echo "" | awk '{print index("'"${line4}"'","lon:")}')
  if [ $pp -gt 0 ] ; then
    lon_mz=$(echo $line4 | cut -c $[pp+4]- | cut -d , -f 1 | cut -d \) -f 1)
  fi

  pp=$(echo "" | awk '{print index("'"${line4}"'","lat:")}')
  if [ $pp -gt 0 ] ; then
    lat_mz=$(echo $line4 | cut -c $[pp+4]- | cut -d , -f 1 | cut -d \) -f 1)
  fi
  
# Cerco nome e quota
  if [ $lon_mz = $lon_raw -a $lat_mz = $lat_raw ] ; then
    pp=$(echo "" | awk '{print index("'"${line4}"'","name:")}')
    if [ $pp -gt 0 ] ; then
      nome=$(echo $line4 | cut -c $[pp+5]- | cut -d , -f 1 | cut -d \) -f 1)
    fi
  
    pp=$(echo "" | awk '{print index("'"${line4}"'","height:")}')
    if [ $pp -gt 0 ] ; then
      quo=$(echo $line4 | cut -c $[pp+7]- | cut -d , -f 1 | cut -d \) -f 1)
    fi

  else
    echo "Stazione $id_staz $net: coordinate inconsistenti in anag_meteozen.dat"
    echo "  attese "$lon_raw $lat_row" trovate "$lon_mz $lat_mz
  fi

else
  echo "Stazione $id_staz $net non trovata in anag_meteozen.dat"
fi

str_area_csv=${id_staz},${lat},${lon},${quo},${net},${nome}

}

#===============================================================================
function parse_product
#
# Riformatta (da yaml a csv) la stringa "Product" estratta da arkioss e relativa
# a una stazione. 
#
# Note:
# - Questa funzione e' usata  anche da get_var_staz.sh
# - Per motivi misteriosi, sembra che il comando "print index" funzioni 
#   correttamente solo se la stringa $line vine riscritta (comando $line2)
#   In alternativa, pare che si possa usare $line, modificando i comandi awk:
#     pp=$(echo $line | awk '{print index("'"${line2}"'","bcode=")}')

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
akurl="http://arkioss4.metarpa:8090"
data_restrict="N"
area_restrict="N"
ope="N"

idp=0
while [ $# -ge 1 ] ; do
  if [ $1 = -h ] ; then
    write_help
    exit 1
  elif [ $1 = "-z" ] ; then
    area_restrict="Y"
    shift
    xmin=$1
    shift
    ymin=$1
    shift
    xmax=$1
    shift
    ymax=$1
    shift
  elif [ $1 = "-d" ] ; then
    data_restrict="Y"
    shift
    data1=$1
    shift
    data2=$1
    shift
  elif [ $1 = "-ope" ] ; then
    ope="Y"
    shift
  elif [ $1 = "-url" ] ; then
    shift
    akurl=$1
    shift
  else
    if [ $idp -eq 0 ] ; then
      data1=$1
    elif [ $idp -eq 1 ] ; then
      data2=$1
    fi
    idp=$[idp+1]
    shift
  fi
done

# Assegno l'ambiente ma_utils
if [ -z $MA_UTILS_DAT ] ; then
  product_shortnames=/usr/share/ma_utils/param_shortnames.csv
else
  echo "(crea_anag_arkioss.ksh) Tabelle ma_utils: copia di lavoro in "$MA_UTILS_DAT
  product_shortnames=${MA_UTILS_DAT}/param_shortnames.csv
fi

# Assegno l'ambiente dballe
if [ -z $DBA_TABLES ] ; then
  dballe_txt=/usr/share/wreport/dballe.txt
else
  echo "(crea_anag_arkioss.ksh) Tabelle dballe: copia di lavoro in "$DBA_TABLES
  dballe_txt=${DBA_TABLES}/dballe.txt
fi

# Dir di lavoro e nome files di output
if [ $ope = "Y" ] ; then
  fileout1=/home/eminguzzi/svn/ma_utils/data/anag_arkioss.csv
  fileout2=/home/eminguzzi/svn/ma_utils/data/param_arkioss.csv
  if [ -z $AK_TEMP ] ; then
    AK_TEMP=$TEMP
  fi
  cd $AK_TEMP
  qid=`mktemp -d ako.XXXXXX | cut -d . -f 2` 2>&1
  work_dir=${AK_TEMP}/ako.${qid}
else
  fileout1=./anag_arkioss.csv
  fileout2=./param_arkioss.csv
  work_dir=./
fi

if [ -d $work_dir ] ; then
  cd $work_dir
  echo "Dir di lavoro "$(pwd)
else
  echo "Dir di lavoro non trovata "$work_dir
  exit 2
fi

# Scrivo l'header dei files di output
rm -f $fileout1 $fileout2
echo "id_staz,lat,lon,quota,dataset,nome" > $fileout1
echo "id_var,bcode,long_name,l1,lt1,p1,p2,tr,unit,short_name,net_number,net_list" > $fileout2

# Costruisco la query arkioss
rm -f caa.query
if [ $data_restrict = "N" ] ; then
  echo "Cerco dati con reftime qualsiasi"
   echo "reftime:" > caa.query
else
  echo "Cerco dati con reftime tra "$data1" e "$data2
  echo "reftime: >="$(date -d $data1 +%Y-%m-%d)", <="$(date -d $data2 +%Y-%m-%d) \
    > caa.query
fi

if [ $area_restrict = "N" ] ; then
  echo "Cerco dati con coordinate qualsiasi"
else
  echo "Cerco dati nel rettangolo "$xmin" "$ymin" "$xmax" "$ymax
  echo "area: bbox coveredby POLYGON(($xmin $ymin, $xmax $ymin, $xmax $ymax, $xmin $ymax, $xmin $ymin))" \
    >> caa.query
fi

# Varie
unset http_proxy

#===============================================================================
# 2) Elaborazioni

#-------------------------------------------------------------------------------
# 2.0 Scarico da meteozen l'anagrafica completa

rm -f tmp.dat anag_meteozen.dat
curl -u "ugo:Ul1ss&" "http://meteozen.metarpa/simcstations/api/stations" > tmp.dat
cat tmp.dat | sed 's/{"id"/\n/g' | cut -d , -f 1,3,4,5,6,7 > anag_meteozen.dat
ns=$(wc -l anag_meteozen.dat |awk '{print $1}')
echo "Scaricata anagrafica meteozen, stazioni trovate "$ns

#-------------------------------------------------------------------------------
# 2.1 Trovo l'elenco dei dataset presenti su arkioss

echo "Scarico l'elenco dei dataset"
rm -f tmp.ds ds.lst
curl $akurl 2>/dev/null > tmp.ds
grep /dataset/ tmp.ds | cut -d / -f 3 | cut -d \' -f 1 | grep -v error > ds.lst

#-------------------------------------------------------------------------------
# 2.2 Trovo i parametri (Product) e le stazioni (Area) presenti in archivio per 
#     ciascun dataset

rm -f all.pd

while read net ; do
  echo "Cerco stazioni e parametri per il dataset "$net
  rm -f ${net}.yml ${net}.varlist tmp1.yml tmp2.yml 
  arki-query --summary --yaml --file=caa.query ${akurl}/dataset/${net} > ${net}.yml

# Elaboro le stazioni (area)
  grep Area ${net}.yml | sort -u > tmp1.yml
  while read line ; do
    parse_area $line
    echo $str_area_csv >> $fileout1
  done < tmp1.yml

# Elaboro i parametri (product)
  grep Product ${net}.yml | sort -t "," -k 2,3 | uniq > tmp2.yml
  cut tmp2.yml -d \( -f 2 | cut -d , -f 1 > ${net}.varlist
  while read line ; do
    parse_product
    echo $str_product_csv >> all.pd
  done < tmp2.yml

done < ds.lst

#-------------------------------------------------------------------------------
# 2.3 Elaboro il file con la lista dei prarmetri: ordino, tolgo i doppioni, 
#     aggiungo la lista delle reti in cui e' presente ciascun parametro

echo "Produco la lista dei parametri"
rm -f sorted.pd 
sort -u -t , -k 2 all.pd > sorted.pd
while read line ; do
  id_var=$(echo $line | cut -d , -f 1)
  net_list=$(grep -l $id_var *.varlist | cut -d . -f 1)
  net_number=$(grep -l $id_var *.varlist | wc -l | awk '{print $1}')
  line_out=${line}","${net_number}","${net_list}
  echo $line_out >> $fileout2
done < sorted.pd
