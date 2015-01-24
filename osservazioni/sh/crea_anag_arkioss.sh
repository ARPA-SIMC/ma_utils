#/bin/ksh
#-------------------------------------------------------------------------------
# Estre da arkioss le infomrazioni relative alle stazioni e ai prarametri 
# presenti in ciascuna rete, e le scrive in due files .csv
#
# Per modificare i path del pacchetto ma_utils, assegnare le variabili:
# MA_UTILS_DAT: tabelle ma_utils (ad esempio: /home/eminguzzi/svn/ma_utils/data;
#               se non specificato, usa: /usr/share/ma_utils)
# MA_UTILS_SVN: eseguibili ma_utils (ad esempio: /home/eminguzzi/svn/ma_utils;
#               se non specificato, usa: /usr/libexec/ma_utils)
#
# Per modificare i path di pacchetti libsim/dballe, assegnare le variabili:
# LIBSIM_DATA:  tabelle libsim, ie. file vargrib2bufr.csv 
#               (se non specificato usa: /usr/share/libsim)
# DBA_TABLES:   tabelle dballe, ie. file dballe.txt 
#               (se non specificato usa: /usr/share/wreport)
# LIBSIM_SVN:   eseguibili libsim (ad esempio:  
#               /home/eminguzzi/svn/vol7d, /home/dcesari/program/f90/svn/trunk; 
#               se non specificato, usa: /usr/bin)
#
# TODO:
# - Aggiungere parmaetro -reft, per limitarsi alle stazioni che hanno dati in un
#   certo periodo (arki-scan ... -> arki-query "" ...)
# - Estrarre un dato qualsiasi da ciascuna stazione, e ricavare lat,lon,quota,nome
#   senza passare da anag_oracle (aspettare che Emanuele implementi la funzione)
# - aggiungere campo "rete"
#
# Note:
# - L'unica informazione di anagrafica contnuta in arkioss e' il codice stazione 
#  (id_oracle): le altre informazioni sono desunte dall'anagrafica ORACLE 
#  (file anag_oracle.dat - ex db_anagrafica.dat, prodotto da crea_anag_oracle.sql)
# - Si potrebbe aggiungere un controllo sull'unicita' dell'id_oracle in 
#   anag_oracle.dat
# - Si potrebbere aggiungere a fileout le colonne relative al nome rete e id_usr
#   (nel mondo Oracle: per collegamento con pagine "caricamenti" su infomet)
#
#                                              Versione 1.0.0, Enrico 16/09/2014
#-------------------------------------------------------------------------------
#set -x

#===============================================================================
# Funzione per riformattare "csv" la descrizione delle varibili Oracle, gestendo
# il fatto che alcuni campi (es. l1) possono essere mancanti

function parse_prod
{

# id-var (Oracle)
id_var=`echo $line|cut -d \( -f 2 | cut -d , -f 1 | cut -d \) -f 1`

# Bcode (codice parametro fisico)
idx=`echo $line | awk '{print index("'"${line}"'","bcode=")}'`
if [ $idx -gt 0 ] ; then
  p1=`expr $idx + 6`
  str_dum=`echo $line | cut -c $p1-`
  bcode=`echo $str_dum | cut -d , -f 1 | cut -d \) -f 1`
else
  bcode="-9999"
fi
bcode2=`echo $bcode | sed 's/B/0/'`

# Long Name (nome esteso variabile fisica, da dballe.txt)
long_name=`grep ^\ $bcode2 $dballe_txt | cut -c 9-72 | sed 's/ *$//g;s/,/-/g'`

# Short Name (nome del parametro per output estra_orari, solo metamb)
str_dum=`grep ^$id_var $product_shortnames`
if [ $? -ne 0 ] ; then
  short_name=$id_var
else
  short_name=`echo $str_dum | cut -d , -f 3`
fi 

# Livello e Timerange
lev_trange=""
keys="l1 lt1 p1 p2 tr unit"
for key in $keys ; do
  idx=`echo $line | awk '{print index("'"${line}"'","'"${key}="'")}'`
  if [ $idx -gt 0 ] ; then
    p1=`expr length $key + $idx + 1`
    str_dum=`echo $line | cut -c $p1-`
    lev_trange=${lev_trange}","`echo $str_dum | cut -d , -f 1 | cut -d \) -f 1`
  else
    lev_trange=${lev_trange}","
  fi
done

# Compongo la stringa di output
out_string=${id_var}","${bcode}","${long_name}${lev_trange}","${short_name}

}

#===============================================================================
# 1) Preliminari

#-------------------------------------------------------------------------------
# 1.1 Path, costanti, utility

# URL di arkioss
akurl=http://arkioss.metarpa:8090/

# Assegno l'ambiente ma_utils
if [ -z $MA_UTILS_DAT ] ; then
  anag_oracle=/usr/share/ma_utils/anag_oracle.dat
  product_shortnames=/usr/share/ma_utils/param_shortnames.csv
  fileout1=./anag_arkioss.csv
  fileout2=./param_arkioss.csv
else
  echo "(crea_anag_arkioss.ksh) Tabelle ma_utils: copia di lavoro in "$MA_UTILS_DAT
  anag_oracle=${MA_UTILS_DAT}/anag_oracle.dat
  product_shortnames=${MA_UTILS_DAT}/param_shortnames.csv
  fileout1=${MA_UTILS_DAT}/anag_arkioss.csv
  fileout2=${MA_UTILS_DAT}/param_arkioss.csv
fi

# Assegno l'ambiente dballe
if [ -z $DBA_TABLES ] ; then
  dballe_txt=/usr/share/wreport/dballe.txt
else
  echo "(acrea_anag_arkioss.ksh) Tabelle dballe: copia di lavoro in "$DBA_TABLES
  dballe_txt=${DBA_TABLES}/dballe.txt
fi

# Segnalo eventuali variabili d'ambiente assegnate a valori non di default
if [ ! -z $MA_UTILS_SVN ] ; then
  echo "(crea_anag_arkioss.ksh) Eseguibili ma_utils: copia di lavoro in "$MA_UTILS_SVN
fi
if [ ! -z $LIBSIM_SVN ] ; then
  echo "(crea_anag_arkioss.ksh) Eseguibili libsim: copia di lavoro in "$LIBSIM_SVN
fi
if [ ! -z $LIBSIM_DATA ] ; then
  echo "(crea_anag_arkioss.ksh) Tabelle libsim: copia di lavoro in "$LIBSIM_DATA
fi

#-------------------------------------------------------------------------------
# 1.2 Altri preliminari

# Costruisco la dir di lavoro
if [ -z $AK_TEMP ] ; then
  AK_TEMP=$TEMP
fi
cd $AK_TEMP
qid=`mktemp -d ako.XXXXXX | cut -d . -f 2` 2>&1
work_dir=${AK_TEMP}/ako.${qid}

if [ -d $work_dir ] ; then
  cd $work_dir
  echo "Dir di lavoro "$work_dir
else
  echo "Dir di lavoro dell'estrazione non trovata "$work_dir
  exit 2
fi

# Scrivo l'header dei files di output
rm -f $fileout1 $fileout2
echo "id_staz,dset,lon,lat,quota,nome" > $fileout1
echo "id_var,bcode,long_name,l1,lt1,p1,p2,tr,unit,short_name,net_number,net_list" > $fileout2

# Seleziono i campi utili dall'anagrafica Oracle 
# Passo da foramto ASCII tabellare a csv delimitato da virgole
tail -n +4 $anag_oracle  | cut -c 12-42 | sed 's/  */,/g' | cut -d , -f 2-5 > tmp1.csv
tail -n +4 $anag_oracle  | cut -c 43- > tmp2.csv
paste -d , tmp1.csv tmp2.csv > anag.csv

#===============================================================================
# 2) Elaborazioni

#-------------------------------------------------------------------------------
# 2.1 Trovo l'elenco dei dataset presenti su arkioss

echo "Scarico l'elenco dei dataset"
rm -f tmp.ds ds.lst
unset http_proxy
curl $akurl 2>/dev/null > tmp.ds
grep /dataset/ tmp.ds | cut -d / -f 3 | cut -d \' -f 1 | grep -v error > ds.lst

#-------------------------------------------------------------------------------
# 2.2 Trovo i parametri (Product) e le stazioni (Area) presenti in archivio per 
#     ciascuna rete

rm -f *.prod.id *.prod.desc *.staz *.yml *.pd
while read net ; do
  echo "Cerco stazioni e parametri per il dataset "$net

  arki-scan --summary --yaml ${akurl}/dataset/${net} > ${net}.yml
  grep Area ${net}.yml | cut -d \( -f 2 | cut -d , -f 1 | sort -u > ${net}.staz
  grep Product ${net}.yml | cut -d \( -f 2 | cut -d , -f 1 | sort -u > ${net}.prod.id
  grep Product ${net}.yml | cut -d : -f 2 | sort -u | sed 's/ //g' > ${net}.prod.desc
  while read id_staz ; do
    str=`grep ^${id_staz}, anag.csv | cut -d , -f 2-`
    echo ${id_staz},${net},$str >> $fileout1
  done < ${net}.staz

  rm -f tmp.pd
  tail -n +2 ${net}.prod.desc > tmp.pd
  while read line ; do
    parse_prod
    echo $out_string >> all.pd
  done < tmp.pd
done < ds.lst

#-------------------------------------------------------------------------------
# 2.3 Elaboro il file con la lista dei prarmetri: ordino, tolgo i doppioni, 
#     aggiungo la lista delle reti in cui e' presente ciscun parametro

echo "Produco la lista dei parametri"
sort -u -t , -k 2 all.pd > sorted.pd
while read line ; do
  id_var=`echo $line | cut -d , -f 1`
  var_list=`grep -l "^VM2(${id_var}" *.prod.desc | cut -d . -f 1`
  var_number=`grep -l "^VM2(${id_var}" *.prod.desc | wc -l`
  line_out=${line}","${var_number}","${var_list}
  echo $line_out >> $fileout2
done < sorted.pd
