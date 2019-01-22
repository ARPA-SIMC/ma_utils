#/bin/ksh
#-------------------------------------------------------------------------------
# Estre da arkimet le informazioni relative alle stazioni europee della rete
# GTS (synop/metar e temp) 
#
# Per modificare i path del pacchetto ma_utils, assegnare le variabili:
# MA_UTILS_DAT: tabelle ma_utils (ad esempio: ~eminguzzi/svn/ma_utils/data;
#               se non specificato, usa: /usr/share/ma_utils)
# MA_UTILS_SVN: eseguibili ma_utils (ad esempio: ~eminguzzi/svn/ma_utils;
#               se non specificato, usa: /usr/libexec/ma_utils)
#
# Per modificare i path di pacchetti libsim/dballe, assegnare le variabili:
# LIBSIM_DATA:  tabelle libsim, ie. file vargrib2bufr.csv 
#               (se non specificato usa: /usr/share/libsim)
# DBA_TABLES:   tabelle dballe, ie. file dballe.txt 
#               (se non specificato usa: /usr/share/wreport)
# LIBSIM_SVN:   eseguibili libsim (ad esempio:  
#               ~eminguzzi/svn/vol7d, /home/dcesari/program/f90/svn/trunk; 
#               se non specificato, usa: /usr/bin)
#
# TODO:
# - Anagrficia completa anche per la rete TEMP: richiede 
#
# Note:
# - Nei files di output, id_staz e' composto come: Xbbsss, con X="S" o "T", bb=
#   WMO block, sss=WMO number. Il carattere X serve a evitare conflitti con i 
#   codici Oracle dei dati ad alta frequenza.
# - A differenza di quanto avviene per le stazioni ad alta frequenza (archiviate
#   in formato VM2), i dati gts sono archiviati in formato BUFR: ciascun 
#   messaggio contiene quindi tutte le informazioni di anagrafica.
# - Estremi zoom: per BPA "6 43.5 14 47", per EmR "9.2 43.8 13 45.2"
#
#                                              Versione 2.2.0, Enrico 29/06/2015
#-------------------------------------------------------------------------------
#set -x

#===============================================================================
function write_help
{
#       123456789012345678901234567890123456789012345678901234567890123456789012345678
  echo "Estre da arkimet le informazioni relative alle stazioni europee della rete"
  echo "GTS (synop e temp)"
  echo "Uso: crea_anag_gts [-z xmin ymin xmax ymax] [-d data_ini data_fin]"
  echo "     [-h] [-ope] [-coo]"
  echo "xmin ymin xmax ymax: estremi dell'aera geografica di ricerca (def: ovunque)"
  echo "data_ini, data_fin: intervallo di date in cui cercare (def: qualsiasi data)"
  echo "-ope: aggiorna l'anagrafica in ~eminguzzi/svn/ma_utils/data"
  echo "-full: scrive anche coordinate, quota e nome stazione (lento; def: solo codice) "
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
# Funzione per riformattare da "yaml" a "csv" la descrizione del "Proddef" GTS

function parse_proddef
{

# blo (WMO block number)
idx=`echo $line | awk '{print index("'"${line}"'","blo=")}'`
if [ $idx -gt 0 ] ; then
  p1=`expr $idx + 4`
  str_dum=`echo $line | cut -c $p1-`
  blo=`echo $str_dum | cut -d , -f 1 | cut -d \) -f 1`
else
  blo="0"
fi
intfill $blo 2
blo2=$str_out

# sta (WMO station number)
idx=`echo $line | awk '{print index("'"${line}"'","sta=")}'`
if [ $idx -gt 0 ] ; then
  p1=`expr $idx + 4`
  str_dum=`echo $line | cut -c $p1-`
  sta=`echo $str_dum | cut -d , -f 1 | cut -d \) -f 1`
else
  sta="0"
fi
intfill $sta 3
sta2=$str_out

# id
idx=`echo $line | awk '{print index("'"${line}"'","id=")}'`
if [ $idx -gt 0 ] ; then
  p1=`expr $idx + 3`
  str_dum=`echo $line | cut -c $p1-`
  id=`echo $str_dum | cut -d , -f 1 | cut -d \) -f 1`
else
  id=""
fi

}

#===============================================================================
function build_anag
{
#----------------------------------------------------------------------
# Cerca di estrarre qualche dato della stazione corrente, per salvare 
# l'anagrafica campleta
#----------------------------------------------------------------------

# Assegnazioni valide per tutti i tentativi
  rm -f ba.query ba_param.csv ba.bufr ba.csv "S"${blo2}${sta2}".ana"
  data_ba1=$(date -d "$data2 - 5day" +%Y%m%d)
  echo ",B12101,,,,,,,,T ist,," > ba_param.csv

# Primo tentativo: estraggo gli ultimi 5 giorni
  echo "reftime: >="$(date -d $data_ba1 +%Y-%m-%d)", >="$(date -d $data1 +%Y-%m-%d)", <="$(date -d $data2 +%Y-%m-%d) > ba.query
  echo "proddef: GRIB: blo="$blo", sta="$sta >> ba.query
  arki-query --data --file=ba.query ${akurl}/dataset/${dataset} > ba.bufr
  dbamsg dump --type=bufr --csv ba.bufr > ba.csv
  $bufr_csv2orari -syn ba.csv $data_ba1 $data2 ba_param.csv "S"${blo2}${sta2} \
    > /dev/null 2>&1
  iret=$?

# Secondo tentativo: estraggo l'intero periodo
  if [ $iret -ne 0 -a $iret -ne 102 ] ; then
    rm -f ba.query ba.bufr ba.csv "S"${blo2}${sta2}".ana" 
    echo "reftime: >="$(date -d $data1 +%Y-%m-%d)", <="$(date -d $data2 +%Y-%m-%d) > ba.query
    echo "proddef: GRIB: blo="$blo", sta="$sta >> ba.query
    arki-query --data --file=ba.query ${akurl}/dataset/${dataset} > ba.bufr
    dbamsg dump --type=bufr --csv ba.bufr > ba.csv
    $bufr_csv2orari -syn ba.csv $data1 $data2 ba_param.csv "S"${blo2}${sta2} \
      > /dev/null 2>&1
    iret=$?
  fi

}

#===============================================================================
# 1) Preliminari

# URL  dell'archivio arkimet
akurl=http://arkimet.metarpa:8090/

# Assegno l'ambiente ma_utils
if [ -z $MA_UTILS_SVN ] ; then
  bufr_csv2orari=/usr/libexec/ma_utils/bufr_csv2orari.exe
  bufr_csv2temp=/usr/libexec/ma_utils/bufr_csv2temp.exe
else 
  echo "(estra_oss.ksh) Eseguibili ma_utils: copia di lavoro in "$MA_UTILS_SVN
  bufr_csv2orari=${MA_UTILS_SVN}/osservazioni/src/bufr_csv2orari.exe
  bufr_csv2temp=${MA_UTILS_SVN}/osservazioni/src/bufr_csv2temp.exe
fi

# Parametri da riga comando
data_restrict="N"
area_restrict="N"
ope="N"
full="N"

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
  elif [ $1 = "-full" ] ; then
    full="Y"
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

# Dir di lavoro e nome files di output
if [ $ope = "Y" ] ; then
  fileout1=~eminguzzi/svn/ma_utils/data/anag_synop.csv
  fileout2=~eminguzzi/svn/ma_utils/data/anag_temp.csv
  if [ -z $AK_TEMP ] ; then
    AK_TEMP=$TEMP
  fi
  cd $AK_TEMP
  qid=`mktemp -d ako.XXXXXX | cut -d . -f 2` 2>&1
  work_dir=${AK_TEMP}/ako.${qid}
else
  fileout1=./anag_synop.csv
  fileout2=./anag_temp.csv
  work_dir=./
fi

if [ -d $work_dir ] ; then
  cd $work_dir
  echo "Dir di lavoro "$(pwd)
else
  echo "Dir di lavoro non trovata "$work_dir
  exit 2
fi

# Costruisco la query arkimet
rm -f cag.query
if [ $data_restrict = "N" ] ; then
  echo "Cerco dati con reftime qualsiasi"
  touch cag.query
else
  echo "Cerco dati con reftime tra "$data1" e "$data2
  echo "reftime: >="$(date -d $data1 +%Y-%m-%d)", <="$(date -d $data2 +%Y-%m-%d) \
    >> cag.query
fi

if [ $area_restrict = "N" ] ; then
  echo "Cerco dati con coordinate qualsiasi"
else
  echo "Cerco dati nel rettangolo "$xmin" "$ymin" "$xmax" "$ymax
  echo "area: bbox coveredby POLYGON(($xmin $ymin, $xmax $ymin, $xmax $ymax, $xmin $ymax, $xmin $ymin))" \
    >> cag.query
fi

# Varie
unset http_proxy

#===============================================================================
# 2) Elaborazioni

for net in temp syn ; do
  if [ $net = "syn" ] ; then
    dataset="gts_synop"
    fileout=$fileout1
    ch1="S"
  elif [ $net = "temp" ] ; then
    dataset="gts_temp"
    fileout=$fileout2
    ch1="T"
  fi
  rm -f ${net}.yml ${net}.yml.short ${net}.anag $fileout
  echo "id_staz,lat,lon,quota,nome" > $fileout

# Scarico il summary del dataset
  echo "Rete "$net": estraggo il summary"
  arki-query --summary --yaml --file=cag.query ${akurl}/dataset/${dataset} \
    > ${net}.yml

# Elaboro le singole stazioni. Gestisco le stazioni con id=" "
  echo "Elaboro le stazioni"
  grep Proddef ${net}.yml | sort -u > ${net}.yml.short
  ns1=`wc -l ${net}.yml.short | awk '{print $1}'`
  while read line1 ; do
    line=`echo $line1 | sed 's/"//g'`
    parse_proddef
    if [ \( $blo -ge 1 -a $blo -le 20 \) -o $blo = 22 -o $blo = 26 -o $blo = 27 -o \
      $blo = 33 -o $blo = 34 -o $blo = 37 -o $blo = 40 ] ; then

# Se richiesto, estraggo i dati e salvo l'anagrafica completa, altrimenti salvo
# solo il codice stazione.
      if [ $net = "syn" -a $full = "Y" ] ; then
        echo "Cerco anagrafica completa, stazione "${ch1}${blo2}${sta2}
        build_anag
        if [ -s eo_${ch1}${blo2}${sta2}.ana ] ; then
          cat eo_${ch1}${blo2}${sta2}.ana >> ${net}.anag
        else
          echo ${ch1}${blo2}${sta2}",,,,," >> ${net}.anag
        fi
      else
        echo ${ch1}${blo2}${sta2} >> ${net}.anag
      fi
    fi
  done < ${net}.yml.short

  if [ -s ${net}.anag ] ; then
    sort -u ${net}.anag >> $fileout
    ns2=`tail -n +2 $fileout | wc -l`
  else
    ns2=0
  fi
  echo "Selezionate ${ns2} stazioni su ${ns1}"
done
