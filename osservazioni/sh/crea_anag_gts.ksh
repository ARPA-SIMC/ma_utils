#/bin/ksh
#-------------------------------------------------------------------------------
# Estre da arkimet le infomrazioni relative alle stazioni europee della rete
# GTS (synop/metar e temp) 
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
#
# Note:
# - A differenza di quanto avvien per le stazioni ad alta frequenza (archiviate
#   in formato VM2), i dati gts sono archiviati in formato BUFR: ciascun 
#   messaggio contiene quindi tutte le informazioni di anagrafica.
#
#                                              Versione 1.0.0, Enrico 18/08/2014
#-------------------------------------------------------------------------------
#set -x

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
# 1) Preliminari

#-------------------------------------------------------------------------------
# 1.1 Path, costanti, utility

# URL di arkioss
akurl=http://arkimet.metarpa:8090/

# Assegno l'ambiente ma_utils
if [ -z $MA_UTILS_DAT ] ; then
  anag_synop=./anag_synop.csv
  anag_temp=./anag_temp.csv
else
  echo "(crea_anag_gts.ksh) Tabelle ma_utils: copia di lavoro in "$MA_UTILS_DAT
  anag_synop=${MA_UTILS_DAT}/anag_synop.csv
  anag_temp=${MA_UTILS_DAT}/anag_temp.csv
fi

# Segnalo eventuali variabili d'ambiente assegnate a valori non di default
if [ ! -z $DBA_TABLES ] ; then
  echo "(acrea_anag_gts.ksh) Tabelle dballe: copia di lavoro in "$DBA_TABLES
fi
if [ ! -z $MA_UTILS_SVN ] ; then
  echo "(crea_anag_gts.ksh) Eseguibili ma_utils: copia di lavoro in "$MA_UTILS_SVN
fi
if [ ! -z $LIBSIM_SVN ] ; then
  echo "(crea_anag_gts.ksh) Eseguibili libsim: copia di lavoro in "$LIBSIM_SVN
fi
if [ ! -z $LIBSIM_DATA ] ; then
  echo "(crea_anag_gts.ksh) Tabelle libsim: copia di lavoro in "$LIBSIM_DATA
fi

#-------------------------------------------------------------------------------
# 1.2 Altri preliminari

# Costruisco la dir di lavoro
if [ -z $AK_TEMP ] ; then
  AK_TEMP=$TEMP
fi
cd $AK_TEMP
qid=`mktemp -d akg.XXXXXX | cut -d . -f 2` 2>&1
work_dir=${AK_TEMP}/akg.${qid}

if [ -d $work_dir ] ; then
  cd $work_dir
  echo "Dir di lavoro "$work_dir
else
  echo "Dir di lavoro dell'estrazione non trovata "$work_dir
  exit 2
fi

# Scrivo l'header dei files di output
rm -f $anag_synop $anag_temp
echo "blo,sta,lon,lat,quota,nome" > $anag_synop
echo "blo,sta,lon,lat,quota,nome" > $anag_temp

# Varie
unset http_proxy

#===============================================================================
# 2) Elaborazioni

for net in temp syn ; do
  if [ $net = "syn" ] ; then
    dataset="gts_synop"
    fileout=$anag_synop
  elif [ $net = "temp" ] ; then
    dataset="gts_temp"
    fileout=$anag_temp
  fi

# Scarico il summary del dataset
  echo "Rete "$net": estraggo il summary"
  arki-scan --summary --yaml ${akurl}/dataset/${dataset} > ${net}.yml

# Elaboro le singole stazioni. Gestisco le stazioni con id=" "
  echo "Elaboro le stazioni"
  grep Proddef ${net}.yml | sort -u > ${net}.yml.short
  ns1=`wc -l ${net}.yml.short | awk '{print $1}'`
  while read line1 ; do
    line=`echo $line1 | sed 's/"//g'`
    parse_proddef
    if [ \( $blo -ge 1 -a $blo -le 20 \) -o $blo = 22 -o $blo = 26 -o $blo = 27 -o \
      $blo = 33 -o $blo = 34 -o $blo = 37 -o $blo = 40 ] ; then
      echo ${blo2},${sta2} >> ${net}.anag
    fi
  done < ${net}.yml.short

  sort -u ${net}.anag >> $fileout
  ns2=`tail -n +2 $fileout | wc -l`
  echo "Selezionate ${ns2} stazioni su ${ns1}"
done
