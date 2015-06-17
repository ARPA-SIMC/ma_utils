#/bin/ksh
#-------------------------------------------------------------------------------
# Estre da arkimet le informazioni relative alle stazioni europee della rete
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
# - Estrarre un dato qualsiasi da ciascuna stazione, e ricavare lat,lon,quota,nome
#   Al momento non e' chiaro come fare: non c'e' modo fi estrarre da arkimet 
#   solo il primo dato valido (la query rischierebbe di rimanere appesa); le 
#   altrenative sembrerebbero estrarre una data a caso (ma si perdono dati), 
#   estrarre tutti i dati (ma diventa lentissimo), fare un ciclo di estrazioni
#   su date successive a caso (peggio che peggio...)
#
# Note:
# - Nei files di output, id_staz e' composto come: Xbbsss, con X="S" o "T", bb=
#   WMO block, sss=WMO number. Il carattere X serve a evitare conflitti con i 
#   codici Oracle dei dati ad alta frequenza.
# - A differenza di quanto avviene per le stazioni ad alta frequenza (archiviate
#   in formato VM2), i dati gts sono archiviati in formato BUFR: ciascun 
#   messaggio contiene quindi tutte le informazioni di anagrafica.
#
#                                              Versione 2.1.1, Enrico 15/06/2015
#-------------------------------------------------------------------------------
#set -x

#===============================================================================
function write_help
{
#       123456789012345678901234567890123456789012345678901234567890123456789012345678
  echo "Estre da arkimet le informazioni relative alle stazioni europee della rete"
  echo "GTS (synop e temp)"
  echo "Uso: crea_anag_gts [-z xmin ymin xmax ymax] [-d data_ini data_fin]"
  echo "     [-h] [-ope] "
  echo "xmin ymin xmax ymax: estremi dell'aera geografica di ricerca (def: ovunque)"
  echo "data_ini, data_fin: intervallo di date in cui cercare (def: qualsiasi data)"
  echo "-ope: aggiorna l'anagrafica in /home/eminguzzi/svn/ma_utils/data"
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
# 1) Preliminari

# Parametri da riga comando
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

# URL  dell'archivio arkimet
akurl=http://arkimet.metarpa:8090/

# Dir di lavoro e nome files di output
if [ $ope = "Y" ] ; then
  fileout1=/home/eminguzzi/svn/ma_utils/data/anag_synop.csv
  fileout2=/home/eminguzzi/svn/ma_utils/data/anag_temp.csv
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
      echo ${ch1}${blo2}${sta2} >> ${net}.anag
    fi
  done < ${net}.yml.short

  sort -u ${net}.anag >> $fileout
  ns2=`tail -n +2 $fileout | wc -l`
  echo "Selezionate ${ns2} stazioni su ${ns1}"
done
