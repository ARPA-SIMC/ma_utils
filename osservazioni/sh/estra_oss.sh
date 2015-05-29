#/bin/ksh
#-------------------------------------------------------------------------------
# Estrae i dati di una o piu' stazioni meteo, e li converte nei vecchi formati
# estra_orari / estra_tmp
#
# Note:
# - Non e' possibile estrarre simultaneamente stazioni Synop e HFR (ie. non GTS),
#   in quanto l'id-oracle di una stazione ad alta frequenza potrebbe coincidere
#   col codice di una stazione GTS.
# - La lista dei parametri e' gestita in modo diverso per dati Synop e HFR, in
#   quanto:
#   . per i dati synop, deve essere possibile estrarre parametri che non sono
#     misurati dalle stazioni HFR (e non sono inclusi in param_arkioss.csv)
#   . nei dati HFR bisogna poter distingure tra parametri diversi con la stessa 
#     variabile fisica
# - L'estrazione dei dati synop si basa solo sulla variabile fisica (Btable): se
#   il messaggio synop contiene piu' volte la stessa varibile, viene estratto 
#   solo l'ultimo valore.
# - L'opzione -oracle permette di estrarre i dati dal vecchio archivo Orcale:
#   e' meno garantita e molto piu' lenta, ma finche' non sara' completato il 
#   trasferimento dei dati storici in arkioss e' l'unico modo per estrarre dati
#   vecchi.
#
# TODO:
# - gestire dataset lmruc_* (per dati in tempo reale)
#
#                                              Versione 2.0.0, Enrico 28/05/2015
#-------------------------------------------------------------------------------
#set -x

#===============================================================================
# Scrive a schermo l'help della procedura
function write_help
{
#       12345678901234567890123456789012345678901234567890123456789012345678901234567890
  echo "Uso: estra_oss.ksh [-syn/-temp/-oracle]  data_ini data_end   IDSTA / -sl FILESTA"
  echo "     IDPAR / -pl FILEPAR   [-ndec N] [-tc] [-phpa]  [-deb] [-h]"
  echo ""
  echo "Estrae da arkioss/arkimet/oracle i dati osservati relativi a una o piu' giornate, "
  echo "stazioni e parametri, e li scrive nei vecchi formati estra_orari/estra_temp."
  echo ""
  echo "Di Default, estrae da arkioss i dati relativi alle reti non GTS"
  echo "  -syn    : estrae da arkimet i dati relativi alla rete Synop"
  echo "  -temp   : estrae da arkimet i dati relativi alla rete TEMP"
  echo "  -oracle : estrae da Oracle i dati relativi alle reti non GTS (obsolescente, ma"
  echo "            necessario per estrarre dati da reti non gts precedenti il 01/04/2014)"
  echo ""
  echo "data_ini: prima data da estrarre  (AAAAMMGG)"
  echo "data_end: ultima data da estrarre (AAAAMMGG)"
  echo ""
  echo "IDSTA:    id delle stazioni da estrarre (se piu' di una, separare con virgole)"
  echo "          - con reti non-GTS usare il codice Oracle (anag_arkioss.csv)"
  echo "          - con reti synop e temp usare il codice WMO (5 cifre: anag_synop.csv)"
  echo "FILESTA:  file con la lista degli id delle stazioni da estrarre"
  echo ""
  echo "IDPAR:    id dei parametri da estrarre (se piu' di uno, separare con virgole)"
  echo "          - per reti non-GTS, indicare i codici Oracle (param_shortnames.csv o"
  echo "            param_arkioss.csv)"
  echo "          - per rete synop, indicare i codici B-table (Bxxxxx: param_shortnames.csv)"
  echo "          - per i radiosondaggi, mettere 'temp'"
  echo "FILEPAR:  file con la lista degli id dei parametri da estrarre"
  echo ""
  echo "-tc       scrive le temperature in gradi centigradi"
  echo "-phpa     scrive la pressione in hPa (invece che in Pa)"
# echo "-uv       scrive le componenti del vento (invece di dierzione e modulo)"
  echo "-deb      salva i file intermedi, costruisce files di log"
  echo "-h        visualizza questo help"
#       12345678901234567890123456789012345678901234567890123456789012345678901234567890
  return
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
# 1) Preliminari

#-------------------------------------------------------------------------------
# 1.1) Path e utility

# URL degli archivi
akmurl="http://arkimet.metarpa:8090"
akourl="http://arkioss.metarpa:8090"
ds_temp="gts_temp"
ds_syn="gts_synop"

# Assegno l'ambiente ma_utils
if [ -z $MA_UTILS_SVN ] ; then
  bufr_csv2orari=/usr/libexec/ma_utils/bufr_csv2orari.exe
  bufr_csv2temp=/usr/libexec/ma_utils/bufr_csv2temp.exe
else 
  echo "(estra_oss.ksh) Eseguibili ma_utils: copia di lavoro in "$MA_UTILS_SVN
  bufr_csv2orari=${MA_UTILS_SVN}/osservazioni/src/bufr_csv2orari.exe
  bufr_csv2temp=${MA_UTILS_SVN}/osservazioni/src/bufr_csv2temp.exe
fi

if [ -z $MA_UTILS_DAT ] ; then
  anag_arkioss=/usr/share/ma_utils/anag_arkioss.csv
  anag_synop=/usr/share/ma_utils/anag_synop.csv
  anag_temp=/usr/share/ma_utils/anag_temp.csv
  param_arkioss=/usr/share/ma_utils/param_arkioss.csv
  param_shortnames=/usr/share/ma_utils/param_shortnames.csv
else
  echo "(estra_oss.ksh) Tabelle ma_utils: copia di lavoro in "$MA_UTILS_DAT
  anag_arkioss=${MA_UTILS_DAT}/anag_arkioss.csv
  anag_synop=${MA_UTILS_DAT}/anag_synop.csv
  anag_temp=${MA_UTILS_DAT}/anag_temp.csv
  param_arkioss=${MA_UTILS_DAT}/param_arkioss.csv
  param_shortnames=${MA_UTILS_DAT}/param_shortnames.csv
fi

#-------------------------------------------------------------------------------
# 1.2 Parametri da riga comando
# Siccome i parametri sta_list e par_list possono essere formati da piu' 
# parole, tutti i test devono essere compiuti sulla prima parola del parametro

input_sta=id
input_par=id
id_arc="hfr"
url=$akourl
deb="N"
opt=""

mand_par=0
req_par=4
if [ $# -eq 0 ] ; then
  write_help
  exit 1
fi
while [ $# -ge 1 ] ; do
  if [ `echo $1 | awk '{print $1}'` = '-h' ] ; then
    write_help
    exit 1
  elif [ `echo $1 | awk '{print $1}'` = '-sl' ] ; then
    input_sta="lst"
    shift
    file_sta=$1
    shift
    req_par=`expr $req_par - 1`
  elif [ `echo $1 | awk '{print $1}'` = '-pl' ] ; then
    input_par="lst"
    shift
    file_par=$1
    shift
    req_par=`expr $req_par - 1`
  elif [ `echo $1 | awk '{print $1}'` = '-syn' ] ; then
    id_arc="syn"
    url=$akmurl
    shift
  elif [ `echo $1 | awk '{print $1}'` = '-temp' ] ; then
    id_arc="temp"
    url=$akmurl
    shift
  elif [ `echo $1 | awk '{print $1}'` = '-oracle' ] ; then
    id_arc="oracle"
    shift
  elif [ `echo $1 | awk '{print $1}'` = '-ndec' ] ; then
    shift
    ndec=$1
    shift
    opt=${opt}" -ndec "${ndec}
  elif [ `echo $1 | awk '{print $1}'` = '-tc' ] ; then
    shift
    opt=${opt}" -tc"
  elif [ `echo $1 | awk '{print $1}'` = '-phpa' ] ; then
    shift
    opt=${opt}" -phpa"
  elif [ `echo $1 | awk '{print $1}'` = '-deb' ] ; then
    shift
    opt=${opt}" -deb"
    deb="Y"
  else
    if [ $mand_par -eq 0 ] ; then
      data1=$1
    elif [ $mand_par -eq 1 ] ; then
      data2=$1
    elif [ $mand_par -eq 2 ] ; then
      if [ $input_sta = "id" ] ; then
        sta_list_str=$1
      else
        par_list_str=$1
      fi
    elif [ $mand_par -eq 3 ] ; then
      par_list_str=$1
    fi
    mand_par=$[$mand_par + 1]
    shift
  fi
done

#-------------------------------------------------------------------------------
# 1.3 Controlli ed eleborazioni dipendenti dai parametri

if [ $mand_par != $req_par ] ; then
  write_help
  exit 1
fi

if [ $input_sta = "lst" ] ; then
  if [ ! -f $file_sta ] ; then
    echo "file "$file_sta" non trovato"
    exit 2
  else
    sta_list=`cat $file_sta`
  fi 
else
  sta_list=`echo $sta_list_str | sed 's/,/ /g'`
fi

if [ $input_par = "lst" ] ; then
  if [ ! -s $file_par ] ; then
    echo "file "$file_par" vuoto o non trovato"
    exit 2
  else
    par_list=`cat $file_par`
  fi
else
  par_list=`echo $par_list_str | sed 's/,/ /g'`
fi

#-------------------------------------------------------------------------------
# 1.4 Costruisco il file con le informazioni relative ai prametri richiesti

rm -f eo_param.csv
if [ $id_arc = "hfr" -o $id_arc = "oracle" ] ; then
  for par in $par_list ; do
    grep ^$par $param_arkioss >> eo_param.csv
    if [ $? -ne 0 ] ; then
      echo "Warning: parametro "$par" non trovato in "$param_arkioss
    fi
  done

elif [ $id_arc = "syn" ] ; then
  for bcode in $par_list ; do
    str=`grep ","${bcode}"," $param_shortnames`
    if [ $? -eq 0 ] ; then
      shortname=`echo $str | cut -d , -f 3`
    else
      shortname=$bcode
    fi
    echo ","$bcode",,,,,,,,"$shortname",," >> eo_param.csv
  done  

fi

#===============================================================================
# 2) Elaborazioni (ciclo sulle stazioni)

nsta=`echo $sta_list | wc -w`
npar=`echo $par_list | wc -w`
echo "Estraggo ${npar} parametri da ${nsta} stazioni"

for id_staz in $sta_list ; do

#-------------------------------------------------------------------------------
# 2.1 Trovo dataset e anagrafica della stazione richiesta

  if [ $id_arc = "hfr" -o $id_arc = "oracle" ] ; then
    grep ^${id_staz}, $anag_arkioss > /dev/null 2>&1
    if [ $? -ne 0 ] ; then
      echo "Stazione "$id_staz" non trovata in "$anag_arkioss
      exit 3
    fi
    str_anag=`grep ^${id_staz}, $anag_arkioss`
    dataset=`echo $str_anag | cut -d , -f 2`
    nome_sta=`echo $str_anag | cut -d , -f 6`

  elif [ $id_arc = "syn" ] ; then
    blo=`echo $id_staz | awk '{print substr($1,1,2)}'`
    sta=`echo $id_staz | awk '{print substr($1,3,3)}'`
    sta2=`echo $sta | sed 's/^0*//'`
    grep ^${blo},${sta} $anag_synop > /dev/null 2>&1
    if [ $? -ne 0 ] ; then
      echo "Stazione "$id_staz" non trovata in "$anag_synop
      exit 3
    fi
    dataset="gts_synop"
    nome_sta=$id_staz

  elif [ $id_arc = "temp" ] ; then
    blo=`echo $id_staz | awk '{print substr($1,1,2)}'`
    sta=`echo $id_staz | awk '{print substr($1,3,3)}'`
    sta2=`echo $sta | sed 's/^0*//'`
    grep ^${blo},${sta} $anag_temp > /dev/null 2>&1
    if [ $? -ne 0 ] ; then
      echo "Stazione "$id_staz" non trovata in "$anag_temp
      exit 3
    fi
    dataset="gts_temp"
    nome_sta=$id_staz

  fi

  intfill $id_staz 5
  id_staz2=$str_out
  echo "Elaboro stazione "$id_staz": "\"$nome_sta\"", dataset "$dataset

#-------------------------------------------------------------------------------
# 2.2 Costruisco la query

  rm -f eo.query eo.out eo.bufr eo.vmold eo.vm eo.query
  rm -f ${dataset}.conf eo_${id_staz2}.csv eo_${id_staz2}.dat
  if [ $id_arc = "hfr" -o $id_arc = "syn" -o $id_arc = "temp" ] ; then
    arki-mergeconf ${url}/dataset/${dataset} > ${dataset}.conf
  fi

# Reftime
  data1m1=`date -d "$data1 - 1day" +%Y%m%d`
  if [ `echo $par_list | awk '{print $1}'` = "temp" ] ; then
    str_reftime=">="`date -d $data1m1 +%Y-%m-%d`" 22, <="`date -d $data2 +%Y-%m-%d`" 22"
  elif [ $id_arc = "hfr" -o $id_arc = "syn" ] ; then
    str_reftime=">="`date -d $data1 +%Y-%m-%d`", <="`date -d $data2 +%Y-%m-%d`
  elif [ $id_arc = "oracle" ] ; then
    str_reftime=`date -d $data1 +"yearmin=%Y monthmin=%m daymin=%d"`" "`date -d $data2 +"yearmax=%Y monthmax=%m daymax=%d"`
  fi

# Stazione e parametri
  if [ $id_arc = "hfr" ] ; then
    str_sta="area: VM2,${id_staz}"
    str_dum="product:"
    for par in $par_list ; do
      str_dum=${str_dum}" VM2,${par} or"
    done
    str_par=${str_dum%or}

  elif [ $id_arc = "syn" -o $id_arc = "temp" ] ; then
    str_sta="proddef: GRIB:blo=${blo},sta=${sta2}"
    str_par=""

  elif [ $id_arc = "oracle" ] ; then
    str_sta="ana_id=${id_staz}"
    str_par=""

  fi

# Query
  if [ $id_arc = "hfr" -o $id_arc = "syn" -o $id_arc = "temp" ] ; then
    cat <<EOF > eo.query
    reftime: ${str_reftime}
    $str_sta
    $str_par
EOF
  elif [ $id_arc = "oracle" ] ; then
    echo dbavm dump --vm $str_sta $str_reftime > eo.query
  fi

#-------------------------------------------------------------------------------
# 2.3 Estrazione e conversioni di formato
# Per le estrazioni da Oracle, in teoria si potrebbero produrre direttamente i
# dati in formato BUFR (dbamsg export o dbaexport), ma pare che non funzioni...

  if [ $id_arc = "hfr" ] ; then
    arki-query --data --config=${dataset}.conf --file=eo.query > eo.vm
  elif [ $id_arc = "syn" -o $id_arc = "temp" ] ; then
    arki-query --data --config=${dataset}.conf --file=eo.query > eo.bufr
  elif [ $id_arc = "oracle" ] ; then
    dbavm dump --vm $str_sta $str_reftime > eo.vmold
    tail -n +2 eo.vmold | sed 's/://g;s/\///g;s/;/,/g' > eo.vm
  fi

  if [ $id_arc = "hfr" -o $id_arc = "oracle" ] ; then
    meteo-vm2-to-bufr < eo.vm > eo.bufr 2>vm2_bufr.err
  fi
  dbamsg dump --type=bufr --csv eo.bufr > eo.csv

  if [ $id_arc = "hfr" -o $id_arc = "oracle" ] ; then
    $bufr_csv2orari $opt eo.csv $data1 $data2 eo_param.csv $id_staz2
  elif [ $id_arc = "syn" ] ; then
    $bufr_csv2orari $opt -syn eo.csv $data1 $data2 eo_param.csv $id_staz2
  elif [ $id_arc = "temp" ] ; then
    $bufr_csv2temp eo.csv
  fi

  if [ $deb = "Y" ] ; then
    cp eo.csv eo_${id_staz2}.csv
    cp eo.bufr eo_${id_staz2}.bufr
    if [ $id_arc = "hfr" -o $id_arc = "oracle" ] ; then
      cp eo.vm eo_${id_staz2}.vm
      cp vm2_bufr.err vm2_bufr_${id_staz2}.err
    fi
    if [ $id_arc = "hfr" -o $id_arc = "syn" -o $id_arc = "temp" ] ; then
      cp eo.query eo_${id_staz2}.query
    fi
  fi

done
