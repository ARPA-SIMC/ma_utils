#/bin/ksh
#-------------------------------------------------------------------------------
# Procedura per estrarre da un dataset Arkimet o da un file i dati su uno o piu'
# punti in formato seriet (tabella ASCII con una riga per ogni ora, una colonna
# per ogni coppia parametro/livello, dati mancanti espliciti)
#
# Lavora nella dir $AK_TEMP/$qid (default: $TEMP/$qid), che, se non viene usata
# l'opzione -debug, viene cancellata al termine dell'esecuzione; i files di 
# output e log vengono comunque copiati nella dir. corrente. 
# Nella dir. corrente devono essere presenti i files di input richiesti in base
# al parametro -reqdata
#
# Codici d'errore:
# 1 = errore nei parametri a riga comando
# 2 = errore nelle directory
# 3 = errore nell'accesso al server
# 4 = nessun dato trovato
# 5 = errore nella conversione GRIB1-UTM -> GRIB2 (ma_grib1_grib2.exe)
# 6 = errore nei dati estratti (gacsv_scan.exe)
# 7 = errore conversione in formato seriet (gacsv2seriet.exe)
# 8 = errore gestione coord. punti richiesti (write_libsim_anag.exe; ptn2pts_csv)
# 9 = i dati estratti non possono essere scritti in formato seriet
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
# TODO eventuali:
# - Quando sara' sistemato l'archivio LAMAZ, togliere l'opzione -fop
# - Quando vg6d_getpoint restituira' i dati con timerange ordinato per 
#   verification time, si potranno togliere i comandi sort (adesso le analisi
#   escono dopo le previsioni)
# - Quando le Grib-API gestiranno correttamente i campi Grib1 SOP, eliminare 
#   l'opzione -fop e tutto quanto ad essa collegato
# - Calcolo automatico degli estremi dello zoom (se mai si rivelasse utile...)
#   * se non richiesta destag, posso farlo a partire dal .ptn, passando a
#     vg6d_subarea le coordinate estreme.
#   * con destag sono castretto a passare a vg6d_subarea gli indici, e per
#     calcolarli devo anche estrarre e leggere un grib (lanciare la query con 
#     arki-xargs --max-args=1)
# - Opzione -server
# - Ricerca automatica della necessita' di destag
# - Mettere un controllo sul tracciato dei gscsv. Il gacsv  in uscita e' 
#   l'append di quelli dei vari segmenti, quindi o non c'e' header, oppure c'e'
#   un header per ciascun segmento.
#
#                                             Versione 1.11.3, Enrico 16/02/2018
#-------------------------------------------------------------------------------
#set -x
#set -e

#-------------------------------------------------------------------------------
# Scrive a schermo l'help della procedura
function write_help
{
#       123456789012345678901234567890123456789012345678901234567890123456789012345
  echo ""
  echo "Uso: ak_seriet.ksh PROG DATASET"
  echo "    [-inpdata=arkimet/file/filelist] [-reqdata=pts/ptn[,dsc][,vl]]"
  echo "    [-split=LENGHT] [-destag] [-decum] [-fop] [-zoom] [-fis=FILE] [-noconv]"
  echo "    [-arc=URL] [-debug] [-tdeb] [-mon=FILE] [-h]"
  echo ""
  echo "PROG      nome del progetto (determina i nomi dei files di input e output)"
  echo "DATASET   nome della fonte dei dati da cui estrarre"
  echo ""
  echo "-inpdata  tipo della fonte dei dati (DATASET)"
  echo "          * arkimet (def.): dataset Arkimet sul server generale SIMC;"
  echo "                            richiede la presenza del file PROG.akq;"
  echo "                            per query multi-dataset, sintassi DS1%DS2"
  echo "          * file:           un file grib (nella dir. corrente)"
  echo "          * filelist:       un file con i nomi di molti files grib (uno per"
  echo "                            riga, nella dir corrente)"
  echo "-reqdata  elenco di chiavi separate da virgole. Permette di determinare il"
  echo "          contenuto dei files in output, a prescindere dai dati estratti."
  echo "          I valori corrispondenti alle chiavi specificate vengono letti da"
  echo "          file, gli altri sono desunti dai dati estratti. Default: pts"
  echo "          * pts : legge i punti richiesti dal file PROG.pts.csv"
  echo "          * ptn : legge i punti richiesti dal file PROG.ptn (obsoleto)"
  echo "          * vl  : legge var e liv richiesti dal file PROG.varliv.csv"
  echo "          * dsc : legge time e trange richiesti dal file PROG.datascad.csv"
  echo ""
  echo "-split=LENGHT: fissa la lunghezza dei segmenti in cui lo stream GRIB viene" 
  echo "          suddiviso per essere elaborato. Puo' essere specificato piu' volte"
  echo "          Valori gestiti: none, hour, day, month, nMB, trange "
  echo "          (default: hour + 20MB; con opzione -destag, solo hour)"
  echo "-destag   interpola su griglia H i dati su grigle U e V (usare se e solo "
  echo "          se si estraggono vento sui model layers o flusso di momento COSMO)"
  echo "-decum    restituisce i parametri cumulati e medi come valori relativi all'"
  echo "          ora precedente (utile solo con dati superficiali previsti)"
  echo "-fop      come prima operazione converte al primo ordine i grib1 con "
  echo "          compressione al secondo ordine (fissa un baco GRIB-API; usare"
  echo "          solo se sono richiesti qcr o qis dal dataset LAMAZ)"
  echo ""
  echo "-zoom     come prima operazione sui grib estratti, li ritaglia sull'area"
  echo "          specificata nella prima riga del file PROGETTO.zoom" 
  echo "-fis=FILE: legge da FILE orografia, albedo, roghness e quota livelli. FILE"
  echo "          e' in formato grib, e definito sulla stessa area di DATASET"
  echo "-noconv   ritorna i dati cosi' come sono nel grib, senza normalizzarli alle"
  echo "          unita' standard DBalle. Necessario per estrarre parametri non"
  echo "          \"standard\", ie. non inclusi nei files dballe.txt e vargrib2bufr.csv"
  echo ""
  echo "-arc=URL  accede al server Arkimet specificato:"
  echo "          archivio centrale SIMC: http://arkimet.metarpa:8090 (default)"
  echo "          archivio backup maialinux: http://maialinux.metarpa:8090"
  echo "          archivio locale MA su radicchio: /scratch/eminguzzi/local_arkimet"
  echo ""
  echo "-debug    non cancella i files temporanei (dir $TEMP/akq.*)"
  echo "-tdeb     visualizza i tempi di esecuzione delle singole elaborazioni"
  echo "-mon=FILE scrive su FILE le date estratte, man man che vengono elaborate"
  echo "-h:       visualizza questo help"
  echo ""
  echo "Per usare versioni di lavoro di programmi e tabelle, esportare le variabili:"
  echo "          MA_UTILS_DAT, MA_UTILS_SVN, LIBSIM_DATA, DBA_TABLES, LIBSIM_SVN"
  echo ""
#       123456789012345678901234567890123456789012345678901234567890123456789012345
  return
}

#-------------------------------------------------------------------------------
# Nel caso di aree LM a 7 km, modifica gli estremi dell'area di zoom in modo che
# le coordinate estreme siano esatte con 3 decimali.
# Potrebbe fallire se la sottoarea coincidesse con gli estremi del grib

function adjust_zoom
{
echo $ds | grep -i ^LMSMR > /dev/null 2>&1
is_lm_ope=$?

if [ $is_lm_ope -eq 1 -o $ds = "lamaw" -o $ds = "lm7tmpc" ] ; then
  xreq="D"
  yreq="D"
elif [ $ds = "lamaz" ] ; then
  xreq="P"
  yreq="D"
else
  return
fi

ixd=`expr $ix / 2 \* 2`
iyd=`expr $iy / 2 \* 2`
fxd=`expr $fx / 2 \* 2`
fyd=`expr $fy / 2 \* 2`
echo "Aggiusto gli estremi dello zoom: "
echo "  Vecchi estremi: "$ix $iy $fx $fy

if [ \( $ix = $ixd -a $xreq = "D" \) -o \( $ix != $ixd -a $xreq = "P" \) ] ; then
 ix=`expr $ix - 1`
fi
if [ \( $iy = $iyd -a $yreq = "D" \) -o \( $iy != $iyd -a $yreq = "P" \) ] ; then
 iy=`expr $iy - 1`
fi
if [ \( $fx = $fxd -a $xreq = "D" \) -o \( $fx != $fxd -a $xreq = "P" \) ] ; then
 fx=`expr $fx + 1`
fi
if [ \( $fy = $fyd -a $yreq = "D" \) -o \( $fy != $fyd -a $yreq = "P" \) ] ; then
 fy=`expr $fy + 1`
fi
echo "  Nuovi estremi:  "$ix $iy $fx $fy

return
}

#===============================================================================
# 1) Preliminari

#-------------------------------------------------------------------------------
# 1.1) Path e utility

# 1.1.1 Assegno l'ambiente ma_utils
if [ -z $MA_UTILS_SVN ] ; then
  write_libsim_anag=/usr/libexec/ma_utils/write_libsim_anag.exe
  gacsv_scan=/usr/libexec/ma_utils/gacsv_scan.exe
  gacsv2seriet=/usr/libexec/ma_utils/gacsv2seriet.exe
  ptn2pts_csv=/usr/libexec/ma_utils/ptn2pts_csv.exe
  write_fisiog_csv=/usr/libexec/ma_utils/write_fisiog_csv.exe
  test_1grib=/usr/libexec/ma_utils/test_1grib.exe
  grib_s2f=/usr/libexec/ma_utils/grib_s2f.exe
  ma_grib1_grib2=/usr/libexec/ma_utils/ma_grib1_grib2.exe
else 
  echo "(ak_seriet.ksh) Eseguibili ma_utils: copia di lavoro in "$MA_UTILS_SVN
  write_libsim_anag=${MA_UTILS_SVN}/arkimet/src/write_libsim_anag.exe
  gacsv_scan=${MA_UTILS_SVN}/arkimet/src/gacsv_scan.exe
  gacsv2seriet=${MA_UTILS_SVN}/arkimet/src/gacsv2seriet.exe
  ptn2pts_csv=${MA_UTILS_SVN}/arkimet/src/ptn2pts_csv.exe
  write_fisiog_csv=${MA_UTILS_SVN}/arkimet/src/write_fisiog_csv.exe
  test_1grib=${MA_UTILS_SVN}/util/grib/src/test_1grib.exe
  grib_s2f=${MA_UTILS_SVN}/util/grib/src/grib_s2f.exe
  ma_grib1_grib2=${MA_UTILS_SVN}/util/grib/src/ma_grib1_grib2.exe
fi

# 1.1.2 Assegno l'ambiente LibSim
if [ -z $LIBSIM_SVN ] ; then
  getpoint=vg6d_getpoint
  subarea=vg6d_subarea  
  transform=vg6d_transform
else
  echo "(ak_seriet.ksh) Eseguibili libsim: copia di lavoro in "$LIBSIM_SVN
  getpoint=${LIBSIM_SVN}/bin/vg6d_getpoint
  subarea=${LIBSIM_SVN}/bin/vg6d_subarea
  transform=${LIBSIM_SVN}/bin/vg6d_transform
fi

# 1.1.3 Segnalo eventuali variabili d'ambiente assegnate a valori non di default
if [ ! -z $MA_UTILS_DAT ] ; then
  echo "(ak_seriet.ksh) Tabelle ma_utils: copia di lavoro in "$MA_UTILS_DAT
fi
if [ ! -z $LIBSIM_DATA ] ; then
  echo "(ak_seriet.ksh) Tabelle libsim: copia di lavoro in "$LIBSIM_DATA
fi
if [ ! -z $DBA_TABLES ] ; then
  echo "(ak_seriet.ksh) Tabelle dballe: copia di lavoro in "$DBA_TABLES
fi

# Directory in cui cerco i files di input
input_dir=`pwd`

# Directory in cui scrivo i files di output
output_dir=`pwd`

#-------------------------------------------------------------------------------
# 1.2) Parametri da riga comando

# Default
inpdata="arkimet"
reqpt="pts"
reqdsc="N"
reqvl="N"
filefis=""
reqsplit="def"
def_split="--max-size=20MB --time-interval=hour"
#def_split_destag="--max-size=20MB --time-interval=hour"
def_split_destag="--time-interval=hour"
fis="N"
zoom="N"
destag="N"
decum="N"
debug="N"
tdeb="N"
filemon=""
mon="N"
akurl="http://arkimet.metarpa:8090"
local_aliases=/autofs/nethomes/eminguzzi/arkimet/dat/match-alias.conf.local
force_fop="N"

if [ $# -eq 0 ] ; then
  write_help
  exit 1
fi
getpoint_opt=""
split_opt=""
seriet_opt=""
mand_par=0
while [ $# -ge 1 ] ; do
  if [ $1 = '-h' ] ; then
    write_help
    exit 1
  elif [ `echo $1 | awk '{print substr($1,1,8)}'` = '-inpdata' ] ; then
    inpdata=`echo $1 | cut -d = -f 2`
    shift
  elif [ `echo $1 | awk '{print substr($1,1,8)}'` = '-reqdata' ] ; then
    echo $1 | grep ptn > /dev/null
    if [ $? -eq 0 ] ; then
      reqpt="ptn"
    fi
    echo $1 | grep dsc > /dev/null
    if [ $? -eq 0 ] ; then
      reqdsc="Y"
    fi
    echo $1 | grep vl > /dev/null
    if [ $? -eq 0 ] ; then
      reqvl="Y"
    fi
    shift
  elif [ `echo $1 | awk '{print substr($1,1,4)}'` = '-fis' ] ; then
    fis="Y"
    filefis=`echo $1 | cut -d = -f 2`
    shift
  elif [ `echo $1 | awk '{print substr($1,1,6)}'` = '-split' ] ; then
    split=`echo $1 | cut -d = -f 2`
    if [ $split = "none" ] ; then
      reqsplit="no"
      shift
    elif [ $split = "hour" -o $split = "day" -o $split = "month" ] ; then
      reqsplit="usr"
      split_opt=${split_opt}" --time-interval="$split
      shift
    elif [ $split = "trange" ] ; then
      reqsplit="usr"
      split_opt=${split_opt}" --split-timerange"
      shift
    elif [ `echo $split | awk '{print match($1,"MB")}'` -gt 0 ] ; then
      reqsplit="usr"
      split_opt=${split_opt}" --max-size="$split
      shift
    else
      echo "split="$split" non gestito"
      exit 1
    fi
  elif [ $1 = '-zoom' ] ; then
    zoom="Y"
    shift
  elif [ $1 = '-noconv' ] ; then
    getpoint_opt="--noconvert"
    seriet_opt="-noconv"
    shift
  elif [ $1 = '-destag' ] ; then
    destag="Y"
    shift
  elif [ $1 = '-decum' ] ; then
    decum="Y"
    shift
  elif [ $1 = '-debug' ] ; then
    debug="Y"
    shift
  elif [ `echo $1 | awk '{print substr($1,1,4)}'` = '-arc' ] ; then
    akurl=`echo $1 | cut -d = -f 2`
    shift
  elif [ $1 = '-tdeb' ] ; then
    tdeb="Y"
    shift
  elif [ `echo $1 | awk '{print substr($1,1,4)}'` = '-mon' ] ; then
    mon="Y"
    filemon=`echo $1 | cut -d = -f 2`
    shift
  elif [ $1 = '-fop' ] ; then
    force_fop="Y"
    shift
  elif [ $mand_par -eq 0 ] ; then
    prog=$1
    mand_par=1
    shift
  elif [ $mand_par -eq 1 ] ; then
    ds=$1
    mand_par=2
    shift
  else
    echo "Skip parametro non gestito "$1
    shift
  fi
done

if [ $mand_par -ne 2 -o \( $inpdata != "arkimet" -a $inpdata != "file" \
     -a $inpdata != "filelst" \) ] ; then
  write_help
  exit 1
fi

if [ $reqsplit = "no" ] ; then
  split_opt=""
elif [ $reqsplit = "def" ] ; then
  if [ $destag = "Y" ] ; then
    split_opt=$def_split_destag
  else  
    split_opt=$def_split
  fi
fi

# 01/06/2018: nell'archivo arkimet adesso i nomi dei dataset mescolano maiuscole e
# minuscole (es: cosmo_5M_vol_ita), quindi tolgo il cambio di caso da qui e da
# ak_seriet.ksh
# E' possibile ci siano problemi estraendo da alcuni dataset. Alternativa: lasicare
# la converisone, escludendo i dataset che mescolano maiuscole e minuscole.

# Maiuscole/minuscole nel nome del dataset (arkimet: minusc.; maialinux: maiusc.)
# if [ $inpdata = "arkimet" ] ; then
#   if [ `echo $akurl | grep "arkimet.metarpa" | wc -l` -eq 1 ] ; then   # arkimet
#     ds=`echo $ds | tr '[:upper:]' '[:lower:]'`
#   elif [ `echo $ds | grep "maialinux.metarpa" | wc -l` -eq 1 ] ; then  # maialinux
#     ds=`echo $akurl | tr '[:lower:]' '[:upper:]'`
#   fi 
# fi

#-------------------------------------------------------------------------------
# 1.3 Controlli etc.

# 1.3.1 Controllo la presenza dei files di input

inpok="Y"

# Query, dati grib
if [ $inpdata = "arkimet" ] ; then
  if [ ! -s ${input_dir}/${prog}.akq ] ; then
    echo "file di input non trovato: "${prog}.akq
    inpok="N"
  fi
elif [ $inpdata = "file" -o $inpdata = "filelist" ] ; then
  if [ ! -s ${input_dir}/${ds} -a ! -s $ds ] ; then
    echo "file di input non trovato: "$ds
    inpok="N"
  fi
fi

# Descrittori dei dati richiesti
if [ $reqpt = "pts" -a ! -s ${input_dir}/${prog}.pts.csv ] ; then
  echo "file di input non trovato: ${prog}.pts.csv"
  inpok="N"
fi
if [ $reqpt = "ptn" -a ! -s ${input_dir}/${prog}.ptn ] ; then
  echo "file di input non trovato: ${prog}.ptn"
  inpok="N"
fi
if [ $reqdsc = "Y" -a ! -s ${input_dir}/${prog}.datasca.csv ] ; then
  echo "file di input non trovato: ${prog}.datasca.csv"
  inpok="N"
fi
if [ $reqvl = "Y" -a ! -s ${input_dir}/${prog}.varliv.csv ] ; then
  echo "file di input non trovato: ${prog}.varliv.csv"
  inpok="N"
fi

# Altro
if [ $fis = "Y" ] ; then
  if [ -s ${input_dir}/${filefis} ] ; then
    filefis=${input_dir}/${filefis}
  elif [ ! -s $filefis ] ; then
    echo "File fisiografico non trovato " $filefis
    fis="N"
  fi
fi

if [ $zoom = "Y" -a ! -f ${input_dir}/${prog}.zoom ] ; then
  echo "file ${input_dir}/${prog}.zoom non trovato"
  inpok="N"
fi

if [ $inpok = "N" ] ; then
  exit 1
fi

if [ $destag = "N" -a $inpdata = "arkimet" -a \
    `echo $ds | grep -E "lamaz|lm7tmpc" | wc -l` -gt 0 ] ; then
  echo "***** WARNING: il dataset "$ds" potrebbe richiedere l'opzione -destag"
fi

echo "ak_seriet: inizio elaborazioni          "`date "+%Y%m%d %H:%M:%S"`

# 1.3.3 Disabilito proxy
if [ $inpdata = "arkimet" -a "X"$http_proxy != "X" ] ; then
  unset http_proxy
fi

# 1.3.4 Se estraggo da un archivio locale, definisco gli alias
is_local=`echo $akurl | grep "http" | wc -l`
if [ $is_local -eq 0 ] ; then
  echo "Richiesta estrazione da un DB locale, uso gli alias in "$local_aliases
  export ARKI_ALIASES=$local_aliases
fi

# 1.3.5 Se richiesto, attivo il log dei tempi di esecuzione
if [ $tdeb = "Y" ] ; then
  profiler=time
# profiler=valgrind
else
  profiler=""
fi

# 1.3.6 Costruisco l'identificativo unico dei files temporanei e la dir di 
#       lavoro; mi sposto nella dir di lavoro
if [ -z $AK_TEMP ] ; then
  AK_TEMP=$TEMP
fi

cd $AK_TEMP
qid=`mktemp -d akq.XXXXXX | cut -d . -f 2` 2>&1
work_dir=${AK_TEMP}/akq.${qid}

if [ -d $work_dir ] ; then
  cd $work_dir
  echo "Dir di lavoro dell'estrazione "$work_dir
else
  echo "Dir di lavoro dell'estrazione non trovata "$work_dir
  exit 2
fi

if [ $mon = "Y" ] ; then
  filemon=$filemon"."$qid 
  touch $filemon
  chmod go+r $filemon
  echo "Monitoraggio avanzamanto estrazione su "$filemon
fi

#-------------------------------------------------------------------------------
# 1.4 Se richiesto, suddivido la query multipla in singole query "rettangolari" 
#     (parsing del file .akq)

if [ $inpdata = "arkimet" ] ; then
  rm -f akq.proc query.*

# Filtro commenti e righe vuote
  grep -Ev "^ *$" ${input_dir}/${prog}.akq | grep -v ^! > akq.proc

# Filtro i separatori e scrivo le query in files separati
  cntq=1
  linesq=0
  while read line ; do
    echo $line | grep \# > /dev/null
    if [ $? -eq 0 ] ; then          # trovato separatore
      if [ $linesq -gt 0 ] ; then   # nuova query
        cntq=`expr $cntq + 1`
        linesq=0
      else                          # ignoro il blocco precedente (ha lunghezza 0)
        rm -f query.${cntq}
      fi
    else                            # riga valida, appendo alla query corrente
      echo $line >> query.${cntq}
      linesq=`expr $linesq + 1`
    fi
  done < akq.proc
  if [ $linesq -eq 0 ] ; then       # L'ultima query e' vuota, la elimino
    rm -f query.${cntq}
    cntq=`expr $cntq - 1`
  fi
fi

#-------------------------------------------------------------------------------
# 1.5 Costriusco i files di appoggio e calcolo le informazioni accessorie

# 1.5.1 Config 
if [ $inpdata = "arkimet" ] ; then
  rm -f ${ds}.conf
  nsep=`echo $ds | awk '{print gsub("%","###",$1)}'`
  cntds=0
  rm -f ${ds}.conf
  while [ $cntds -le $nsep ] ; do
    cntds=`expr $cntds + 1`
    dsc=`echo $ds | cut -d % -f $cntds`
    arki-mergeconf ${akurl}/dataset/${dsc} >> ${ds}.conf
    if [ $? -ne 0 ] ; then
      echo "Errore nell'accesso al dataset: ${akurl}/dataset/${dsc}"
      exit 3
    fi
  done
fi

# 1.5.2 Elenco dei punti richiesti in formato "Native Vol7D"
rm -f ${prog}_anag.v7d
if [ $reqpt = "pts" ] ; then
  $write_libsim_anag -csv ${input_dir}/${prog}.pts.csv ${prog}_anag.v7d
elif [ $reqpt = "ptn" ] ; then
  $write_libsim_anag -ptn ${input_dir}/${prog}.ptn ${prog}_anag.v7d
fi
if [ $? -ne 0 ] ; then
  echo "Errore write_libsim_anag"
  exit 8
fi

# 1.5.3 Indici estremi dello zoom (se richiesto)
# Leggo gli indici estremi dal file PROGETTO.zoom
# Per evitare problemi al destag, aggiungo una cornice e mi assicuro che le 
# coordinate esterme dell'area di ritaglio siano esatte con 3 decimali (il 
# problema si pone per le aree LM a 7 km).
# Attualmente la funzione adjust_zoom potrebbe fallire se qualche estremo della
#  sottoarea coincidesse con il bordo del grib.

if [ $zoom = "Y" ] ; then
  rec=`head -n 1 ${input_dir}/${prog}.zoom`
  dum=`echo $rec | awk '{print $1}'`
  ix=`expr $dum - 1`
  dum=`echo $rec | awk '{print $2}'`
  iy=`expr $dum - 1`
  dum=`echo $rec | awk '{print $3}'`
  fx=`expr $dum + 1`
  dum=`echo $rec | awk '{print $4}'`
  fy=`expr $dum + 1`
  
  adjust_zoom
fi

# 1.5.4 Script per arki-xargs.ksh 
# I parametri dello script sono:
# - $1: file su cui appendere i risultati (gacsv)
# - $2: file input (segmento prodotto da arki-xargs, Grib)
# Files intermedi:
# - tmp1.grb: grib dopo il ritaglio
# - tmp2.grb: grib dopo destag
# - tmp3.grb: grib dopo decumulazione

cat <<EOF1 > xargs.ksh
cd ${work_dir}
rm -f tmp.gacsv tmp?.grb
EOF1

if [ $force_fop = "Y" ] ; then
  if [ ! -z $profiler ] ; then
    echo "echo inizio conversione FOP" >> xargs.ksh
  fi
  echo $profiler $grib_s2f \$2 tmp0.grb >> xargs.ksh
else
  echo ln -s \$2 tmp0.grb >> xargs.ksh
fi

if [ $zoom = "Y" ] ; then
  if [ ! -z $profiler ] ; then
    echo "echo inizio zoom" >> xargs.ksh
  fi
  echo $profiler $subarea --trans-type=zoom --sub-type=index \
    --ix=$ix --iy=$iy --fx=$fx --fy=$fy tmp0.grb tmp1.grb >> xargs.ksh
else
  echo ln -s tmp0.grb tmp1.grb >> xargs.ksh
fi

if [ $destag = "Y" ] ; then
  if [ ! -z $profiler ] ; then
    echo "echo inizio destag" >> xargs.ksh
  fi
  echo $profiler $transform --a-grid tmp1.grb tmp2.grb >> xargs.ksh
else
  echo ln -s tmp1.grb tmp2.grb >> xargs.ksh
fi

if [ $decum = "Y" ] ; then
  if [ ! -z $profiler ] ; then
    echo "echo inizio decumulazione" >> xargs.ksh
  fi
  echo $profiler arki-query --data \"timerange: Timedef,,254\" grib:tmp2.grb \> tmp3.grb >> xargs.ksh
  echo $profiler $transform --comp-stat-proc=0 --comp-step=\'0 01\' tmp2.grb tmpa.grb >> xargs.ksh
  echo $profiler $transform --comp-stat-proc=1 --comp-step=\'0 01\' tmp2.grb tmpc.grb >> xargs.ksh
  echo cat tmpa.grb tmpc.grb \>\> tmp3.grb >> xargs.ksh
else
  echo ln -s tmp2.grb tmp3.grb >> xargs.ksh
fi

if [ ! -z $profiler ] ; then
  echo "echo inizio getpoint" >> xargs.ksh
fi

cat <<EOF2 >> xargs.ksh
$profiler $getpoint --output-td=0 --coord-file=${prog}_anag.v7d \
  --trans-type=inter --sub-type=near --coord-format=native --output-format=grib_api_csv $getpoint_opt \
  --output-keys=gacsv:simpledate,gacsv:p1h,gacsv:p2h,gacsv:timerange,gacsv:level1,gacsv:l1,gacsv:l2,gacsv:centre,gacsv:category,gacsv:number,gacsv:npoint,gacsv:lon,gacsv:lat,gacsv:value,editionNumber \
  tmp3.grb tmp.gacsv
cat tmp.gacsv >> \$1
EOF2

if [ ! -z $profiler ] ; then
  echo "echo fine getpoint" >> xargs.ksh
fi
if [ $mon = "Y" ] ; then
  echo "tail -n 1 tmp.gacsv | cut -d , -f 1 >> $filemon" >> xargs.ksh
fi

chmod +x xargs.ksh

if [ $inpdata = "arkimet" ] ; then
  echo "Preliminari completati, query divisa in "$cntq" query rettangolari"
else
  echo "Preliminari completati"
fi

#===============================================================================
# 2) Elaborazioni

#-------------------------------------------------------------------------------
# 2.1 Scarico i grib da Arkimet ed estraggo i dati sui punti richiesti

if [ $inpdata = "arkimet" ] ; then
  cnt=1
  filesok=0
  gacsv_list=""
  while [ $cnt -le $cntq ] ; do
    echo "ak_seriet: inizio estrazione query "$cnt"    "`date "+%Y%m%d %H:%M:%S"`
  
#   Per ciscuna delle query rettangolari, estraggo i dati in formato gacsv
#    echo "arki-query --config=${ds}.conf --file=query.${cnt} --inline | arki-xargs ${split_opt} ./xargs.ksh ${prog}.gacsv.${cnt}"

    arki-query --config=${ds}.conf --file=query.${cnt} --inline  | \
      arki-xargs ${split_opt} ./xargs.ksh ${prog}.gacsv.${cnt} > \
      arki.log.${cnt} 2>&1

    if [ -s ${prog}.gacsv.${cnt} ] ; then
      filesok=`expr $filesok + 1`
      sort --temporary-directory=$AK_TEMP ${prog}.gacsv.${cnt} > tmp.gacsv
      mv tmp.gacsv ${prog}.gacsv.${cnt}

      if [ $filesok -eq 1 ] ; then
        gacsv_list=${prog}.gacsv.${cnt}
      else
        gacsv_list=${gacsv_list}" "${prog}.gacsv.${cnt}
      fi
  
#     Se l'elenco di variabili e livelli non e' stato fornito come input, 
#     incremento la lista dei var-liv in output
      if [ $reqvl = "N" -o $reqdsc = "N" ] ; then
        $profiler $gacsv_scan ${prog}.gacsv.${cnt} -r ${prog}.datasca.csv.${cnt} \
          -c ${prog}.varliv.csv.${cnt} -m $ds > gacsv_scan.log.$cnt
        rc=$?
        if [ $rc -eq 10 ] ; then
          echo "gacsv_scan: i dati richiesti non possono essere scritti in formato seriet"
          echo "file "${prog}.gacsv.${cnt}
          exit 9
        elif [ $rc -ne 0 ] ; then
          echo "Errore gacsv_scan, file "${prog}.gacsv.${cnt}
          exit 6
        fi
    
        if [ $reqvl = "N" ] ; then
          if [ $filesok -eq 1 ] ; then
            cp ${prog}.varliv.csv.${cnt} ${prog}.varliv.csv
          else
            tail -n +2 ${prog}.varliv.csv.${cnt} >> ${prog}.varliv.csv
          fi
        fi
        if [ $reqdsc = "N" ] ; then
          if [ $filesok -eq 1 ] ; then
            cp ${prog}.datasca.csv.${cnt} ${prog}.datasca.csv
          else
            tail -n +2 ${prog}.datasca.csv.${cnt} >> ${prog}.datasca.csv
          fi
        fi
      fi

#   Questa query non ha estratto nulla
    else
      echo "Nessun dato estratto per la query "$cnt

    fi
    cnt=`expr $cnt + 1`
  done

#-------------------------------------------------------------------------------
# 2.2 In alternativa, estraggo dal(i) file(s) grib i dati sui punti richiesti

elif [ $inpdata = "file" -o $inpdata = "filelst" ] ; then

# Copio/costruisco il file con l'elenco dei files grib da elaborare
  if [ $inpdata = "filelst" ] ; then
    cp  ${input_dir}/$ds ./${prog}.lst
  elif [ $inpdata = "file" ] ; then
    echo $ds > ${prog}.lst
  fi

# Ciclo sui files grib
  cnt=0
  filesok=0
  gacsv_list=""
  while read line ; do
    if [ ! -s $line -a ! -s ${input_dir}/${line} ] ; then
      echo "File non trovato, skip "$line
    else

#     Linko il file nella dir di lavoro, con estensione .grib
      file=`basename $line`
      if [ -s ${input_dir}/${line} ] ; then      # File nella dir di lancio
        ln -s ${input_dir}/${line} ./${file}.grib
      elif [ -s $line ] ; then                   # Path assoluto
        ln -s $line ./${file}.grib
      fi

#     Se e' un GRIB1 UTM, lo converto a GRIB2
      en=`$test_1grib ${file}.grib | cut -d , -f 1`
      proj=`$test_1grib ${file}.grib | cut -d , -f 2`
      if [ $en = 1 -a $proj = "UTM" ] ; then
        echo "Input file in formato GRIB1 UTM, converto in GRIB2 "$line
        mv ${file}.grib tmp.grib
        $ma_grib1_grib2 tmp.grib ${file}.grib
        if [ $? -ne 0 ] ; then
          exit 5
        fi 
      fi

#     Se e' richiesta l'opzione destag ordino per refernce time
      if [ $destag = "Y" ] ; then
        echo "Ordino per reference time il file "${file}.grib
        mv ${file}.grib tmp.grib
        arki-scan --data --sort=reftime tmp.grib > ${file}.grib
      fi

#     Estraggo i valori sui punti richiesti
      cnt=`expr $cnt + 1`
      echo "ak_seriet: inizio elaborazione file "$cnt"   "`date "+%Y%m%d %H:%M:%S"`
      arki-query --inline "" ${file}.grib | \
        arki-xargs ${split_opt} ./xargs.ksh ${prog}.gacsv.${cnt} > \
        arki.log.${cnt} 2>&1
        sort --temporary-directory=$AK_TEMP ${prog}.gacsv.${cnt} > tmp.gacsv
        mv tmp.gacsv ${prog}.gacsv.${cnt}

      if [ -s ${prog}.gacsv.${cnt} ] ; then
        filesok=`expr $filesok + 1`
        gacsv_list=${gacsv_list}" "${prog}.gacsv.${cnt}

#       Se l'elenco di variabili e livelli non e' stato fornito come input,
#       incremento la lista dei var-liv in output
        if [ $reqvl = "N" -o $reqdsc = "N" ] ; then
          $profiler $gacsv_scan ${prog}.gacsv.${cnt} -r ${prog}.datasca.csv.${cnt} \
            -c ${prog}.varliv.csv.${cnt} -m "file" > gacsv_scan.log.$cnt
          rc=$?
          if [ $rc -eq 10 ] ; then
            echo "gacsv_scan: i dati richiesti non possono essere scritti in formato seriet"
            echo "file "${prog}.gacsv.${cnt}
            exit 9
          elif [ $rc -ne 0 ] ; then
            echo "Errore gacsv_scan, file "${prog}.gacsv.${cnt}
            exit 6
          fi

          if [ $reqvl = "N" ] ; then
            if [ $filesok -eq 1 ] ; then
              cp ${prog}.varliv.csv.${cnt} ${prog}.varliv.csv
            else
              tail -n +2 ${prog}.varliv.csv.${cnt} >> ${prog}.varliv.csv
            fi
          fi
          if [ $reqdsc = "N" ] ; then
            if [ $filesok -eq 1 ] ; then
              cp ${prog}.datasca.csv.${cnt} ${prog}.datasca.csv
            else
              tail -n +2 ${prog}.datasca.csv.${cnt} >> ${prog}.datasca.csv
            fi
          fi
        fi

      fi
    fi
  done < ${prog}.lst
  cntq=$cnt
fi

if [ $filesok -eq 0 ] ; then
  echo "Non ho trovato nessun dato, mi fermo"
  exit 4
fi

#-------------------------------------------------------------------------------
# 2.3 Converto in formato seriet

echo "ak_seriet: converto in formato seriet   "`date "+%Y%m%d %H:%M:%S"`

# 2.3.1 Se e' stato specificato il file fisiografico, ne estraggo i valori sui 
#       punti richiesti

if [ $fis = "Y" ] ; then
  $getpoint --coord-file=${prog}_anag.v7d --coord-format=native \
  --trans-type=inter --sub-type=near  --output-format=grib_api_csv \
  --output-keys=gacsv:simpledate,gacsv:p1h,gacsv:p2h,gacsv:timerange,gacsv:level1,gacsv:l1,gacsv:l2,gacsv:centre,gacsv:category,gacsv:number,gacsv:npoint,gacsv:lon,gacsv:lat,gacsv:value,editionNumber \
  $filefis ${prog}.fisiog.gacsv
  seriet_opt=${seriet_opt}" -f ${prog}.fisiog.gacsv"
fi

# 2.3.2 Se sono stati specificati dall'utente, raduno i descrittori di punti, 
#       var/liv, data/scad. Se l'elenco delle var/liv e'stato dedotto dai dati 
#       estratti, elimino i record doppi (quando lo stesso var/liv e'richiesto 
#       in due query)

if [ $reqpt = "ptn" ] ; then
  $ptn2pts_csv ${input_dir}/${prog}.ptn ./${prog}.pts.csv
  if [ $? -ne 0 ] ; then
    echo "Errore ptn2pts_csv"
    exit 8
  fi
elif [ $reqpt = "pts" ] ; then
  cp ${input_dir}/${prog}.pts.csv ./
fi
if [ $reqdsc = "Y" ] ; then
  cp ${input_dir}/${prog}.datasca.csv ./
fi
if [ $reqvl = "Y" ] ; then
  cp ${input_dir}/${prog}.varliv.csv ./
else
  mv ${prog}.varliv.csv tmp.csv
  while read line ; do
    grep $line ${prog}.varliv.csv > /dev/null 2>&1
    if [ $? -ne 0 ] ; then
      echo $line >> ${prog}.varliv.csv
    fi
  done < tmp.csv
fi

# Se c'e', copio la namelist
if [ -f ${input_dir}/gacsv2seriet.nml ] ; then
  cp ${input_dir}/gacsv2seriet.nml ./ 
else
  $gacsv2seriet -c
  cp gacsv2seriet.nml $input_dir 
fi

# Converto in formato seriet
$profiler $gacsv2seriet $seriet_opt ${prog}.pts.csv ${prog}.varliv.csv ${prog}.datasca.csv \
  $gacsv_list > gacsv2seriet.log 2>&1
  errc=$?
if [ $errc -ne 0 -a $errc -ne 101 -a $errc -ne 102 -a $errc -ne 103 ] ; then
  cat gacsv2seriet.log
  echo "Errore gacsv2seriet "$errc
  exit 7
fi
cat gacsv2seriet.log

#-------------------------------------------------------------------------------
# 2.4 Salvo i files di ouput, cancello la dir di lavoro e concludo

# 2.4.1 Costruisco il file di log complessivo
cnt=1
while [ $cnt -le $cntq ] ; do
  echo "##### Query "$cnt >> ${prog}.log
  echo "##### Estrazione dati (arki-query/arki-scan)" >> ${prog}.log
  cat arki.log.${cnt} >> ${prog}.log
  if [ -s gacsv_scan.log.${cnt} ] ; then
    echo "##### gacsv_scan" >> ${prog}.log
    cat gacsv_scan.log.${cnt} >> ${prog}.log
  fi
  cnt=`expr $cnt + 1`
done
cat gacsv2seriet.log >> ${prog}.log

# 2.4.2 Costruisco il file fisiografico
if [ $fis = "Y" ] ; then
  $write_fisiog_csv ${prog}.pts.csv ${prog}.fisiog.gacsv ${prog}_fisiog.csv
fi

# 2.4.3 Salvo i files di output nella dir. del progetto
mv qcnt.log ${prog}_qcnt.log
cp ${prog}.pts.csv ${prog}.varliv.csv ${prog}.datasca.csv ${prog}.log ${prog}_qcnt.log \
  ${prog}_anag.v7d $output_dir
for file in `ls seriet_???.txt` ; do
  cnt=`echo $file | awk '{print substr($1,8,3)}'`
  cp $file ${output_dir}/${prog}_punti_${cnt}.txt
done
if [ $fis = "Y" ] ; then
  cp ${prog}_fisiog.csv $output_dir
  cp ${prog}.fisiog.gacsv $output_dir
fi

# 2.4.4 Cancello la dir. di lavoro dell'estrazione
if [ $debug = "N" ] ; then
  echo "Cancello dir di lavoro "${AK_TEMP}/akq.${qid}
  rm -fR ${AK_TEMP}/akq.${qid}
  if [ $mon = "Y" ] ; then
    echo "Cancello "$filemon
    rm $filemon
  fi
fi
echo "Elaborazioni terminate                  "`date "+%Y%m%d %H:%M:%S"`
echo "Gli output sono in: "$output_dir

exit $errc
