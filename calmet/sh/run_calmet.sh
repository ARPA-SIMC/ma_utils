#!/bin/bash
###########################################################################
# run_calmet.ksh
#
# Procedura generica per lanciare il run di calmet 
# Uso: run_calmet.ksh PROGETTO [-h] [-lmtest]
#
# Input: prima di lanciare la procedura, la dir. $inp_dir deve esistere e 
# contenere i seguenti files: 
# - pre_calmet.inp     : info di controllo
# - calmet_$PROJ.inp   : opzioni per il run  
# - srq_surf_$PROJ.dat : anagr. staz. sup. richieste
# - srq_temp_$PROJ.dat : anagr. profili richiesti
# - geo_*.dat          : param. fisiografici (ev. stagionali), fmt. Celmet
#
# Se richesta estrazione Oracle:
# - prq_surf_$PROJ.dat : param. da usare per ciascuna staz. superficiale
#
# Se richesti campi di background LAM  (formato MM5.DAT)
# - grib2mm5_$PROJ.inp : opzioni per uso campi LAM
# - mm5orog_$PROJ.grb  : orografia del modello LAM
# - wt_$PROJ.dat       : pesi per interpolazione vento (usando LAM e oss.)
#
# Se richiesto uso dati LAMAZ come osservazioni 
# - lamaz_$PROJ.pts.csv
#
# NOTE:
# L'opzione -lmtest agisce tra l'estrazione dati e la scrittura dei files
#   up* e surf* -> funziona se 
#
# TODO:
# 2.2.1
# 2.3.1
#
#                                                 V9.0.0, Enrico 31/12/2015
###########################################################################
#set -x
set -u

#-------------------------------------------------------------------------
# Scrive a schermo l'help della procedura
function write_help
{
  echo "Uso: run_calmet.ksh PROG [-lmtest]"
  echo "In progetti_run deve esistere la subdir PROG, con tutt i files necessari"
  echo "con opzione -lmtest: introduce valori fittizi nei dati LAMAZ usati cone oss." 
  return
}

#--------------------------------------------------------------------------
# Crea il messaggio d'errore in chiaro per il log della procedura
function crea_err_txt
{
  c1=`echo "$input_err" | awk '{print substr($1,1,1)}'`
  c2=`echo "$input_err" | awk '{print substr($1,2,1)}'`
  c3=`echo "$input_err" | awk '{print substr($1,3,1)}'`
  c4=`echo "$input_err" | awk '{print substr($1,4,1)}'`
  c5=`echo "$input_err" | awk '{print substr($1,4,1)}'`

  input_err_msg=""
  if [ $c1 = "1" ] ; then
    input_err_msg=$input_err_msg" file mm5.dat;"
  fi
  if [ $c2 = "1" ] ; then
    input_err_msg=$input_err_msg" dati TEMP;"
  fi
  if [ $c3 = "1" ] ; then
    input_err_msg=$input_err_msg" dati sup.;"
  fi
  if [ $c4 = "1" ] ; then
    input_err_msg=$input_err_msg" file surf.dat;"
  fi
  if [ $c5 = "1" ] ; then
    input_err_msg=$input_err_msg" file calmet.inp;"
  fi

  return 
}
#--------------------------------------------------------------------------
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

###########################################################################
# 1) Elaborazioni preliminari

echo "*** RUN_CALMET.KSH ***"

#--------------------------------------------------------------------------
# 1.1) Gestione parametri

opt_lamaz=""
if [ $# -eq 0 ] ; then
  write_help
  exit
fi

while [ $# -ge 1 ] ; do
  if [ $1 = '-h' ] ; then
    write_help
    exit
  elif [ $1 = "-lmtest" ] ; then
    opt_lamaz="-test"
    shift
  else
    proj=$1
    shift
  fi
done

#--------------------------------------------------------------------------
# 1.2) Path e altri parametri costanti

proj_dir=~eminguzzi/calmet/progetti/${proj} # files statici del progett
dat_dir=~eminguzzi/calmet/dat/              # files costanti calmet
run_root=/scratch/eminguzzi/calmet/${proj}       # root esecuzione

out_dir=${run_root}/out/                         # prodotti finali
log_dir=${run_root}/log/                         # log
run_dir=${run_root}/run/                         # esecuzione calmet
oss_dir=${run_root}/oss/                         # archivio dati osservati

# Assegno l'ambiente ma_utils
if [ -z $MA_UTILS_SVN ] ; then
  ak_seriet=/usr/libexec/ma_utils/ak_seriet.sh
  ak_getgrib=/usr/libexec/ma_utils/ak_getgrib.sh
  grib2up_dat=/usr/libexec/ma_utils/grib2up_dat.exe
  rw_dat_calmet=/usr/libexec/ma_utils/rw_dat_calmet.exe
  rw_xout_calmet=/usr/libexec/ma_utils/rw_xout_calmet.exe
  grib23ddat=/usr/libexec/ma_utils/grib23ddat.exe
else 
  echo "(run_calmet.sh) Eseguibili ma_utils: copia di lavoro in "$MA_UTILS_SVN
  ak_seriet=${MA_UTILS_SVN}/arkimet/sh/ak_seriet.ksh
  ak_getgrib=${MA_UTILS_SVN}/arkimet/sh/ak_getgrib.ksh
  grib2up_dat=${MA_UTILS_SVN}/calmet/src/grib2up_dat.exe
  rw_dat_calmet=${MA_UTILS_SVN}/calmet/src/rw_dat_calmet.exe
  rw_xout_calmet=${MA_UTILS_SVN}/calmet/src/rw_xout_calmet.exe
  grib23ddat=${MA_UTILS_SVN}/calmet/src/grib23ddat.exe
fi

# Assegno l'ambiente LibSim
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

# Livelli LAMAZ da usare per pseudo-osservazioni
zlay_lamaz_2003=~eminguzzi/util/grib/lm_ope/LAMAZ_layers_20030402.grb
zlay_lamaz_2006=~eminguzzi/util/grib/lm_ope/LAMAZ_layers_20060126.grb
zlay_lamaz_2012=~eminguzzi/util/grib/lm_ope/LAMAZ_layers_20120606.grb

lay_lm_2003="GRIB1,110,035,036 or GRIB1,110,034,035 or GRIB1,110,033,034 or GRIB1,110,032,033 or GRIB1,110,031,032 or GRIB1,110,030,031 or GRIB1,110,029,030 or GRIB1,110,028,029 or GRIB1,110,027,028 or GRIB1,110,026,027 or GRIB1,110,025,026 or GRIB1,110,024,025 or GRIB1,110,023,024 or GRIB1,110,022,023 or GRIB1,110,021,022 or GRIB1,110,020,021 or GRIB1,110,019,020 or GRIB1,110,018,019 or GRIB1,110,017,018 or GRIB1,110,016,017 or GRIB1,110,015,016"
nlayb_2003="35"
nlayt_2003="15"

lay_lm_2006="GRIB1,110,040,041 or GRIB1,110,039,040 or GRIB1,110,038,039 or GRIB1,110,037,038 or GRIB1,110,036,037 or GRIB1,110,035,036 or GRIB1,110,034,035 or GRIB1,110,033,034 or GRIB1,110,032,033 or GRIB1,110,031,032 or GRIB1,110,030,031 or GRIB1,110,029,030 or GRIB1,110,028,029 or GRIB1,110,027,028 or GRIB1,110,026,027 or GRIB1,110,025,026 or GRIB1,110,024,025 or GRIB1,110,023,024 or GRIB1,110,022,023 or GRIB1,110,021,022 or GRIB1,110,020,021 or GRIB1,110,019,020 or GRIB1,110,018,019 or GRIB1,110,017,018"
nlayb_2006="40"
nlayt_2012="17"

lay_lm_2012=$lay_lm_2006
nlayb_2012=$nlayb_2006
nlayt_2012=$nlayt_2006

#--------------------------------------------------------------------------
# 1.3) Lettura pre_calmet.inp 

if [ ! -f ${proj_dir}/pre_calmet.inp ] ; then
  echo "File ${proj_dir}/pre_calmet.inp non trovato"
  exit
fi
cd $proj_dir

dummy=`tail -n +5 pre_calmet.inp | head -n +1 | cut -d \  -f 1`
datah_ini=`tail -n +6 pre_calmet.inp | head -n +1 | cut -d \  -f 1`
datah_fin=`tail -n +7 pre_calmet.inp | head -n +1 | cut -d \  -f 1`
run_len=`tail -n +8 pre_calmet.inp | head -n +1 | cut -d \  -f 1`
sav_dat=`tail -n +12 pre_calmet.inp | head -n +1 | cut -d \  -f 1`
sav_grb=`tail -n +13 pre_calmet.inp | head -n +1 | cut -d \  -f 1`
sav_xout=`tail -n +14 pre_calmet.inp | head -n +1 | cut -d \  -f 1`
sav_log=`tail -n +15 pre_calmet.inp | head -n +1 | cut -d \  -f 1`
sav_inp=`tail -n +16 pre_calmet.inp | head -n +1 | cut -d \  -f 1`
fill_miss=`tail -n +17 pre_calmet.inp | head -n +1 | cut -d \  -f 1`

model_exe=`tail -n +20 pre_calmet.inp | head -n +1 | cut -d \  -f 1`
model_ver=`tail -n +21 pre_calmet.inp | head -n +1 | cut -d \  -f 1`
fisi_stag=`tail -n +22 pre_calmet.inp | head -n +1 | cut -d \  -f 1`
inp_ready=`tail -n +23 pre_calmet.inp | head -n +1 | cut -d \  -f 1`

use_bckgr=`tail -n +25 pre_calmet.inp | head -n +1 | cut -d \  -f 1`
use_lamob=`tail -n +26 pre_calmet.inp | head -n +1 | cut -d \  -f 1`
use_temp=`tail -n +27 pre_calmet.inp | head -n +1 | cut -d \  -f 1`
use_supob=`tail -n +28 pre_calmet.inp | head -n +1 | cut -d \  -f 1`

temp_wind=`tail -n +30 pre_calmet.inp | head -n +1 | cut -d \  -f 1`
repl_temp=`tail -n +31 pre_calmet.inp | head -n +1 | cut -d \  -f 1`
db_table_=`tail -n +32 pre_calmet.inp | head -n +1 | cut -d \  -f 1`
hint_sup1=`tail -n +33 pre_calmet.inp | head -n +1 | cut -d \  -f 1`
hint_sup2=`tail -n +34 pre_calmet.inp | head -n +1 | cut -d \  -f 1`
force_sup=`tail -n +35 pre_calmet.inp | head -n +1 | cut -d \  -f 1`

data_ini=`echo $datah_ini | awk '{print substr($1,1,8)}'`
data_fin=`echo $datah_fin | awk '{print substr($1,1,8)}'`
ndays=$[($(date -d $data_fin +%s)-$(date -d $data_ini +%s))/86400+1]

#--------------------------------------------------------------------------
# 1.4) Elaborazioni e controlli sul contenuto di pre_calmet.inp

# Compatibilita' con run_calmet.ksh versione 6
if [ $use_supob -eq 2 ] ; then
  echo "Parametro obsoleto: uso oss. sup gia' estratte"
  echo "Cosidero come: \"uso oss. sup da stazione\" e \"dati di input gia estratti\""
  use_supob=1
  inp_ready=1
fi
if [ $sav_inp -eq 2 ] ; then
  echo "Warning: parametro \"salvo dati input\" = 2 obsoleto"
  echo "funziona lo stesso, ma sarebbe meglio usare pre_calmet.inp, V7"
  sav_inp=1
fi

# Valori dei parametri illegali 
if [ $dummy != $proj ] ; then
  echo "Il file pre_calmet.inp si riferisce al progetto "$dummy
  exit
fi

# Valori dei parametri non ancora gestiti
if [ $repl_temp -ne 0 ] ; then
  echo "Sostituzione TEMP mancanti con dati da modello non implementata"
  exit
fi
if [ $use_bckgr -eq 2 ] ; then
  echo "Uso campi ECMWF non implementato"
  exit
fi

# Combinazioni dei parametri illegali 
if [ $use_temp -eq 1 -a $use_lamob -eq 1 ] ; then
  echo "Opzioni illegali in pre_calmet.inp"
  echo "Impossibile usare simultaneamente radiosondaggi osservati e da modello"
  exit
fi
if [ $use_temp -eq 0 -a $use_lamob -eq 0 -a \
     $use_bckgr -eq 0 -a $model_exe != "nil" ] ; then
  echo "Opzioni illegali in pre_calmet.inp"
  echo "Specificare una fonte per i dati in quota"
  exit
fi
if [ $use_supob -eq 0 -a $use_lamob -eq 0 -a \
     $use_bckgr -eq 0 -a $model_exe != "nil" ] ; then
  echo "Opzioni illegali in pre_calmet.inp"
  echo "Specificare una fonte per i dati superficiali"
  exit
fi
if [ $use_lamob -eq 1 -a  $use_temp -eq 1 ] ; then
  echo "Opzioni illegali in pre_calmet.inp"
  echo "Impossibile simultaneamente TEMP e profili da modello"
  exit
fi
if [ $use_lamob -eq 1 -a $use_supob -eq 1 ] ; then
  echo "Warning: richieste simultaneamente oss. sup. da stazione e da modello"
  echo "Tecnicamente possibile, potrebbe non avere senso."
  exit
fi
if [ $use_bckgr -eq 1 -a $use_lamob -eq 1 ] ; then
  echo "Opzioni illegali in pre_calmet.inp"
  echo "Impossibile usare i dati LAMA simultaneamente come background e come oss."
  exit
fi

# Parametri calcolati a partire da pre_calmet.inp
if [ $use_bckgr -eq 0 -a $repl_temp -eq 0 ] ; then
  req_bckgr=0
else
  req_bckgr=1
fi

if [ $use_lamob -eq 1 ] ; then
  stept_req=1
elif [ $use_temp -eq 0 ] ; then
  stept_req=0
elif [ $use_temp -eq 1 ] ; then
  stept_req=6
fi

if [ $force_sup -eq 1 ] ; then
  opt_proc_st1a="-force"
else
  opt_proc_st1a=""
fi

if [ $temp_wind -eq 1 ] ; then
  opt_estra_temp_calmet="-nowind"
else
  opt_estra_temp_calmet=""
fi

if [ $use_lamob -eq 0 -a $use_temp -eq 0 -a $use_supob -eq 0 ] ; then
  opt_crea_calmet_inp="-noobs"
else
  opt_crea_calmet_inp=""
fi

if [ $model_ver -ge 60 ] ; then
  opt_calmet2grib="-v63"
else
  opt_calmet2grib=""
fi

#--------------------------------------------------------------------------
# 1.5) Controllo i files di appoggio e li copio nella dir di lavoro

# creo/ripulisco le dir
if [ ! -d $pro_dir ] ; then
  mkdir -p $pro_dir
  if [ $? -ne 0 ] ; then
    echo "Impossibilie creare "$pro_dir
    exit
  fi
fi
if [ ! -d $out_dir ] ; then
  mkdir -p $out_dir
  if [ $? -ne 0 ] ; then
    echo "Impossibilie creare "$out_dir
    exit
  fi
fi
if [ ! -d $log_dir ] ; then
  mkdir -p $log_dir
  if [ $? -ne 0 ] ; then
    echo "Impossibilie creare "$log_dir
    exit
  fi
fi
if [ ! -d $oss_dir ] ; then
  mkdir -p $oss_dir
  if [ $? -ne 0 ] ; then
    echo "Impossibilie creare "$oss_dir
    exit
  fi
fi
if [ ! -d $run_dir ] ; then
  mkdir -p $run_dir
  if [ $? -ne 0 ] ; then
    echo "Impossibilie creare "$run_dir
    exit
  fi
else
  rm -f "$run_dir"/*
fi

# controllo i singoli files di input
inpf[1]=pre_calmet.inp
inpf[2]=calmet_${proj}.inp
nfiles=2

if [ $use_supob -eq 1 -o $use_lamob -eq 1 ] ; then
  nfiles=`expr $nfiles + 1`
  inpf[$nfiles]=srq_surf_${proj}.lst
fi
if [ $use_temp -eq 1 -o $use_lamob -eq 1 ] ; then
  nfiles=`expr $nfiles + 1`
  inpf[$nfiles]=srq_temp_${proj}.lst
fi
if [ $use_supob -eq 1 ] ; then
  nfiles=`expr $nfiles + 1`
  inpf[$nfiles]=prq_surf_${proj}.lst
fi

if [ $fisi_stag -eq 0 -a $model_exe != "nil" ] ; then
  nfiles=`expr $nfiles + 1`
  inpf[$nfiles]=geo_${proj}_yea.dat
elif [ $model_exe != "nil" ] ; then
  nfiles=`expr $nfiles + 1`
  inpf[$nfiles]=geo_${proj}_mam.dat
  nfiles=`expr $nfiles + 1`
  inpf[$nfiles]=geo_${proj}_jja.dat
  nfiles=`expr $nfiles + 1`
  inpf[$nfiles]=geo_${proj}_son.dat
  nfiles=`expr $nfiles + 1`
  inpf[$nfiles]=geo_${proj}_djf.dat
fi

if [ $use_bckgr -ne 0 ] ; then
  nfiles=`expr $nfiles + 1`
  inpf[$nfiles]=grib23ddat_${proj}.inp.40lay
  nfiles=`expr $nfiles + 1`
  inpf[$nfiles]=grib23ddat_${proj}.inp.35lay
  nfiles=`expr $nfiles + 1`
  inpf[$nfiles]=static_${proj}.grb.40lay
  nfiles=`expr $nfiles + 1`
  inpf[$nfiles]=static_${proj}.grb.35lay
  nfiles=`expr $nfiles + 1`
  inpf[$nfiles]=${proj}.zoom
fi

if [ $use_bckgr -ne 0 -a \( $use_temp -eq 1 -o $use_supob -eq 1 \) ] ; then
  nfiles=`expr $nfiles + 1`
  inpf[$nfiles]=wt_${proj}.dat
fi

if [ $use_lamob -eq 1 ] ; then
  nfiles=`expr $nfiles + 1`
  inpf[$nfiles]=lamaz_$proj.pts.csv
  nplamaz=`wc -l lamaz_$proj.pts.csv`
  nfiles=`expr $nfiles + 1`
  inpf[$nfiles]=sup.varliv.csv
fi

cnt=1
while [ $cnt -le $nfiles ] ; do
  if [ ! -f ${inpf[$cnt]} ] ; then
    echo "File ""${inpf[$cnt]}"" non trovato"
    exit
  else
    cp ${inpf[$cnt]} $run_dir
  fi
  cnt=`expr $cnt + 1`
done

#--------------------------------------------------------------------------
# 1.6) Varie 

# Inizializzo il log statistico con record d'intestazione
if [ -f $log_dir/calmet_${proj}.sta ] ; then
  echo "" >> $log_dir/calmet_${proj}.sta
  echo "### run_calmet.ksh restarted ###" >> $log_dir/calmet_${proj}.sta
else
  cp $dat_dir/header.sta $log_dir/calmet_${proj}.sta
fi 

# Inizializzo il log dell'esito dei run
if [ -f $log_dir/daily_run.log ] ; then 
  echo "" >> $log_dir/daily_run.log
  echo "### run_calmet.ksh restarted ###" >> $log_dir/daily_run.log
else
  echo "run;err_code;err_msg" > $log_dir/daily_run.log
fi

# Se e' richiesta la sostituzione dei run mancanti, inizializzo il relativo log
if [ $fill_miss -eq 1 ] ; then 
  if [ -f $log_dir/replaced_run.log ] ; then
    echo "" >> $log_dir/replaced_run.log
  else
    echo "### run_calmet.ksh restarted ###" >> $log_dir/replaced_run.log
    echo "run_mancante;run_sostituto" > $log_dir/replaced_run.log
  fi
  last_ok="null"
fi

# Rinomino il file con i pesi per LAM 
if [ $use_bckgr -ne 0 -a \( $use_temp -eq 1 -o $use_supob -eq 1 \) ] ; then
  mv $run_dir/wt_${proj}.dat $run_dir/wt.dat
fi

echo "preliminari ok, inizio elaborazione di "$ndays" run calmet"

###########################################################################
# 2) Preparo i dati per l'input di Calmet (ciclo sui run da lanciare)
#

#--------------------------------------------------------------------------
# 2.0) Preliminari; gestisco dati di inputi gia' pronti

cd $run_dir
cday=0
while [ $cday -lt $ndays ] ; do
  cday=`expr $cday + 1`
  incr=`expr $cday - 1`
  data=$(date -d "$data_ini + ${incr}day" +%Y%m%d)

  echo "### Inizio elaborazione run del "$data"      "`date -u +"%d-%b-%Y %T"`

  # ripulisco la dir. di lavoro da (eventuali) files dei run precedenti
  rm -f *.st1a *.st2 up*.dat surf.dat mm5.dat mm5.grb # dati input
  rm -f calmet.dat calmet.grb calmet.inp calmet.lst   # output
  rm -f datical.* *.log                               # log
  rm -f surf_req_db.dat date_calmet.inp               # files di lavoro

  # calcolo le date estreme per run ed estrazione (date_estra_calmet.inp)
  $exe_dir/crea_date_calmet.exe $data $run_len $hint_sup2 date_calmet.inp

  # se richiesto, calcolo date per estrazione LAMAZ
  if [ $use_lamob -eq 1 -o $use_bckgr -gt 0 ] ; then
    str_reftime_24h=">="`date -d $data +%Y-%m-%d`" 00, <="`date -d $data +%Y-%m-%d`" 23"
    str_reftime_25h=">="`date -d $data +%Y-%m-%d`" 00, <="`date -d "$data + 1 day" +%Y-%m-%d`" 00"
  fi

# 2.0.1 Assegnazioni dipendenti dalla data richiesta (per model layers LAMA)
  if [ $data -lt 20060126 ] ; then
    nlay_lm=35
    nlayb=$nlayb_2003
    nlayt=$nlayt_2003
    lay_lm=$lay_lm_2003
    zlay_lamaz=$zlay_lamaz_35
  elif [ $data -lt 20120606 ] ; then
    nlay_lm=40
    nlayb=$nlayb_2006
    nlayt=$nlayt_2006
    lay_lm=$lay_lm_2006
    zlay_lamaz=$zlay_lamaz_2006
  else
    nlay_lm=40
    nlayb=$nlayb_2012
    nlayt=$nlayt_2012
    lay_lm=$lay_lm_2012
    zlay_lamaz=$zlay_lamaz_2012
  fi
  
  if [ $use_bckgr -eq 1 -a $data -lt 20060126 ] ; then
    grib23ddat_inp=grib23ddat_${proj}.inp.2003
    grb_static=static_${proj}.grb.2003
  elif [ $use_bckgr -eq 1 -a $data -lt 20120606 ] ; then
    grib23ddat_inp=grib23ddat_${proj}.inp.2006
    grb_static=static_${proj}.grb.2006
  elif [ $use_bckgr -eq 1 -a $data -ge 20120606 ] ; then
    grib23ddat_inp=grib23ddat_${proj}.inp.2012
    grb_static=static_${proj}.grb.2012
  elif [ $use_bckgr -eq 2 ] ; then
    grib23ddat_inp=grib23ddat_${proj}.inp.ecmwf
    grb_static=static_${proj}.grb.ecmwf
  fi

# 2.0.2 Se i dati di input sono gia' pronti, li scompatto, controllo che ci 
#       siano tutti e salto le sezioni da  2.1 a 2.4

  if [ $inp_ready -eq 1 ] ; then         
    file=$proj"_input_"$data".tar"
    cp ${oss_dir}/${file}.gz ./
    gunzip ${file}.gz
    tar -xvf $file > tar.log 2>&1

    if [ \( ! -s surf.dat -o ! -s up1.dat \) -a $use_bckgr -eq 0 ] ; then
      echo "Files osservazioni non trovati per la data "$data
      exit
    elif [ ! -s 3d.dat -a $use_bckgr -eq 1 ] ; then
      echo "File background LAM non trovato per la data "$data
      exit
    fi
  else

#--------------------------------------------------------------------------
# 2.1) Preparo i campi di background da modello

  if [ $use_bckgr -gt 0 ] ; then
    echo "Estraggo campi di background LAMAZ"

#   Estraggo i grib LAMAZ dall'archivio
    if [ $use_bckgr -eq 1 ] ; then 
      rm -f lamaz.akq lamaz.grb bckgr.grb
      cat <<EOF1 > lamaz.akq
reftime: $str_reftime_24h
product: t or u or v or pr or q
level: $lay_lm
#
reftime: $str_reftime_24h
product: t
level: g00
#
reftime: $str_reftime_24h
product: tp
level: g00
timerange: Timedef,0,,1h
EOF1
      $ak_getgrib lamaz lamaz >> getgrib_lamaz.log 2>&1

#     Destag, zoom, sort
      zoom_str=`head -n 2 ${proj}.zoom | tail -n 1`
      ix=$(echo $zoom_str | awk '{print $1}')
      iy=$(echo $zoom_str | awk '{print $2}')
      fx=$(echo $zoom_str | awk '{print $3}')
      fy=$(echo $zoom_str | awk '{print $4}')
      $transform --a-grid  lamaz.grb tmp1.grb >> getgrib_lamaz.log 2>&1
      $subarea --trans-type=zoom --sub-type=index \
         --ix=$ix --iy=$iy --fx=$fx --fy=$fy tmp1.grb tmp2.grb
      arki_scan grib:tmp2.grb --sort=day:reftime --data -o bckgr.grb

#   Copio i grib ECMWF nella dir. di lavoro
    elif [ $use_bckgr -eq 2 ] ; then 
      echo ""
    fi

#   Converto i grib in formato 3d.dat
    cp $grib23ddat_inp ./grib23ddat.inp
    $grib23ddat $grb_static bckgr.grb 3d.dat -utm 1 > grib23ddat.log
#   -fd date_calmet.inp

  fi

#--------------------------------------------------------------------------
# 2.2) Preparo le osservazioni superificiali (file surf.dat; dati da 
#      stazioni o LAMAZ)

# 2.2.1 Estraggo da Oracle le osservazioni superficiali e le elaboro
  if [ $use_supob -eq 1 ] ; then
    $exe_dir/crea_surf_req.exe srq_surf_${proj}.lst prq_surf_${proj}.lst \
      surf_req_db.dat
    $exe_dir/estra_surf_calmet.exe date_calmet.inp surf_req_db.dat $db_table
    $exe_dir/proc_st1a.exe date_calmet.inp surf_req_db.dat \
      $hint_sup1 $hint_sup2 $opt_proc_st1a
  fi

# 2.2.2 Estraggo i dati superficiali LAMAZ e li elaboro
  if [ $use_lamob -eq 1 ] ; then
    echo "Estraggo osservazioni superficiali LAMAZ"

    rm -f sup.akq sup.pts.csv seriet_???.txt seriet_surf.log
    cat <<EOF2 > sup.akq
reftime: $str_reftime_25h
product: u or v or t or td
level: g02 or g10
#
reftime: $str_reftime_25h
product: pr or tcc or cl or cm or ch 
level: g00
#
reftime: $str_reftime_25h
product: tp
level: g00
timerange: Timedef,0,,1h
EOF2

    ln -s lamaz_${proj}.pts.csv sup.pts.csv
    $ak_seriet -reqdata=pts,vl sup lamaz >> seriet_surf.log 2>&1

#   Li scrivo come se fossero osservazioni (formato .st2)
    tail -n +2 lamaz_${proj}.pts.csv > lamaz_${proj}.pts.csv.tmp
    cnt=1
    while read line ; do
      if [ $cnt -lt 10 ] ; then
        cnt2="00"$cnt
      elif [ $cnt -lt 100 ] ; then
        cnt2="0"$cnt
      else
        cnt2=$cnt
      fi
      kk=`echo $line | cut -d \, -f 6`
      if [ $kk -lt 10 ] ; then
        idk="0000"$kk  
      elif [ $kk -lt 100 ] ; then
        idk="000"$kk  
      elif [ $kk -lt 1000 ] ; then
        idk="00"$kk  
      elif [ $kk -lt 10000 ] ; then
        idk="0"$kk  
      else
        idk=$kk  
      fi

      $exe_dir/proc_seriet_surf.exe sup_punti_${cnt2}.txt 000_${idk}.st2 \
        $opt_lamaz >> proc_seriet_surf.log 2>&1
      cnt=`expr $cnt + 1`
    done < lamaz_${proj}.pts.csv.tmp
  fi

# Scrivo tutte le osservazioni su surf.dat (srq_surf_${proj}.lst deve 
# ovviamente contenere tutte le osservazioni, da stazione e da modello)

  if [ $use_supob -eq 1 -o $use_lamob -eq 1 ] ; then
    $exe_dir/crea_surf_dat.exe date_calmet.inp srq_surf_${proj}.lst
  fi

#--------------------------------------------------------------------------
# 2.3) Preparo le oss. in quota (estraggo da Oracle o prendo da dati LAM)

# 2.3.1 Estraggo da Oracle i radiosondaggi 
  if [ $use_temp -eq 1 ] ; then
    echo "Estraggo TEMP"

    $exe_dir/estra_temp_calmet.exe date_calmet.inp srq_temp_${proj}.lst \
      $repl_temp 3d.dat $opt_estra_temp_calmet

# 2.3.2 Estraggo i profili LAMAZ e li elaboro
  elif [ $use_lamob -eq 1 ] ; then
    echo "Estraggo profili LAMAZ"

    rm -f up_3d.akq up_3d.grb up_10m.akq up_10m.grb
    cat <<EOF3 > up_3d.akq
reftime: $str_reftime_25h
product: t or u or v or pr
level: $lay_lm
EOF3
    $ak_getgrib up_3d lamaz >> getgrib_prf.log 2>&1

    if [ $data -lt 20060126 ] ; then
      cat <<EOF4 > up_10m.akq
reftime: $str_reftime_25h
product: t or u or v
level: g02
#
reftime: $str_reftime_25h
product: pr
level: g00
EOF4
      $ak_getgrib up_10m lamaz >> getgrib_prf.log 2>&1
    fi

#   Li scrivo come se fossero osservazioni (formato up.dat)
    if [ $data -lt 20060126 ] ; then
      $grib2up_dat up_3d.grb $zlay_lamaz lamaz_$proj.pts.csv \
         srq_temp_${proj}.lst $nlayb $nlayt ${data}00 25 \
         -10m up_10m.grb $opt_lamaz > grib2up_dat.log
    else
      $grib2up_dat up_3d.grb $zlay_lamaz lamaz_$proj.pts.csv \
         srq_temp_${proj}.lst $nlayb $nlayt ${data}00 25 \
         $opt_lamaz > grib2up_dat.log
    fi

  fi

#--------------------------------------------------------------------------
# 2.4) se richiesto, salvo i dati di input

  if [ $sav_inp -eq 1 ] ; then
    file=$proj"_input_"$data".tar"
    tar -cvf $file surf.dat 3d.dat up*.dat > /dev/null 2>&1
    gzip $file
    mv $file".gz" $oss_dir
    echo "Archiviati dati di input"
  fi

  fi                                       # Fine costruzione dati di input

#--------------------------------------------------------------------------
# 2.5) costruisco il file di controllo (calmet.inp)

  $exe_dir/crea_calmet_inp.exe calmet_${proj}.inp date_calmet.inp \
    srq_surf_${proj}.lst srq_temp_${proj}.lst $stept_req $model_ver \
    $opt_crea_calmet_inp

###########################################################################
# 3) Se i dati lo permettono, lancio Calmet e smisto l'output

  if [ $model_exe != "nil" ] ; then

#--------------------------------------------------------------------------
# 3.1) Analisi dei dati in input

  if [ $use_lamob -eq 1 -o $use_temp -eq 1 -o $use_supob -eq 1 ] ; then
    $exe_dir/test_inp_calmet.exe $stept_req > /dev/null
    input_err=`head -n 1 datical.str | cut -c 1-5`
    crea_err_txt
    head -n 1 datical.str >> $log_dir/calmet_${proj}.sta
  else
    input_err=-999
  fi

#--------------------------------------------------------------------------
# 3.2) Seleziono il geo.dat stagionale, lancio Calmet

  if [ $fisi_stag -eq 0 ] ; then
    cp geo_${proj}_yea.dat geo.dat
  else
    mese=`tail -n +4 date_estra_calmet.inp | head -n +1 | cut -c 5-6`
    case $mese in
      (3|4|5)
      cp geo_${proj}_mma.dat geo.dat ;;
      (6|7|8)
      cp geo_${proj}_jja.dat geo.dat ;;
      (9|10|11)
      cp geo_${proj}_son.dat geo.dat ;;
      (12|1|2)
      cp geo_${proj}_djf.dat geo.dat ;;
    esac
  fi

# Se uso la versione 6 di Calmet, modifico l'header del geo.dat
  if [ $model_ver -gt 52 ] ; then
    rm -f tmp.geo
    tail -n +3 geo.dat > tmp.geo

    str_grid_in=`head -n 2 geo.dat | tail -n 1`
    nx=`echo $str_grid_in | awk '{print $1}'`
    ny=`echo $str_grid_in | awk '{print $2}'`
    dd=`echo $str_grid_in | awk '{print $3}'`
    x0=`echo $str_grid_in | awk '{print $4}'`
    y0=`echo $str_grid_in | awk '{print $5}'`

    intfill $nx 8
    str_grid_out=$str_out
    intfill $ny 8
    str_grid_out=${str_grid_out}${str_out}
    intfill $x0 12
    str_grid_out=${str_grid_out}${str_out}
    intfill $y0 12
    str_grid_out=${str_grid_out}${str_out}
    intfill $dd 12
    str_grid_out=${str_grid_out}${str_out}
    intfill $dd 12
    str_grid_out=${str_grid_out}${str_out}

    rm geo.dat
    echo "GEO.DAT"     > geo.dat
    echo "0"           >> geo.dat
    echo "UTM"         >> geo.dat
    echo "32  N"       >> geo.dat
    echo "WGS-84"      >> geo.dat
    echo $str_grid_out >> geo.dat
    echo "KM"          >> geo.dat
    cat tmp.geo        >> geo.dat
  fi

# Verifico la versione di calmet.inp
  head -n 1 calmet.inp | grep 2.1 > /dev/null
  new=$?
  if [ \( $model_ver -gt 52 -a $new -eq 1 \) -o \
       \( $model_ver -le 52 -a $new -eq 0 \) ] ; then
    echo "Errore, versione sbagliata di calmet.inp"
    exit
  fi

# Lancio Calmet
  echo "Lancio calmet per il giorno "$data"        "`date -u +"%d-%b-%Y %T"`
  rm -f calmet.end
  $exe_dir/$model_exe

#--------------------------------------------------------------------------
# 3.3) Aggiorno il log dell'esito dei run

  if [ -f calmet.end ] ; then
    echo "Simulazione completata con successo        "`date -u +"%d-%b-%Y %T"`
    err="0"
    err_msg="OK"
  else
    if [ $input_err -gt 0 ] ; then
      err=$input_err
      err_msg=$input_err_msg
    else
      err=999
      err_msg="Errore sconosciuto"
    fi
  fi
  echo $data";"$err";"$err_msg >> $log_dir/daily_run.log

#--------------------------------------------------------------------------
# 3.4) Se richiesto, copio l'output dell'ultimo giorno in cui calmet ha 
#      girato. Se ci sono run mancanti all'inizio della serie, li 
#      sostituisco con il primo che riesce a girare.

  if [ $fill_miss -eq 1 ] ; then

#   Se e' il 1o run che riesce a girare, recupero quelli dei giorni precedenti
    if [ -f calmet.end  -a $last_ok = "null" -a $cday -gt 1 ] ; then
      cnt=1
      while [ $cnt -lt $cday ] ; do
        incr=`expr $cnt - 1`

#       Costruisco il calmet.dat
        data_repl=$(date -d "$data_ini + ${incr}day" +%Y%m%d)
        $rw_dat_calmet calmet.dat calmet_repl.dat ${data_repl}01
        $rw_xout_calmet calmet_xout.dat calmet_xout_repl.dat ${data_repl}01
        echo ${data_repl}";"${data} >> $log_dir/replaced_run.log

#       Archivio
        if [ $sav_dat -eq 1 ] ; then
          cp calmet_repl.dat $out_dir/calmet_${data_repl}.dat
        fi
        if [ $sav_grb -ne 0 ] ; then
          $exe_dir/calmet2grib.exe calmet_repl.dat calmet.grb $sav_grb \
            -wcart $opt_calmet2grib
          cp calmet.grb $out_dir/calmet_${data_repl}.grb
          if [ $sav_xout -ne 0 ] ; then
            $exe_dir/xout2grib.exe calmet_xout_repl.dat calmet_xout.grb $sav_grb
            cat calmet_xout.grb >> $out_dir/calmet_${data_repl}.grb
          fi
        fi 

        cnt=`expr $cnt + 1`
      done
    fi

#   Gestione run appena eseguito
    if [ -f calmet.end ] ; then         # run completato con successo
      rm -f calmet_${last_ok}.dat
      last_ok=$data
      cp calmet.dat calmet_${last_ok}.dat
      cp calmet_xout.dat calmet_xout_${last_ok}.dat

    elif [ $last_ok != "null" ] ; then  # run non completato ma sostitibile
      $rw_dat_calmet calmet_${last_ok}.dat calmet.dat ${data}01
      $rw_xout_calmet calmet_xout_${last_ok}.dat calmet_xout.dat ${data}01
      echo ${data}";"${last_ok} >> $log_dir/replaced_run.log

    else                                # ancora nessun run completo
      rm -f calmet.dat calmet_xout.dat      

    fi

  fi

#--------------------------------------------------------------------------
# 3.5) Archivio output

# Se richiesto, archivio output in formato binario
  if [ $sav_dat -eq 1 -a -f calmet.dat ] ; then
    cp calmet.dat $out_dir/calmet_"$data".dat
  fi

# Se richiesto, archivio output in formato GRIB
  if [ $sav_grb -ne 0 -a -f calmet.dat ] ; then
    $exe_dir/calmet2grib.exe calmet.dat calmet.grb $sav_grb \
      -wcart $opt_calmet2grib
    cp calmet.grb $out_dir/calmet_"$data".grb
    if [ $sav_xout -ne 0 ] ; then
      $exe_dir/xout2grib.exe calmet_xout.dat calmet_xout.grb $sav_grb
      cat calmet_xout.grb >> $out_dir/calmet_"$data".grb
    fi

  elif [ $sav_grb -ne 0 -a ! -f calmet.end -a $fill_miss -eq 0 ] ; then
    $exe_dir/dummy_grib_calmet.exe calmet.inp $sav_grb calmet.grb
    cp calmet.grb $out_dir/calmet_"$data".grb

  fi

# Se richiesto, archivio log esteso
  if [ $sav_log -ne 0 ] ; then
    cp calmet.lst $log_dir/calmet_"$data".lst
    if [ $use_lamob -eq 1 -o $use_temp -eq 1 -o $use_supob -eq 1 ] ; then
      cp datical.dat $log_dir/datical_"$data".dat
    fi
    if [ $use_supob -eq 1 -o $use_supob -eq 2 ] ; then
      cp interp_st1a.log $log_dir/interp_st1a_"$data".log
    fi
    if [ $use_supob -eq 1 ] ; then
      cp estra_surf_calmet_qcnt.log $log_dir/qcnt_"$data".log
    fi
    if [ $repl_temp -ne 0 ] ; then
      cp estra_temp_calmet.log $log_dir/repl_temp_"$data".log
    fi
    if [ $use_bckgr -ne 0 ] ; then
      cp grib23ddat.log $log_dir/grib23ddat_"$data".log
      cp getgrib_lamaz.log $log_dir/getgrib_lamaz_"$data".log
    fi
    if [ $use_lamob -eq 1 -a $inp_ready -eq 0 ] ; then 
      cp seriet_surf.log $log_dir/seriet_surf_"$data".log
      cp getgrib_prf.log $log_dir/getgrib_prf_"$data".log
      cp proc_seriet_surf.log $log_dir/proc_seriet_surf_"$data".log
      cp grib2up_dat.log $log_dir/grib2up_dat_"$data".log
    fi
  fi

  fi                             # Fine sezione skippata se non giro calmet

done

#--------------------------------------------------------------------------
echo "run_calmet.ksh: elaborazioni terminate      "`date -u +"%d-%b-%Y %T"`
exit


