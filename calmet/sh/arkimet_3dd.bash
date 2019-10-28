#! /bin/bash
#-------------------------------------------------------------------------------
# Procedura per estrarre le analisi o le previsoni COSMO per un run Calmet, 
# riscrivendole in formato 3D.dat.
# Puo' essere lanciato da terminale oppure nell'ambiente AQweb
#
# TODO:
# Script in fase "alfa": verificato solo che con un giorno di dati LAMA5 gira e
# produce un output. Mancano:
# - verifica su periodo lungo
# - verifica su dati LAMAZ e previsti
# - verifica della lettura dell'output da parte di Calmet
# - verifica dell'interpolaazione tmeporale in caso di dati meteo mancanti
#
#                                               Versione 3.0.0 Enrico 28/10/2019
#-------------------------------------------------------------------------------
#set -x
set -eu

#-------------------------------------------------------------------------------
# Scrive a schermo l'help della procedura
function write_help
{
  echo ""
  echo "Uso: arkimet_3dd.bash dataset lon_min lat_min lon_max lat_max"
  echo "[-fis file_fis] [-skip N] [-c] [-h]"
  echo "Estrae da arkimet e costruisce l'input meteo 3D.DAT per un run calmet"
  echo ""
  echo "Input (nella directory corrente):"
  echo "- 3dd.akq (query arkimet): deve essere gia' presente (opzione -c costruisce un esempio)"
  echo "- grib23ddat.inp (namelist): deve essere gia' presente (opzione -c costruisce un esempio)"
  echo "- fisiog.grb.${dataset}: formato grib; deve contenere: orografia, quota model layers."
  echo "  Con opzione -fis viene linkato il file specificato, altrimenti viene costruito dallo"
  echo "  script a partire da dati in  /usr/share/ma_utils/ o $MA_UTILS_DAT"
  echo ""
  echo "dataset:  dataset arkimet da cui estrarre i dati:" 
  echo "  lama5_arc, lama5: analisi Cosmo 5Km (da 16/06/2018)"
  echo "  lamaz: analisi Cosmo 7Km (fino a 31/12/2019)"
  echo "  cosmo_5M_vol_ita: forecast Cosmo 5 Km (ultimi 5 giorni)"
  echo "lon_min lat_min lon_max lat_max: estremi dell'area da ritagliare"
  echo ""
  echo "-skip N   sfoltisce la griglia del modello in input (un punto ogni N)"
  echo "-c        costruisce templates di 3dd.akq e grib23ddat.inp per il dataset specificato"
  echo "-h        visualizza questo help e termina"
  return
}

#-------------------------------------------------------------------------------
# Scrive query e namelist d'esempio

function write_tmpl
{
  ds=$1
  rm -f grib23ddat.inp 3dd.akq
  
# Parametri dipendenti dal dataset
  if [ $ds = "lamaz" ] ; then
    str_reft=">=2019-06-30 23, <=2019-07-01 23"
    tipo_sca=1
    nlev=20
    lev_list="40,39,38,37,36,35,34,33,32,31,30,29,28,27,26,25,24,23,22,21"

  elif [ $ds = "lama5" -o $ds = "lama5_arc" ] ; then
    str_reft=">=2019-06-30 23, <=2019-07-01 23"
    tipo_sca=1
    nlev=24
    lev_list="45,44,43,42,41,40,39,38,37,36,35,34,33,32,31,30,29,28,27,26,25,24,23,22"

  elif [ $ds = "cosmo_5M_vol_ita" ] ; then
    str_reft="=2019-07-01"
    tipo_sca=2
    nlev=24
    lev_list="45,44,43,42,41,40,39,38,37,36,35,34,33,32,31,30,29,28,27,26,25,24,23,22"

  else
    echo "Dataset "$ds" non gestito"
    return

  fi  

# Stringa per query sui livelli
  first="Y"
  ll=$(echo $lev_list | sed 's/,/\ /g')
  for lev in $ll ; do
    levp1=$[$lev+1]
    l1=$(printf "%03d" $lev)
    l2=$(printf "%03d" $levp1)
    if [ $first = "Y" ] ; then
      str_lev="GRIB1,110,${l1},${l2}"
      first="N"
    else
      str_lev=${str_lev}" or GRIB1,110,${l1},${l2}"
    fi
  done
  
# Costruisco namelist
  cat <<EOF > grib23ddat.inp
${tipo_sca}        ! Tipo di scadenze (1: analisi, 2: previsioni)
24       ! Numero di scadenze da elaborare
${nlev}       ! Numero e lista dei livelli verticali (chiave "topLevel"; ordinati dal basso)
${lev_list}
1        ! input H2O 3d (0:none, 1:Q, 2:Q+cloud water, 3:Q+cloud water+cloud ice
0        ! Time zone degli orari in output (0: ore GMT; /=0: LST; 1: LST Italia)
3        ! formato in output (2: versione 2.1; 3: versione 3)
EOF

# Costruisco query arkimet  
  cat <<EOF > 3dd.akq
reftime: ${str_reft}
product: t or u or v or pr or q
level: ${str_lev}
# 
reftime: ${str_reft}
product: t or tp
level: g00
EOF

  return
}

#-------------------------------------------------------------------------------
# 1) Preliminari

# Assegno l'ambiente ma_utils
if [ -z $MA_UTILS_SVN ] ; then
  ak_getgrib=/usr/libexec/ma_utils/ak_getgrib.ksh
  split_grib_par=/usr/libexec/ma_utils/split_grib_par.exe
  tinterp_grib=/usr/libexec/ma_utils/tinterp_grib.exe
  grib23ddat=/usr/libexec/ma_utils/grib23ddat.exe
else 
  echo "(arkimet_3dd.bash) Eseguibili ma_utils: copia di lavoro in "$MA_UTILS_SVN
  ak_getgrib=${MA_UTILS_SVN}/arkimet/sh/ak_getgrib.ksh
  split_grib_par=${MA_UTILS_SVN}/util/grib/src/split_grib_par.exe
  tinterp_grib=${MA_UTILS_SVN}/util/grib/src/tinterp_grib.exe
  grib23ddat=${MA_UTILS_SVN}/calmet/src/grib23ddat.exe
fi

if [ -z $MA_UTILS_DAT ] ; then
  fis_dir=/usr/share/ma_utils/
else 
  echo "(arkimet_3dd.bash) Files statici ma_utils: copia di lavoro in "$MA_UTILS_DAT
  fis_dir=${MA_UTILS_DAT}
fi

# Assegnazioni in ambiente AQweb
# fis_dir=/home/maope/www/aqweb/arkimet/data
# export AK_TEMP=/autofs/calmet/aqweb/arkimet

# Assegnazioni in ambiente di sviluppo
file_mon=arkimet_3dd.log

# Parametri da riga comando
run="Y"
nskip=0
mand_par=0
file_fis="nil"
file_nml="grib23ddat.inp"
file_query="3dd.akq"
dataset="nil"

if [ $# -eq 0 ] ; then
  write_help
  exit 1
fi
while [ $# -ge 1 ] ; do
  if [ $1 = '-h' ] ; then
    write_help
    exit 1
  elif [ $1 = '-c' ] ; then
    run="N"
    shift
  elif [ $1 = "-skip" ] ; then
    shift
    nskip=$1
    shift
  elif [ $1 = "-fis" ] ; then
    shift
    file_fis=$1
    shift
  elif [ $mand_par -eq 0 ] ; then
    dataset=$1
    mand_par=1
    shift
  elif [ $mand_par -eq 1 ] ; then
    lon_min=$1
    mand_par=2
    shift
  elif [ $mand_par -eq 2 ] ; then
    lat_min=$1
    mand_par=3
    shift
  elif [ $mand_par -eq 3 ] ; then
    lon_max=$1
    mand_par=4
    shift
  elif [ $mand_par -eq 4 ] ; then
    lat_max=$1
    mand_par=5
    shift
  fi
done

if [ $run = "N" ] ; then
  if [ $dataset = "nil" ] ; then
    echo "Occorre specificare un dataset (lamaz, lama5_arc, lama5, cosmo_5M_vol_ita)"
    exit 1
  else
    write_tmpl $dataset
    exit 0
  fi
fi

# Controlli sui parametri
if [ $mand_par -ne 5 ] ; then
  write_help
  exit 1
fi
if [ ! -s 3dd.akq ] ; then
  echo "file 3dd.akq non trovato"
  exit
fi
if [ ! -s grib23ddat.inp ] ; then
  echo "file grib23ddat.inp non trovato"
  exit
fi
if [ $file_fis != "nil" -a ! -s $file_fis ] ; then
  echo "file $file_fis non trovato"
  exit
fi
if [ $dataset != "lamaz" -a $dataset != "lama5" -a $dataset != "lama5_arc" \
  -a $dataset != "cosmo_5M_vol_ita" ] ; then
  echo "Dataset $dataset non gestito (il nome del dataset e' case sensitive!)"
  exit 1
fi
if [ $dataset = "lama5" ] ; then
  echo "Warning: il dataset lama5 contiene solo i dati degli ultimi giorni!"
fi
  
# Nome area per files fisiografici
if [ $dataset = "lamaz" ] ; then
  dsarea="LAMAZ"
  split_opt=""
elif [ $dataset = "lama5_arc" -o $dataset = "lama5" ] ; then
  dsarea="LAMA5"
  split_opt="-destag -split=hour"
elif [ $dataset = "cosmo_5M_vol_ita" ] ; then
  dsarea="COS5MITA"
  split_opt="-destag -split=trange"
fi

#-------------------------------------------------------------------------------
# 2) Esecuzione

# 2.1 Cancello eventuali files temporanei di estrazioni precedenti
rm -f 3dd.grb 3dd.miss.grb 3dd.query.* sg*.grb *.conf $file_mon fisiog.grb* 
rm -f date.txt tmp1.txt tmp2.txt split_grib_par.log grib23ddat.log

# 2.2 Se non specificato, costruisco i file fisiografico per il dataset richiesto
if [ $file_fis = "nil" ] ; then
  rm -f fisiog.grb.${dataset}
  cp ${fis_dir}/${dsarea}_orog.grb fisiog.grb.${dataset}
  cat ${fis_dir}/${dsarea}_layers.grb >> fisiog.grb.${dataset}
elif [ $file_fis != fisiog.grb.${dataset} ] ; then
  ln -s $file_fis fisiog.grb.${dataset}
fi

# 2.3 Estrazione i dati
echo "Estraggo i dati. Comando:"
echo "$ak_getgrib 3dd $dataset -zoom=$lon_min,$lat_min,$lon_max,$lat_max $split_opt -mon=$file_mon"
$ak_getgrib 3dd $dataset -zoom=$lon_min,$lat_min,$lon_max,$lat_max $split_opt -mon=$file_mon

if [ $nskip -gt 1 ] ; then
  mv 3dd.grb 3dd.grb.full
  vg6d_transform --trans-mode=s --trans-type=boxregrid --sub-type=average \
    --npx=$nskip --npy=$nskip 3dd.grb.full 3dd.grb
fi

# 2.4 Controllo se ci sono istanti mancanti; se necessario, li interpolo nel tempo
# I dati di pioggia (gli unici non istantanei) restano con timeRangeIndicator=13,
# che (al momento) non e' gestito da tinterp_grib...

if [ $dataset = "lamaz" -o $dataset = "lama5_arc" -o $dataset = "lama5" ] ; then
  echo "Verifico se ci sono istanti mancanti"
  rm -f tmp1.txt tmp2.txt dates.txt
  grib_get -P dataDate,hour 3dd.grb | sort | uniq > tmp1.txt
  while read line ; do
    date=$(echo $line | awk '{print $1}')
    h1=$(echo $line | awk '{print $2}')
    h2=$(printf "%02d" $h1)
    echo ${date}" "${h2} >> tmp2.txt
  done < tmp1.txt
  sort tmp2.txt > dates.txt
  
  nist_file=$(wc -l dates.txt | awk '{print $1}')
  datah1=$(head -n 1 dates.txt)
  datah2=$(tail -n 1 dates.txt)
  datah1s=$(date -d "$datah1" +%s)
  datah2s=$(date -d "$datah2" +%s)

  nist_req=$[($datah2s-$datah1s)/3600+1]
  
#  data_ini=`head -n 1 dates.txt | awk '{print $1}'`
#  data_ini_sec=`date -d $data_ini +%s`
#  data_end=`tail -n 1 dates.txt | awk '{print $1}'`
#  data_end_sec=`date -d $data_end +%s`
#  nist_req=$[($data_end_sec-$data_ini_sec) / 3600 + 24 ]   ######
  
  if [ $nist_file -lt $nist_req ] ; then
    echo "Trovati ${nist_file} istanti su ${nist_req}: richiesta interpolazione temporale"
    mv 3dd.grb 3dd.miss.grb
    $split_grib_par 3dd.miss.grb -lev >> split_grib_par.log
    for file in `ls sg*.grb` ; do
      rm -f tmp.grb
      if [ $file = sg_200_002_061_001_000_000.grb ] ; then
        mv $file tmp.grb
        grib_set -s timeRangeIndicator=0,P1=0,P2=0 tmp.grb $file
      fi
  
      $tinterp_grib $file tmp.grb 1
      cat tmp.grb >> 3dd.grb    
    done
  else
    echo "Trovati ${nist_file} istanti su ${nist_req}: ok"
  fi
fi
  
# 2.5 Ordino per reference time
echo "Ordino per reference time"
mv 3dd.grb 3dd.grb.unsorted
arki-query "" --data --sort=reftime grib:3dd.grb.unsorted > 3dd.grb

# 2.6 Ritaglio l'area dei dati fisiografici
echo "Ritaglio l'area dei dati fisiografici"
vg6d_transform --trans-mode=s --trans-type=zoom --sub-type=coordbb \
  --ilon=$lon_min --ilat=$lat_min --flon=$lon_max --flat=$lat_max \
  fisiog.grb.${dataset} fisiog.grb

if [ $nskip -gt 1 ] ; then
  mv fisiog.grb fisiog.grb.full
  vg6d_transform --trans-mode=s --trans-type=boxregrid --sub-type=average \
    --npx=$nskip --npy=$nskip fisiog.grb.full fisiog.grb
fi

# 2.7 Converto in formato 3d.dat
echo "Converto in formato 3d.dat"
$grib23ddat fisiog.grb 3dd.grb 3d.dat
