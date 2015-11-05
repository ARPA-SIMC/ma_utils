#! /bin/bash
#-------------------------------------------------------------------------------
# Procedura per estrarre le analisi COSMO (dataset LAMAZ) per un run Calmet, 
# in formato 3D.dat; puo' essere lanciato sia nell'ambiente AQweb sia in quello
# "standard".
#
# Input dinamici
# - 3dd.akq               (query per estrazione arkimet)
#
# Input statici:
# - fisiog.grb.LAMAZ      (orografia e quote model layers nel dataset LAMAZ)
# - grib23ddat.inp        (opzioni per il file di output 3d.dat)
#
# Output:
# - 3d.dat
#                                               Versione 2.0.0 Enrico 01/09/2015
#-------------------------------------------------------------------------------
#set -x

#-------------------------------------------------------------------------------
# Scrive a schermo l'help della procedura
function write_help
{
  echo "uso: arkimet_3dd.bash lon_min lat_min lon_max lat_max [-skip N]"
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

# Path
#dat_dir=/home/maope/www/aqweb/arkimet/data
dat_dir=../dat
#file_mon=/autofs/calmet/aqweb/arkimet/arkimet_3dd.log
file_mon=./arkimet_3dd.log
export AK_TEMP=/autofs/calmet/aqweb/arkimet

# Parametri da riga comando
nskip=0
mand_par=0

if [ $# -eq 0 ] ; then
  write_help
  exit 1
fi
while [ $# -ge 1 ] ; do
  if [ $1 = '-h' ] ; then
    write_help
    exit 1
  elif [ $1 = "-skip" ] ; then
    shift
    nskip=$1
    shift
  elif [ $mand_par -eq 0 ] ; then
    lon_min=$1
    mand_par=1
    shift
  elif [ $mand_par -eq 1 ] ; then
    lat_min=$1
    mand_par=2
    shift
  elif [ $mand_par -eq 2 ] ; then
    lon_max=$1
    mand_par=3
    shift
  elif [ $mand_par -eq 3 ] ; then
    lat_max=$1
    mand_par=4
    shift
  fi
done

if [ $mand_par -ne 4 ] ; then
  write_help
  exit 1
fi

#-------------------------------------------------------------------------------
# 2) Esecuzione

# Cancello eventuali files temporanei di estrazioni precedenti
rm -f tmp.date 3dd.grb* 3dd.dat sg*.grb $file_mon fisiog.grb* split_grib_par.log

# Link degli input statici nella dir di lavoro
cp $dat_dir/fisiog.grb.LAMAZ .
cp $dat_dir/grib23ddat.inp .

# Estrazione i dati
echo "Estraggo i dati"
$ak_getgrib 3dd lamaz -zoom=$lon_min,$lat_min,$lon_max,$lat_max -mon=$file_mon

if [ $nskip -gt 1 ] ; then
  mv 3dd.grb 3dd.grb.full
  vg6d_subarea --trans-type=boxregrid --sub-type=average --npx=$nskip --npy=$nskip \
    3dd.grb.full 3dd.grb
fi

# Controllo se ci sono istanti mancanti; se necessario, li interpolo nel tempo
# I dati di pioggia (gli unici non istantanei) restano con timeRangeIndicator=13,
# che (al momento) non e' gestito da tinterp_grib...
echo "Verifico se ci sono istanti mancanti"
grib_get -P dataDate,hour 3dd.grb | sort | uniq > tmp.date
nist_file=`wc -l tmp.date | awk '{print $1}'`

data_ini=`head -n 1 tmp.date | awk '{print $1}'`
data_ini_sec=`date -d $data_ini +%s`
data_end=`tail -n 1 tmp.date | awk '{print $1}'`
data_end_sec=`date -d $data_end +%s`
nist_req=$[($data_end_sec-$data_ini_sec) / 3600 + 24 ]

if [ $nist_file -lt $nist_req ] ; then
  echo "Istanti trovati: ${nist_file} su ${nist_req}: richiesta interpolazione temporale"
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
fi

# Ordino per reference time
echo "Ordino per reference time"
mv 3dd.grb 3dd.grb.unsorted
arki-scan --data --sort=reftime grib:3dd.grb.unsorted > 3dd.grb

# Ritaglio l'area dei dati fisiografici
echo "Ritagiio l'area dei dati fisiografici"
vg6d_subarea --trans-type=zoom --sub-type=coordbb \
  --ilon=$lon_min --ilat=$lat_min --flon=$lon_max --flat=$lat_max \
  fisiog.grb.LAMAZ fisiog.grb

if [ $nskip -gt 1 ] ; then
  mv fisiog.grb fisiog.grb.full
  vg6d_subarea --trans-type=boxregrid --sub-type=average --npx=$nskip --npy=$nskip \
    fisiog.grb.full fisiog.grb
fi

# Converto in foramto 3d.dat
echo "Converto in formato 3d.dat"
$grib23ddat fisiog.grb 3dd.grb 3d.dat
