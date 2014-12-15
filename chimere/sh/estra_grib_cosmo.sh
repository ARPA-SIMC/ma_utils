#!/bin/bash
#-------------------------------------------------------------------------------
# estra_grib_cosmo.sh
#
# Script per estrarre da arkimet i dati COSMO (analisi o previsioni) per un run 
#   di 24 ore di chimere. Scrive nella dir. corrente un file grib (25 scadenze
#   istantanee) per ciascuno dei parametri richiesti.
#
# Oltre a estrarre i dati, compie alcune elaborazioni (dipendono dal dataset 
#   meteo, ma sono indipendenti dal dominio Chimere; consentono di riallineare i
#   dati di Cosmo operativo a quelli delle analisi):
#   - destag e antirotazione del vento in quota
#   - trasformazione dei dati cumulati/medi in istantanei;
#   - calcolo contenuto d'acqua nei primi 10 cm si suolo;
#   - calcolo di acqua e ghiaccio totali delle nubi.
#
# Uso: chiamato da crea_input_meteo.sh e feed_postcosmo.sh 
#
# Note:
#   SCAD0: -1 = analisi 24h, -0.5 analisi 12h, 0/1/2 previsioni
#
#   Con PROJ=none devono essere preventivamente assegnate le variabili:
#   hh_ini, dataset, nhours, scad0;  nzmet, db_lev_list, ptopmet, metmod; 
#   hmix, run_sync, tmp_root; plist_3d, plist_2d
#
# Exit codes:
# 1: errore nei parametri da riga comando
# 2: errore nei parametri Chimere (pre_chimere.inp)
# 3: errore accesso al DB
# 4: errore nell'estrazione dati
# 5: errore nell'elaborazione dati
#
# TODO: 
# - test dei parametri superficiali opzionali non usati di default
#
# Patch (in attesa del riallineamento e completamento degli archivi):
# - estrazione tp da lamaz (par 1.3)
# - destag vento in quota lamaz (par. 2.2)
# - quando i post-processing di lm7tmpcp e cosmo_i7p saranno completati e 
#   allineati, si potranno estrarre direttamente i parametri cum/med nell'ora 
#   precedente (par. 2.1, 1.3, 2.3). Questo semplificherebbe un po' i conti, ma
#   richiederebbe l'introduzione di nuovi alias per le scadenze previste 
#   (c0124 -> c0124ph, ...); per le analisi dovrebbe bastare Timedef,0,x,1h
#
#                                    Versione 7.6.0 (Arkimet), Enrico 03/12/2014
#-------------------------------------------------------------------------------
#set -x

#-------------------------------------------------------------------------------
# Scrive a schermo l'help della procedura
function write_help
{
  echo "Uso: estra_grib_cosmo.sh PROJ YYYYMMDD [-h]"
  echo "PROJ:     progetto Chimere, di cui viene letto il file pre_chimere.inp; mettere"
  echo "          \"none\" se non si vuole usare pre_chimere.inp (le variabili"
  echo "           d'environment necessarie devono essere state definite ed esportate)"
  echo "YYYYMMDD: data iniziale (l'ora iniziale e' letta da pre_chimere.inp)."
  echo "          Per i forecast (SCAD0 = 0, 1, 2) corrisponde al reference time;"
  echo "          Per le analisi (SCAD0 = -1, -0.5) all'istante iniziale."
  return
}

################################################################################
# 1) Elaborazioni preliminari
#

ier=0
echo "estra_grib_cosmo.sh: inizio elaborazioni"

#-------------------------------------------------------------------------------
# 1.1) Gestione parametri

if [ $# -lt 2 ] ; then
  write_help
  exit 1
fi
if [ $1 = "-h" ] ; then
  write_help
  exit 1
fi
proj=$1
datac_ref=$2

#-------------------------------------------------------------------------------
# 1.2) Assegno l'ambiente ma_utils

if [ -z $MA_UTILS_SVN ] ; then
  post_wind_lm=/usr/libexec/ma_utils/post_wind_lm.exe
  split_grib_par=/usr/libexec/ma_utils/split_grib_par.exe
  math_grib=/usr/libexec/ma_utils/math_grib.exe
  grib_runmean=/usr/libexec/ma_utils/grib_runmean.exe
else
  echo "(estra_grib_cosmo.sh) Eseguibili ma_utils: copia di lavoro in "$MA_UTILS_SVN
  post_wind_lm=${MA_UTILS_SVN}/post_lm/src/post_wind_lm.exe
  split_grib_par=${MA_UTILS_SVN}/util/grib/src/split_grib_par.exe
  math_grib=${MA_UTILS_SVN}/util/grib/src/math_grib.exe
  grib_runmean=${MA_UTILS_SVN}/util/grib/src/grib_runmean.exe
fi  

#-------------------------------------------------------------------------------
# 1.3) Definisco le variabili d'ambiente relative a questo run Chimere 
#      (dipendono  pre_chimere.inp e dalle dir di installazione)

if [ $proj != "none" ] ; then
  if [ $HOSTNAME = "maialinux" ] ; then           # maialinux fed16
    chimere_env=$HOME/ver16/ope/ninfa/bin/chimere_env.sh
  else                                            # lattuga e PCs
    chimere_env=/home/eminguzzi/chimere/bin/chimere_env.sh
  fi
  . $chimere_env $proj || exit 2
fi

dataset=`echo $dataset | tr [:upper:] [:lower:]`
if [ -z $akurl ] ; then
  if [ $dataset = "cosmo_i7" -o $dataset = "COSMO_I7" ] ; then
    akurl="http://maialinux.metarpa:8090"
  else
    akurl="http://arkimet.metarpa:8090"
  fi
fi
if [ $dataset = "cosmo_i7" ] ; then
  dataset="COSMO_I7"
fi

if [ $dataset = "COSMO_I7" -o $dataset = "lm7tmpc" ] ; then
  ds_area="LMSMR4"
elif [ $dataset = "lamaz" ] ; then
  ds_area="LAMAZ"
elif [ $dataset = "lm28tmpc" ] ; then
  ds_area="LMSMR5"
fi
#dev file_zlay=/usr/share/ma_utils/${ds_area}_layers_20120606.grb
file_zlay=/home/eminguzzi/svn/ma_utils/data/${ds_area}_layers_20120606.grb

# Controlli sui parametri
if [ $metmod != "LM" ] ; then
  echo "Modello meteo non gestito per estrazione arkimet [METMOD]: "$metmod
  exit 2
fi
if [ $nhours -ne 24 -a  $nhours -ne 12 ] ; then
  echo "Lunghezza run Chimere non gestita [NHOURS]: "$nhours
  exit 2
fi

if [ $scad0 = "-1" ] ; then
  if [ $hh_ini -ne 0 ] ; then
    echo "Le analisi 24h devono iniziare alle ore 00Z (hh_ini="$hh_ini")"
    exit 2
  fi
elif [ $scad0 = "-0.5" ] ; then
  if [ $hh_ini -ne 0 -a $hh_ini -ne 12 ] ; then
    echo "Le analisi 12h devono iniziare alle ore 00Z o 12Z (hh_ini="$hh_ini")"
    exit 2
  fi
elif [ $scad0 -eq 0 -o $scad0 -eq 24 -o $scad0 -eq 48 ] ; then
  if [ $hh_ini -ne 0 -a $hh_ini -ne 12 ] ; then
    echo "Sono gestiti solo forecast con inizio alle 00Z o 12Z (hh_ini="$hh_ini")"
    exit 2
  fi
  if [ $dataset != "lm7tmpc" -a $dataset != "COSMO_I7" ] ; then
    echo "Dataset non gestito per le previsioni [DATASET]: "$dataset
    exit 2
  fi

else
  echo "Scadenza iniziale non gestita [scad0]: "$scad0
  exit 2
fi

if [ $hmix -eq 0 ] ; then
  echo "L'uso delle analisi LAMA di Hmix non e' ancora gestito"
  exit 2
fi

#-------------------------------------------------------------------------------
# 1.4) Calcolo le variabili dipendenti dalla data richiesta

datac_akq=`date -d ${datac_ref} +%Y-%m-%d`
datac_txt=`date -d ${datac_ref} +%Y%m%d`

if [ $scad0 = "-1" -o $scad0 = "-0.5" ] ; then                       # analisi
  datacm1_akq=`date -d "${datac_ref} - 1day" +%Y-%m-%d`
  datacp1_akq=`date -d "${datac_ref} + 1day" +%Y-%m-%d`
  if [ $scad0 = "-1" ] ; then
    str_reftime_ist=">="${datac_akq}" 00, <="${datacp1_akq}" 00"
    str_reftime_cum=">="${datacm1_akq}" 23, <="${datacp1_akq}" 01"
    str_reftime_med=">="${datacm1_akq}" 23, <="${datacp1_akq}" 01"
  elif [ $scad0 = "-0.5" -a $hh_ini -eq 0 ] ; then
    str_reftime_ist=">="${datac_akq}" 00, <="${datac_akq}" 12"
    str_reftime_cum=">="${datacm1_akq}" 23, <="${datac_akq}" 13"
    str_reftime_med=">="${datacm1_akq}" 23, <="${datac_akq}" 13"
  elif [ $scad0 = "-0.5" -a $hh_ini -eq 12 ] ; then
    str_reftime_ist=">="${datac_akq}" 12, <="${datacp1_akq}" 00"
    str_reftime_cum=">="${datacm1_akq}" 11, <="${datacp1_akq}" 01"
    str_reftime_med=">="${datacm1_akq}" 11, <="${datacp1_akq}" 01"
  fi

# str_timerange_ist="an"               # patch fino a ricostruzione indice LAMAZ
  if [ $dataset = "lamaz" ] ; then
    str_timerange_ist="Timedef"        # patch fino a ricostruzione indice LAMAZ
    str_timerange_cum="ac0001"
    str_timerange_med="aa0001"
    str_proddef=""
  elif [ $dataset = "lm7tmpc" -o $dataset = "COSMO_I7" ] ; then
    str_timerange_ist="an"             # patch fino a ricostruzione indice LAMAZ
    str_timerange_cum="Timedef"
    str_timerange_med="Timedef"
    str_proddef="proddef: GRIB: tod=0"
  fi

elif [ $scad0 = "0" ] ; then                                        # previ d+0
  str_reftime_ist="="${datac_akq}" "${hh_ini}
  str_reftime_cum="="${datac_akq}" "${hh_ini}
  str_reftime_med="="${datac_akq}" "${hh_ini}
  str_timerange_ist="an or f0124"
  str_timerange_cum="an or c0124 or c025"
  str_timerange_med="an or a0124 or a025"
  str_proddef="proddef: GRIB: tod=1"

elif [ $scad0 = "24" ] ; then                                       # previ d+1
  str_reftime_ist="="${datac_akq}" "${hh_ini}
  str_reftime_cum="="${datac_akq}" "${hh_ini}
  str_reftime_med="="${datac_akq}" "${hh_ini}
  str_timerange_ist="f024 or f2548"
  str_timerange_cum="c023 or c024 or c2548 or c049"
  str_timerange_med="a023 or a024 or a2548 or a049"
  str_proddef="proddef: GRIB: tod=1"

elif [ $scad0 = "48" ] ; then                                       # previ d+2
  str_reftime_ist="="${datac_akq}" "${hh_ini}
  str_reftime_cum="="${datac_akq}" "${hh_ini}
  str_reftime_med="="${datac_akq}" "${hh_ini}
  str_timerange_ist="f048 or f4972"
  str_timerange_cum="c047 or c048 or c4972"
  str_timerange_med="a047 or a048 or a4972"
  str_proddef="proddef: GRIB: tod=1"

fi

#-------------------------------------------------------------------------------
# 1.5) Costruisco la stringa dei livelli richiesti (per parametri 3D)

cnt=1
while [ $cnt -le $nzmet ] ; do
  lev=`echo $db_lev_list | awk '{print $'$cnt'}'`
  levp1=`expr $lev + 1`

  if [ $lev -lt 10 ] ; then 
    slev=0${lev}
  else
    slev=$lev
  fi
  if [ $levp1 -lt 10 ] ; then 
    slevp1=0${levp1}
  else
    slevp1=$levp1
  fi

  if [ $cnt -eq 1 ] ; then
    str_lev3d=hld${slev}${slevp1}
  else
    str_lev3d=${str_lev3d}" or "hld${slev}${slevp1}
  fi

  cnt=`expr $cnt + 1`
done
str_lev3d=`echo $str_lev3d | sed 's/,$//g'`

################################################################################
# 2) Estrazione

#-------------------------------------------------------------------------------
# 2.1) Preliminari

if [ $run_sync -eq 1 ] ; then
  tmp_dir=$tmp_root/meteo
else
  tmp_dir=$tmp_root
fi
if [ ! -d $tmp_dir ] ; then
  echo "Directory di lavoro "$tmp_dir" inesistente"
  exit 2
fi
cd $tmp_dir
echo "Dir di lavoro: "`pwd`

unset http_proxy
rm -f ${dataset}.conf
arki-mergeconf ${akurl}/dataset/${dataset} > ${dataset}.conf || exit 3

# Scarico gli alias dal server, per poterli usare nelle query su file locali
arki-dump --aliases ${akurl} > ./match_alias.conf || exit 3
export ARKI_ALIASES=`pwd`/match_alias.conf

#-------------------------------------------------------------------------------
# 2.2) Parametri 3D

for param in $plist_3d ; do
  rm -f ${param}.query ${param}.grb

# 2.2.1 Mnemonici arkimet di ciascun parametro  
  case $param in 
  ALTI_3D)
    var="GRIB1,,2,8" ;;
  PRES_3D)
    var="pr" ;;
  ZWIN_3D)
    var="u" ;;
  MWIN_3D) 
    var="v" ;;
  TEMP_3D)
    var="t" ;;
  SPHU_3D)
    var="q" ;;
  CLIQ_3D)
    var="qcr or qw or qr" ;;
  CICE_3D)
    var="qis or qi or qs" ;;
  esac

# 2.2.2 Costruisco la query
  if [ $param = "ALTI_3D" ] ; then
    cat <<EOF1 > ${param}.query
    product: $var
    level: $str_lev3d
EOF1

  else
    cat <<EOF2 > ${param}.query
    reftime: $str_reftime_ist
    product: $var
    level: $str_lev3d
    timerange: $str_timerange_ist
    $str_proddef
EOF2

  fi

# 2.2.3 Estraggo dall'archivio
# NB: per gli archivi su area grande (lm7tmpc e cosmo_i7), volendo si potrebbe
# ritagliare l'area lato server (sintassi da verificare): arki-query --postproc=\
# "vg6d_subarea --trans-type=zoom --sub-type=index --ix=$ix --iy=$iy --fx=$fx --fy=$fy"
  if [ $param = "ALTI_3D" ] ; then
    arki-query --data --file=${param}.query grib:${file_zlay} > ${param}.grb
  else
    arki-query --data --file=${param}.query -C ${dataset}.conf > ${param}.grb
  fi

  if [ -s ${param}.grb ] ; then
    echo "Estratti grib: "`du -h  ${param}.grb`
  else
    echo "Errore estraendo i grib "$param
    ier=4
  fi
done

# 2.2.4 copio le quote dei livelli per tutti gli istanti richiesti
  echo "Copio le quote dei livelli per gli istanti richiesti"
  mv ALTI_3D.grb zlay.grb
  rm -f tranges.lst 
  grib_get -p dataDate,dataTime,unitOfTimeRange,P1,P2,timeRangeIndicator \
    TEMP_3D.grb | sort | uniq > tranges.lst

  while read line ; do
    dd=`echo $line | awk '{print $1}'`
    dt=`echo $line | awk '{print $2}'`
    uotr=`echo $line | awk '{print $3}'`
    p1=`echo $line | awk '{print $4}'`
    p2=`echo $line | awk '{print $5}'`
    tri=`echo $line | awk '{print $6}'`
    rm -f tmp.grb
    grib_set -s dataDate=${dd},dataTime=${dt},unitOfTimeRange=${uotr},P1=${p1},P2=${p2},timeRangeIndicator=${tri} \
      zlay.grb tmp.grb
    cat tmp.grb >> ALTI_3D.grb
  done < tranges.lst

# 2.2.5 destag e antruto il vento
if [ $dataset = "lamaz" ] ; then   # patch: in attesa del riallineamento lamaz
  echo "Destag e antirotazione vento (lamaz)"
  rm -f ZWIN_3D.grb.org MWIN_3D.grb.org tmp1.grb tmp2.grb
  mv ZWIN_3D.grb ZWIN_3D.grb.org
  mv MWIN_3D.grb MWIN_3D.grb.org
  arki-scan --data --sort=reftime,timerange,level grib1:ZWIN_3D.grb.org > \
    tmp1.grb || ier=10
  arki-scan --data --sort=reftime,timerange,level grib1:MWIN_3D.grb.org > \
    tmp2.grb || ier=11
  $post_wind_lm tmp1.grb tmp2.grb ZWIN_3D.grb MWIN_3D.grb -dest -antir || ier=12

elif [ $dataset = "lm7tmpc" -o $dataset = "COSMO_I7" ] ; then
  echo "Destag e antirotazione vento (standard)"
  rm -f stag.grb destag.grb
  cat TEMP_3D.grb ZWIN_3D.grb MWIN_3D.grb >> stag.grb
  mv ZWIN_3D.grb ZWIN_3D.grb.org
  mv MWIN_3D.grb MWIN_3D.grb.org
  vg6d_transform --a-grid stag.grb destag.grb || ier=13
  arki-query --data "product: u" grib1:destag.grb > ZWIN_3D.grb || ier=14
  arki-query --data "product: v" grib1:destag.grb > MWIN_3D.grb || ier=15

fi

# 2.2.6 gestione cloud water/ice
if [ $dataset = "lm7tmpc" -o $dataset = "COSMO_I7" ] ; then
  if [ `echo $plist_3d | grep CLIQ_3D | wc -l` -eq 1 ] ; then
    echo "Calcolo cloud liquid water"
    rm -f sg*.grb tmp.grb
    mv CLIQ_3D.grb CLIQ_3D.grb.org
    arki-scan --data --sort=reftime,timerange,level grib1:CLIQ_3D.grb.org > \
      tmp.grb || ier=20
    $split_grib_par tmp.grb || ier=21
    $math_grib -check=grid,time,lev \
      1. sg_200_201_031.grb 1. sg_200_201_035.grb CLIQ_3D.grb sum || ier=22
  fi
  if [ `echo $plist_3d | grep CICE_3D | wc -l` -eq 1 ] ; then
    echo "Calcolo cloud ice"
    rm -f sg*.grb tmp.grb
    mv CICE_3D.grb CICE_3D.grb.org
    arki-scan --data --sort=reftime,timerange,level grib1:CICE_3D.grb.org > \
      tmp.grb || ier=23
    $split_grib_par tmp.grb || ier=24
    $math_grib -check=grid,time,lev \
      1. sg_200_201_033.grb 1. sg_200_201_036.grb CICE_3D.grb sum || ier=25
  fi
fi

#-------------------------------------------------------------------------------
# 2.3) Parametri 2D

for param in $plist_2d ; do
  rm -f ${param}.query ${param}.grb

# 2.3.1 Mnemonici arkimet di ciascun parametro
  case $param in 
  SURP_2D)
    tipo_sca="ist"
    str_levsup="g00"
    var="pr" ;;
  TEM2_2D) 
    tipo_sca="ist"
    str_levsup="g02"
    var="t" ;;
  TOPC_2D)
    tipo_sca="cum"
    str_levsup="g00"
    var="tp" ;;
  SSHF_2D) 
    tipo_sca="med"
    str_levsup="g00"
    var="bflhs" ;;
  SLHF_2D) 
    tipo_sca="med"
    str_levsup="g00"
    var="bflqds" ;;
  SOIM_2D)
    tipo_sca="ist"
    str_levsup="lug000010 or ug0001 or ug0002 or ug0006"
    var="ssw or qsoil" ;;
  LOWC_2D)
    tipo_sca="ist"
    str_levsup="g00"
    var="cl" ;;
  MEDC_2D)
    tipo_sca="ist"
    str_levsup="g00"
    var="cm" ;;
  HIGC_2D)
    tipo_sca="ist"
    str_levsup="g00"
    var="ch" ;;
  U10M_2D)
    tipo_sca="ist"
    str_levsup="g10"
    var="u" ;;
  V10M_2D)
    tipo_sca="ist"
    str_levsup="g10"
    var="v" ;;
  USTA_2D)
    tipo_sca="ist"
    str_levsup="g00"
    var="ustar" ;;
  SWRD_2D)
    tipo_sca="med"
    str_levsup="g00"
    var="sosbs" ;;
  ALB_2D)
    tipo_sca="ist"
    str_levsup="g00"
    var="alb" ;;
  esac

# 2.3.2 Costruisco la query
  if [ $tipo_sca = "ist" ] ; then
    str_reftime=$str_reftime_ist
    str_timerange=$str_timerange_ist
  elif [ $tipo_sca = "cum" ] ; then
    str_reftime=$str_reftime_cum
    str_timerange=$str_timerange_cum
  elif [ $tipo_sca = "med" ] ; then
    str_reftime=$str_reftime_med
    str_timerange=$str_timerange_med
  fi  

  rm -f ${param}.query
  cat <<EOF2 > ${param}.query
  reftime: $str_reftime
  product: $var
  level: $str_levsup
  timerange: $str_timerange
  $str_proddef
EOF2

# 2.3.3 Estraggo dall' archivio
  arki-query --data --file=${param}.query -C ${dataset}.conf >> ${param}.grb
  if [ -s ${param}.grb ] ; then
    echo "Estratti grib: "`du -h  ${param}.grb`
  else
    echo "Errore estraendo i grib "$param
    ier=4
  fi

# 2.3.4 Post-processing dei dati non istantanei
  if [ $tipo_sca = "cum" -o $tipo_sca = "med" ] ; then
    rm -f ${param}.grb.org ${param}.grb.p1 ${param}.grb.ext 
    mv ${param}.grb ${param}.grb.org

#   Passo a valori relativi all'ora precedente (patch)
    if [ $dataset = "lm7tmpc" -o $dataset = "COSMO_I7" ] ; then
      if [ $tipo_sca = "cum" ] ; then
        vg6d_transform --comp-stat-proc=1 --comp-step="0 01" \
          ${param}.grb.org ${param}.grb.p1 || ier=30
      elif [ $tipo_sca = "med" ] ; then
        vg6d_transform --comp-stat-proc=0 --comp-step="0 01" \
          ${param}.grb.org ${param}.grb.p1 || ier=31
      fi
    else
      ln -s ${param}.grb.org ${param}.grb.p1
    fi

#   Passo a valori "istantanei" (ie. media sull'intervallo di 2 ore centrato
#      nell'istante richiesto) e filtro le scadenze extra.
#   Note
#   - per l'ultimo istante di un run Cosmo (+72), il calcolo e' impossibile
#   - per l'ultima ora di un'analisi (12 o 24), il calcolo e' possibile solo per
#     dati storici (serve la prima ora di analisi del run successivo)
#   - per i run con analisi storiche, sarebbe richiesta la presenza obbligatoria
#     delle analisi del giorno successivo
#   In tutti questi casi, per l'ultimo istante prendo la media dell'ultima ora.

    if [ $scad0 = "-1" -o $scad0 = "-0.5" ] ; then

#     Analisi: verifico se esiste il dato all'istante finale + 1h;
#     se non c'e', replico il dato all'istante finale
      if [ $scad0 = "-0.5" -a $hh_ini -eq 0 ] ; then
        arki-query  --data "reftime: ="${datac_akq}" 13" grib1:${param}.grb.p1 \
          > tmp.grb || ier=32
        ng=`grib_count tmp.grb 2>/dev/null`
        if [ $ng -eq 0 ] ; then
          arki-query  --data "reftime: ="${datac_akq}" 12" grib1:${param}.grb.p1 \
            > tmp.grb || ier=33
          grib_set -s hour=1 tmp.grb ${param}.grb.13Z || ier=34
          cat ${param}.grb.13Z >> ${param}.grb.p1
        fi
      else
        arki-query  --data "reftime: ="${datacp1_akq}" 01" grib1:${param}.grb.p1 \
          > tmp.grb || ier=35
        ng=`grib_count tmp.grb 2>/dev/null`
        if [ $ng -eq 0 ] ; then
          arki-query  --data "reftime: ="${datacp1_akq}" 00" grib1:${param}.grb.p1 \
            > tmp.grb || ier=36
          grib_set -s hour=1 tmp.grb ${param}.grb.25Z || ier=37
          cat ${param}.grb.25Z >> ${param}.grb.p1
        fi
      fi

      $grib_runmean ${param}.grb.p1 ${param}.grb.ext \
        2 -nval 1 -istout 1 || ier=38
      arki-query  --data "reftime: $str_reftime_ist" grib1:${param}.grb.ext \
        > ${param}.grb || ier=39

    else
      $grib_runmean ${param}.grb.p1 ${param}.grb.ext \
        2 -nval 1 -istout 1 -forc || ier=40
      if [ $scad0 -eq 48 ]  ; then
        arki-query  --data "timerange: Timedef,72h,,1h" grib1:${param}.grb.p1 \
          > ${param}.grb.f7172 || ier=41
        grib_set -s timeRangeIndicator=0,P1=72,P2=0 ${param}.grb.f7172 \
          ${param}.grb.f072 || ier=42
        cat ${param}.grb.f072 >> ${param}.grb.ext
      fi
      arki-query  --data "timerange: $str_timerange_ist" grib1:${param}.grb.ext \
        > ${param}.grb || ier=43
    fi
  fi

done

# 2.3.5 Post-processing umidita' del terreno prodotta con multilayer soil model:
#       calcolo il contenuto medio nei primi 10 cm, scrivo il grib come nelle 
#       analisi lamaz (2/86 invece di 201/198)

if [ `echo $plist_2d | grep SOIM_2D | wc -l` -eq 1 -a \
     \( $dataset = "lm7tmpc" -o $dataset = "COSMO_I7" \) ] ; then
  rm -f sg*.grb tmp.grb
  mv SOIM_2D.grb SOIM_2D.grb.org
  $split_grib_par -lev SOIM_2D.grb.org || ier=50
  $math_grib -check=grid,time,var \
    0.15 sg_200_201_198_111_001_000.grb 0.25 sg_200_201_198_111_002_000.grb \
    tmp.grb sum || ier=51
  $math_grib -check=grid,time,var \
    1. tmp.grb 0.6 sg_200_201_198_111_006_000.grb \
    SOIM_2D.grb sum || ier=52
fi

#-------------------------------------------------------------------------------
# Accrocchio per aggirare i problemi nella gestione di TOPC (08/11/2011)
#
# Attualmente risulta impossibile gestire correttamente i campi di
# precipitazione, per due motivi:
# 1) arkimet non gestisce correttamente i timerange delle analisi non istantanee, 
#    per cui nel file TOPC_2D.grb.org sono presenti anche le cumulate in 12 ore:
#    decumula_lm non funziona correttamente, e il file .ext e' gia' sbagliato.
# 2) probabilmente eleborando i campi successivmente con grib-api e gribex si 
#    ricade nei casi in cui i campi costanti danno errore

# mv TOPC_2D.grb TOPC_2D.grb.bad
# cong TOPC_2D.grb.org TOPC_2D.patch.grb +"setkey sca:1,0,1,13"
# 
# if [ $scad0 = "-1" ] ; then
#   arki-query --data "reftime: $str_reftime_ist" grib1:TOPC_2D.patch.grb > TOPC_2D.grb
# else
#   arki-query --data "reftime: $str_timerange_ist" grib1:TOPC_2D.patch.grb > TOPC_2D.grb
# fi

exit $ier
