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
#   L'interpolazione orizzontale e' compiuta successivamente negli script 
#   chiamanti (volendo potrebbe essere spostata qui, vedi nota al par. 2.2.3)
#
# Uso: chiamato da crea_input_meteo.sh e feed_postcosmo.sh 
#
# Note:
#   scad0: -1 = analisi 24h, -0.5 analisi 12h, >=0 previsioni (scad. iniziale, ore)
#   hh_ini: solo 00 o 12 (previsioni: ora del reftime; analisi: ora ultimo istante)
#   nhours: usato solo per previsioni (tra 1 e 72; per analisi e' definito da scad0)
#
#   Con PROJ=none devono essere preventivamente assegnate le variabili:
#   hh_ini, dataset, nhours, scad0;  nzmet, db_lev_list, ptopmet, metmod; 
#   hmix, run_sync, tmp_root; plist_3d, plist_2d, file_layersSup
#
#   file_layersSup e' il nome (con path) di un file grib contenente le quote di 
#   tutti i livelli COSMO, in m dalla superficie, sul dominio dei dati in input 
#   ($dataset).
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
#                                    Versione 7.7.1 (Arkimet), Enrico 21/09/2015
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
  echo "YYYYMMDD: data iniziale (l'ora iniziale e' letta da pre_chimere.inp):"
  echo "          per i forecast (SCAD0 >= 0) corrisponde al reference time"
  echo "          per le analisi (SCAD0 = -1, -0.5) al primo istante da elaborare"
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
  grib_skip_first=/usr/libexec/ma_utils/grib_skip_first.exe
else
  echo "(estra_grib_cosmo.sh) Eseguibili ma_utils: copia di lavoro in "$MA_UTILS_SVN
  post_wind_lm=${MA_UTILS_SVN}/post_lm/src/post_wind_lm.exe
  split_grib_par=${MA_UTILS_SVN}/util/grib/src/split_grib_par.exe
  math_grib=${MA_UTILS_SVN}/util/grib/src/math_grib.exe
  grib_runmean=${MA_UTILS_SVN}/util/grib/src/grib_runmean.exe
  grib_skip_first=${MA_UTILS_SVN}/util/grib/src/grib_skip_first.exe
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

# Controlli sui parametri
if [ $metmod != "LM" ] ; then
  echo "Modello meteo non gestito per estrazione arkimet [METMOD]: "$metmod
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
else
  if [ $hh_ini -ne 0 -a $hh_ini -ne 12 ] ; then
    echo "Sono gestiti solo forecast con inizio alle 00Z o 12Z (hh_ini="$hh_ini")"
    exit 2
  fi
  if [ $dataset != "lm7tmpc" -a $dataset != "COSMO_I7" ] ; then
    echo "Dataset non gestito per le previsioni [DATASET]: "$dataset
    exit 2
  fi
  if [ $nhours -gt 72 ] ; then
    echo "Previsioni COSMO gestite solo fino a +72 ore (richieste: +"$nhours")"
    exit 2
  fi
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
    expr_reftime_ist=">="${datac_akq}" 00, <="${datacp1_akq}" 00"
    expr_reftime_cum=">="${datacm1_akq}" 23, <="${datacp1_akq}" 01"
    expr_reftime_med=">="${datacm1_akq}" 23, <="${datacp1_akq}" 01"
  elif [ $scad0 = "-0.5" -a $hh_ini -eq 0 ] ; then
    expr_reftime_ist=">="${datac_akq}" 00, <="${datac_akq}" 12"
    expr_reftime_cum=">="${datacm1_akq}" 23, <="${datac_akq}" 13"
    expr_reftime_med=">="${datacm1_akq}" 23, <="${datac_akq}" 13"
  elif [ $scad0 = "-0.5" -a $hh_ini -eq 12 ] ; then
    expr_reftime_ist=">="${datac_akq}" 12, <="${datacp1_akq}" 00"
    expr_reftime_cum=">="${datacm1_akq}" 11, <="${datacp1_akq}" 01"
    expr_reftime_med=">="${datacm1_akq}" 11, <="${datacp1_akq}" 01"
  fi

# expr_trange_ist="an"               # patch fino a ricostruzione indice LAMAZ
  if [ $dataset = "lamaz" ] ; then
    expr_trange_ist="Timedef"        # patch fino a ricostruzione indice LAMAZ
    expr_trange_cum="ac0001"
    expr_trange_med="aa0001"
    expr_proddef=""
  elif [ $dataset = "lm7tmpc" -o $dataset = "COSMO_I7" ] ; then
    expr_trange_ist="an"             # patch fino a ricostruzione indice LAMAZ
    expr_trange_cum="Timedef"
    expr_trange_med="Timedef"
    expr_proddef="GRIB: tod=0"
  fi

else
  expr_reftime_ist="="${datac_akq}" "${hh_ini}
  expr_reftime_cum="="${datac_akq}" "${hh_ini}
  expr_reftime_med="="${datac_akq}" "${hh_ini}
  expr_proddef="GRIB: tod=1"

  sc1=$scad0
  sc2=`expr $scad0 + $nhours`
  if [ $sc1 -eq 0 ] ; then
    sc1_proc=0
  else
    sc1_proc=`expr $sc1 - 1`
  fi
  if [ $sc2 -eq 72 ] ; then
    sc2_proc=72
    proc72="Y"
  else
    sc2_proc=`expr $sc2 + 1`
    proc72="N"
  fi

  expr_trange_ist=""
  expr_trange_cum=""
  expr_trange_med=""
  for scad in `seq $sc1 $sc2` ; do
    expr_trange_ist=$expr_trange_ist" or Timedef,${scad}h,254"
  done
  for scad in `seq $sc1_proc $sc2_proc` ; do
    if [ $scad -eq 0 ] ; then
      expr_trange_cum=$expr_trange_cum" or Timedef,${scad}h,254"
      expr_trange_med=$expr_trange_med" or Timedef,${scad}h,254"
    else
      expr_trange_cum=$expr_trange_cum" or Timedef,${scad}h,1,${scad}h"
      expr_trange_med=$expr_trange_med" or Timedef,${scad}h,0,${scad}h"
    fi
  done
  expr_trange_ist=`echo ${expr_trange_ist# or }`
  expr_trange_cum=`echo ${expr_trange_cum# or }`
  expr_trange_med=`echo ${expr_trange_med# or }`

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
    expr_lev3d=hld${slev}${slevp1}
  else
    expr_lev3d=${expr_lev3d}" or "hld${slev}${slevp1}
  fi

  cnt=`expr $cnt + 1`
done
expr_lev3d=`echo $expr_lev3d | sed 's/,$//g'`

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

# Se gli alias non sono gia' stati assegnati, li scarico dal server (servono per
# le query arkimet sui files di lavoro)
if [ -z "${ARKI_ALIASES:-}" ] ; then
  arki-dump --aliases ${akurl} > ./match_alias.conf || exit 3
  export ARKI_ALIASES=`pwd`/match_alias.conf
fi

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
    level: $expr_lev3d
EOF1

  else
    cat <<EOF2 > ${param}.query
    reftime: $expr_reftime_ist
    product: $var
    level: $expr_lev3d
    timerange: $expr_trange_ist
    proddef: $expr_proddef
EOF2

  fi

# 2.2.3 Estraggo dall'archivio e ordino i livelli
# Note:
# - diagmet richiede che i livelli siano ordinati dal basso: in caso contrario 
#   gira comunque, ma alcuni calcoli danno risultati sbagliati.
# - volendo si potrebbe mettere qui il ritaglio dell'area (al momento e' fatto 
#   successivmente negli script chiamanti). La sintassi del ritaglio lato server
#   dovrebbe essere: arki-query --postproc=\
#   "vg6d_subarea --trans-type=zoom --sub-type=index --ix=$ix --iy=$iy --fx=$fx --fy=$fy"

  if [ $param = "ALTI_3D" ] ; then
    arki-query --data --sort=-level --file=${param}.query grib:${file_layersSup} > \
      ${param}.grb
  else
    arki-query --data --sort=reftime,timerange,-level --file=${param}.query \
      -C ${dataset}.conf > ${param}.grb
  fi

  if [ -s ${param}.grb ] ; then
    echo "Estratti grib: "`du -h  ${param}.grb`
#    rm -f tmp.grb
#    mv ${param}.grb tmp.grb
#    arki-scan --data --sort=reftime,timerange,-level grib:tmp.grb > \
#      ${param}.grb || ier=10
  else
    echo "Errore estraendo i grib "$param
    ier=4
  fi
done

# 2.2.4 copio le quote dei livelli per tutti gli istanti richiesti
  echo "Copio le quote dei livelli per gli istanti richiesti"
  rm -f zlay.grb tranges.lst 
  mv ALTI_3D.grb zlay.grb
  grib_get -p dataDate,dataTime,unitOfTimeRange,P1,P2,timeRangeIndicator \
    TEMP_3D.grb | uniq > tranges.lst

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
  rm -f tmp1.grb tmp2.grb
  mv ZWIN_3D.grb tmp1.grb
  mv MWIN_3D.grb tmp2.grb
  $post_wind_lm tmp1.grb tmp2.grb ZWIN_3D.grb MWIN_3D.grb -dest -antir || ier=12
  rm tmp1.grb tmp2.grb

elif [ $dataset = "lm7tmpc" -o $dataset = "COSMO_I7" ] ; then
  echo "Destag e antirotazione vento (standard)"
  rm -f stag.grb destag.grb tmp.grb tmp1.grb
  $grib_skip_first TEMP_3D.grb tmp1.grb -1
  cat tmp.grb ZWIN_3D.grb MWIN_3D.grb >> stag.grb
  rm tmp.grb ZWIN_3D.grb MWIN_3D.grb
  vg6d_transform --a-grid stag.grb destag.grb || ier=13
  rm stag.grb
  arki-query --data --sort=hour:reftime,timerange,-level "product: u" \
     grib:destag.grb > ZWIN_3D.grb || ier=14
  arki-query --data --sort=hour:reftime,timerange,-level "product: v" \
     grib:destag.grb > MWIN_3D.grb || ier=15
  rm destag.grb

fi

# 2.2.6 gestione cloud water/ice
if [ $dataset = "lm7tmpc" -o $dataset = "COSMO_I7" ] ; then
  if [ `echo $plist_3d | grep CLIQ_3D | wc -l` -eq 1 ] ; then
    echo "Calcolo cloud liquid water"
    rm -f sg*.grb tmp.grb
    mv CLIQ_3D.grb tmp.grb
    $split_grib_par tmp.grb || ier=21
    $math_grib -check=grid,time,lev \
      1. sg_200_201_031.grb 1. sg_200_201_035.grb CLIQ_3D.grb sum || ier=22
  fi
  if [ `echo $plist_3d | grep CICE_3D | wc -l` -eq 1 ] ; then
    echo "Calcolo cloud ice"
    rm -f sg*.grb tmp.grb
    mv CICE_3D.grb tmp.grb
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
    expr_levsup="g00"
    var="pr" ;;
  TEM2_2D) 
    tipo_sca="ist"
    expr_levsup="g02"
    var="t" ;;
  TOPC_2D)
    tipo_sca="cum"
    expr_levsup="g00"
    var="tp" ;;
  SSHF_2D) 
    tipo_sca="med"
    expr_levsup="g00"
    var="bflhs" ;;
  SLHF_2D) 
    tipo_sca="med"
    expr_levsup="g00"
    var="bflqds" ;;
  SOIM_2D)
    tipo_sca="ist"
    expr_levsup="lug000010 or ug0001 or ug0002 or ug0006 or ug0018"
    var="ssw or qsoil" ;;
  LOWC_2D)
    tipo_sca="ist"
    expr_levsup="g00"
    var="cl" ;;
  MEDC_2D)
    tipo_sca="ist"
    expr_levsup="g00"
    var="cm" ;;
  HIGC_2D)
    tipo_sca="ist"
    expr_levsup="g00"
    var="ch" ;;
  U10M_2D)
    tipo_sca="ist"
    expr_levsup="g10"
    var="u" ;;
  V10M_2D)
    tipo_sca="ist"
    expr_levsup="g10"
    var="v" ;;
  USTA_2D)
    tipo_sca="ist"
    expr_levsup="g00"
    var="ustar" ;;
  SWRD_2D)
    tipo_sca="med"
    expr_levsup="g00"
    var="sosbs" ;;
  ALB_2D)
    tipo_sca="ist"
    expr_levsup="g00"
    var="alb" ;;
  esac

# 2.3.2 Costruisco la query
  if [ $tipo_sca = "ist" ] ; then
    expr_reftime=$expr_reftime_ist
    expr_trange=$expr_trange_ist
  elif [ $tipo_sca = "cum" ] ; then
    expr_reftime=$expr_reftime_cum
    expr_trange=$expr_trange_cum
  elif [ $tipo_sca = "med" ] ; then
    expr_reftime=$expr_reftime_med
    expr_trange=$expr_trange_med
  fi  

  rm -f ${param}.query
  cat <<EOF2 > ${param}.query
  reftime: $expr_reftime
  product: $var
  level: $expr_levsup
  timerange: $expr_trange
  proddef: $expr_proddef
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
      if [ $scad0 = "-0.5" -a $hh_ini -eq 0 ] ; then   # analisi 00-12
        arki-query  --data "reftime: ="${datac_akq}" 13" grib1:${param}.grb.p1 \
          > tmp.grb || ier=32
        ng=`grib_count tmp.grb 2>/dev/null`
        if [ $ng -eq 0 ] ; then
          arki-query  --data "reftime: ="${datac_akq}" 12" grib1:${param}.grb.p1 \
            > tmp.grb || ier=33
          grib_set -s hour=13 tmp.grb ${param}.grb.13Z || ier=34
          cat ${param}.grb.13Z >> ${param}.grb.p1
        fi
      else                                             # analisi 12-24 o 00-24
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
      arki-query  --data "reftime: $expr_reftime_ist" grib1:${param}.grb.ext \
        > ${param}.grb || ier=39

    else
      $grib_runmean ${param}.grb.p1 ${param}.grb.ext \
        2 -nval 1 -istout 1 -forc || ier=40
      if [ $proc72 = "Y" ]  ; then
        arki-query  --data "timerange: Timedef,72h,,1h" grib1:${param}.grb.p1 \
          > ${param}.grb.f7172 || ier=41
        grib_set -s timeRangeIndicator=0,P1=72,P2=0 ${param}.grb.f7172 \
          ${param}.grb.f072 || ier=42
        cat ${param}.grb.f072 >> ${param}.grb.ext
      fi
      arki-query  --data "timerange: $expr_trange_ist" grib1:${param}.grb.ext \
        > ${param}.grb || ier=43
    fi
  fi

done

# 2.3.5 Calcolo il contenuto d'acqua nello strato superificale del terreno.
# I calcoli e codifica grib  (2/86 invece di 201/198) sono gli stessi usati per 
# produrre le analisi LAMA: Per maggiori dettagli vedi
# /home/eminguzzi/svn/feed_lama/proc_cosmo_ana.sh, sez. 3.5

if [ `echo $plist_2d | grep SOIM_2D | wc -l` -eq 1 -a \
     \( $dataset = "lm7tmpc" -o $dataset = "COSMO_I7" \) ] ; then
  rm -f sg*.grb tmp*.grb
  mv SOIM_2D.grb SOIM_2D.grb.org

  $split_grib_par -lev soil_q.grib || ier=50
  $math_grib -check=grid,time,var 1. sg_200_201_198_111_001_000.grib \
    1. sg_200_201_198_111_002_000.grib tmp1.grib sum || ier=51
  $math_grib -check=grid,time,var 1. sg_200_201_198_111_006_000.grib \
    1. tmp1.grib tmp2.grib sum || ier=52
  $math_grib -check=grid,time,var 0.0555 sg_200_201_198_111_018_000.grib \
    1. tmp2.grib tmp3.grib sum || ier=53

  grib_set -s table2Version=2,indicatorOfParameter=86,indicatorOfTypeOfLevel=112,topLevel=0,bottomLevel=10 \
    tmp3.grib SOIM_2D.grb
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
#   arki-query --data "reftime: $expr_reftime_ist" grib1:TOPC_2D.patch.grb > TOPC_2D.grb
# else
#   arki-query --data "reftime: $expr_trange_ist" grib1:TOPC_2D.patch.grb > TOPC_2D.grb
# fi

exit $ier
