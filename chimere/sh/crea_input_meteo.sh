#!/bin/bash
unset LANG
#-------------------------------------------------------------------------------
# crea_input_meteo.sh
# Script per creare l'input meteorologico per una serie di run di chimere.
#
# Uso: crea_input_meteo.sh PROGETTO [-h] [-noestra]  [-noint]
#
# Input: 
# - parametri di controllo del run (pre_chimere.inp)
#
# - La proj_dir deve esistere, e contenere i files pre_chimere.inp e LANDPAR
# - Nella dir $domains_dir devono esistere i files COORD, VCOORD, LANDUSE e 
#   BIOFACS relativi al dominio di integrazione.
#
# Note:
# Sostituisce gli script interf-*
# Gestisce solo run di 24 ore.
# Per usare dati non provenienti dall'archivio:
# - specificare DATASET=NONE in pre_chimere.inp
# - preparare i grib a mano e metterli nella dir. ... 
#   I nomi dei files devono essere nella forma: pppp_nD.grb
#
# Con  opzione -noestra, nella dir di esecuzione deve essere presente un file 
#   per parametro (grib); la ripulitura iniziale della dir e' parziale
# Con opzione -noint, suppone che l'area di Chimere sia la stessa dei grib, e
#   non effettua l'interpolazione orizzontale (la procedura NON fa controlli!!)
#
#                         Versione 7.1.2 (V200709C), Michele & Enrico 30/12/2014
#
# NOTE PER NUOVA VERSIONE
# - gestire caso ECMWF (disponibile GEOP_3D, varibile nel tempo; estra_grib_ecmwf?)
#
#-------------------------------------------------------------------------------
#set -x

#-------------------------------------------------------------------------------
# Scrive a schermo l'help della procedura
function write_help
{
  echo "Uso: crea_input_meteo.sh PROJ [-h] [-noestra] [-noint]"
  echo "Tutti i files di input devono essere gia' pronti"
  echo "Con opzione -noestra non lancia estrazione GRIB (nella dir. di lavoro "
  echo "  deve esistere un file per parametro, decumulati e con 25 scadenze)"
  echo " -noint:suppone che l'area di CHIMERE sia la stessa dei grib e non interpola"
    return
}

################################################################################
# 1) Elaborazioni preliminari
#

echo "*** CREA_INPUT_METEO.SH ***"
echo ""

#-------------------------------------------------------------------------------
# 1.1) Gestione parametri

rqestra="Y"
opt_int=""
if [ $# -eq 0 ] ; then
  write_help
  exit 1
fi
while [ $# -ge 1 ] ; do
  if [ $1 = '-h' ] ; then
    write_help
    exit 1
  elif [ $1 = '-noestra' ] ; then  
    rqestra="N"
    shift
 elif [ $1 = '-noint' ] ; then
    opt_int="-noint"
    shift
  else
    proj=$1
    shift
  fi    
done

#-------------------------------------------------------------------------------
# 1.2) Definisco le variabili d'ambiente relative a questo run Chimere 
#      (dipendono  pre_chimere.inp e dalle dir di installazione)

if [ $HOSTNAME = "fileserver" ] ; then          # maialinux fed16
  chimere_env=$HOME/ver16/ope/ninfa/bin/chimere_env.sh
else                                            # lattuga e PCs
  chimere_env=/home/eminguzzi/chimere/bin/chimere_env.sh
fi
. $chimere_env $proj

estra_grib_cosmo=$util_dir/estra_grib_cosmo.sh
estra_grib_ecm_ana=$util_dir/estra_grib_ecm_ana.sh

FNCOO="COORD"
fncoord=${dom_dir}/HCOORD/COORD_${dom}
vcoord=${dom_dir}/VCOORD/VCOORD_${nlayer_chimere}_${first_layer_pressure}_${top_chimere_pressure}
fnlanduse=${dom_dir}/${dom}/LANDUSE_${dom}
fnbio=${fis_dir}/BIOFACS_${dom}
fnlandpar=${chem_dir}/inputdata.${labchem}/LANDPAR

date_end_sec=`date -d $date_end +%s`
date_ini_sec=`date -d $date_ini +%s`
ndays=`expr \( $date_end_sec - $date_ini_sec \) / 86400 + 1`

nx=`awk 'BEGIN{stop=0}{x[FNR]=$1}END{for(k=2;k<=FNR;k++) {if(x[k-1]>x[k]&&stop==0){print k-1;stop=1}}}' ${fncoord}`
np=`wc -l ${fncoord} | gawk '{print $1}' `
ny=`expr ${np} / ${nx}`

if [ $run_sync -eq 1 ] ; then
  tmp_dir=$tmp_root/meteo
else
  tmp_dir=$tmp_root
fi
fnmetinfos=${tmp_dir}/METINFOS.nml
fnmeteo=${tmp_dir}/METEO.nc
diagmet_fniMETEO=${tmp_dir}/METEOINP.nc

# Controlli
if [ $dataset != "ECM_ANA" -a $dataset != "LAMAZ" -a \
     $dataset != "COSMO_I7" -a  $dataset != "LM7TMPC" -a \
     $dataset != "NONE" ] ; then
  echo "dataset "$dataset" non gestito"
  exit 2
fi
if [ $hh_ini -ne 00 -a $rqestra  = "Y" ] ; then
  echo "Estrazione GRIB gestita solo per run che iniziano alle 00"
  exit 2
fi
np2=`expr $nx \* $ny`
if [ $np -ne $np2 ] ; then
  echo "Errore nel calcolo di nx e ny a partire da COORD_${dom}"
  echo "nx, ny ,np "$nx" "$ny" "$np
  exit 2
fi
if [ ! -d $tmp_dir ] ; then
  echo "Directory di lavoro inesistente "$tmp_dir
  exit 2
fi

# Log a schermo
echo "Progetto: "$proj
echo "Versione Chimere: "$vers
echo "Griglia orizzontale: domain="$dom", nx="$nx", ny="$ny
echo "Scadenza iniziale di previsione: "$scad0
echo "Opzioni principali: qsscale="$qsscale", dataset meteo="$dataset
echo "Files di input principali: "
echo "  fncoord: "$fncoord
echo "  vcoord:  "$vcoord
echo "  fnmeteo: "$fnmeteo
echo "  lmlsm:   "$lm_lsm
echo "Path:"
echo "  dir del progetto (proj_dir) "$proj_dir
echo "  dir di lavoro (tmp_dir)     "$tmp_dir
echo "  dir output (METEO.*.nc.gz): "$meteo_dir

################################################################################
# 2) Ciclo sui run. 
# - datac_ver (YYYYMMDD) e' la data  di validita' del run in corso (ie. data di
#   calendario da elaborare)
# - datac_ref (YYYYMMDD) e' il reference time  del run in corso (ie. istante 
#   iniziale del run)
# Le due date coincidono per analisi e d+0, mentre sono diverse per d+1 e d+2;
# i files intermdi Chimere contengono la data di validita', mentre i grib usano
# il reference time

cd $tmp_dir
rm -f *

cnt=0
simp=dum
while [ $cnt -lt $ndays ] ; do
  cnt=`expr $cnt + 1`

#-------------------------------------------------------------------------------
# 2.1) Preliminari

# Calcolo la data corrente
  incr=`expr $cnt - 1`
  datac_ver=`date -d "${date_ini} + ${incr}day" +%Y%m%d`
  if [ $scad0 -eq -1 ] ; then
    datac_ref=$datac_ver
    lab_scad0="ana"
  elif [ $scad0 -eq 0 ] ; then
    datac_ref=$datac_ver
    lab_scad0="+00"
  elif [ $scad0 -eq 24 ] ; then
    datac_ref=`date -d "$datac_ver - 1day" +%Y%m%d`
    lab_scad0="+24"
  elif [ $scad0 -eq 48 ] ; then
    datac_ref=`date -d "$datac_ver - 2day" +%Y%m%d`
    lab_scad0="+48"
  fi

  echo ""
  echo "***********************************************************************"
  echo "Elaborazione run "$datac_ref" "$lab_scad0" ("`date`")"

# Ripulisco la dir di lavoro dai files del run precedente
  if [ $rqestra = "Y" ] ; then 
    rm -f METEO BEMISSIONS METINFOS ????_?D ????_?D.grb lama.* *.log AEMISSIONS*
  else
    rm -f METEO BEMISSIONS METINFOS ????_?D lama.* *.log AEMISSIONS*
  fi

#-------------------------------------------------------------------------------
# 2.2) Costruisco la lista dei parametri e seleziono i livelli da estrarre

# Parametri obbligatori
  plist_3d="ALTI_3D PRES_3D ZWIN_3D MWIN_3D TEMP_3D SPHU_3D"
  plist_2d="SURP_2D TEM2_2D TOPC_2D SWRD_2D ALB_2D"

# Parametri opzionali
  if [ $cloudw = 0 ] ; then
    plist_3d=$plist_3d" CLIQ_3D"
  fi
  if [ $clice = 0 ] ; then
    plist_3d=$plist_3d" CICE_3D"
  fi
  if [ $flux = 0 ] ; then
    plist_2d=$plist_2d" SSHF_2D SLHF_2D"
  fi
  if [ $ustar = 0 ] ; then
    plist_2d=$plist_2d" USTA_2D"
  fi
  if [ $hmix = 0 ] ; then
    plist_2d=$plist_2d" PBLH_2D"
  fi
  if [ $uv10m = 0 ] ; then
    plist_2d=$plist_2d" U10M_2D V10M_2D"
  fi
  if [ $soilhum = 0 ] ; then
    plist_2d=$plist_2d" SOIM_2D"
  fi

  optcl=`echo $lcloud | cut -d \  -f 1`
  optcm=`echo $mcloud | cut -d \  -f 1`
  optch=`echo $hcloud | cut -d \  -f 1`
  if [ $optcl = 0 -o $cloudw = 1 ] ; then
    plist_2d=$plist_2d" LOWC_2D"
  fi
  if [ $optcm = 0 -o $cloudw = 1 ] ; then
    plist_2d=$plist_2d" MEDC_2D"
  fi
  if [ $optch = 0 -o $cloudw = 1 ] ; then
    plist_2d=$plist_2d" HIGC_2D"
  fi

  export plist_3d
  export plist_2d

# Trovo il numero di livelli richiesti. 
# db_lev e' il numero totale di livelli (levels) presenti nei dati grib

  case $ptopmet in
  500)
    if   [ $metmod = "LM" -a $datac_ref -lt 20060126 ] ; then
      nzmet=22
      db_lev_list="35 34 33 32 31 30 29 28 27 26 25 24 23 22 21 20 19 18 17 16 15 13 11 9 7 5 3 1"
    elif [ $metmod = "LM" -a $datac_ref -ge 20060126 ] ; then
      nzmet=25
      db_lev_list="40 39 38 37 36 35 34 33 32 31 30 29 28 27 26 25 24 23 22 21 20 19 18 17 15 13 11 9 7 5 3 1"
    elif [ $metmod = "ECMWF" ] ; then
      nzmet=23
    fi ;;
  *)
    echo "Top pressure "$ptopmet " not yet implemented"
    exit 2
  esac

  export nzmet db_lev_list
  np3d=`expr $np \* $nzmet`

#-------------------------------------------------------------------------------
# 2.3) Estraggo i dati meteo: da arkimet (COSMO) o da files (ECMWF)
#      In ogni caso, questa sezione produce un file grib per ogni parametro, con
#      date estreme, scadenze e numero livelli giusti.

  echo ""
  if [ $rqestra = "Y" ] ; then 

    case ${dataset} in
    LAMAZ | COSMO_I7 | LM7TMPC )  
      echo "*** Estrazione GRIB ("$estra_grib_cosmo")"
      $estra_grib_cosmo $proj $datac_ref 
      if [ $? -gt 0 ] ; then
        echo "Segnalato errore nell'estrazione grib"
        exit 3
      fi ;;
    ECM_ANA)
      echo "*** Estrazione GRIB ("$estra_grib_ecm_ana")"
      $estra_grib_ecm_ana $proj $datac_ref ;;
    NONE)
      echo "Estrazione grib non richiesta" ;;
    esac
  fi

#-------------------------------------------------------------------------------
# 2.4) Interpolazione orizzontale dei parametri richiesti
# NB: la Soil Moisture LM deve essere interpolata dai soli punti di terra.

  echo ""
  echo "*** Interpolazione orizzontale"
  plist=$plist_3d" "$plist_2d
  for param in $plist ; do
    echo ""
    echo "Elaboro file "${param}.grb 
    if [ $metmod = "LM" -a $param = SOIM_2D ] ; then
      $util_dir/exdom_grib.exe ${param}.grb ${param} \
        $fncoord -lsmask $lm_lsm $opt_int 2>&1 |grep -v "FORTRAN STOP"
    else
      $util_dir/exdom_grib.exe ${param}.grb ${param} $fncoord $opt_int \
        2>&1 | grep -v "FORTRAN STOP"
    fi
  done

#-------------------------------------------------------------------------------
# 2.5) Interpolazione temporale (se necessaria)

  case ${metmod} in
  ECMWF)
    echo ""
    echo "*** Interpolazione temporale"

    for param in $plist_3d ; do
      echo "Elaboro parametro "$param
      mv $param ${param}_6h
      $util_dir/tinterp_sim.exe ${param}_6h $param $np $nzmet
    done
    for param in $plist_2d ; do
      echo "Elaboro parametro "$param
      mv $param ${param}_6h
      $util_dir/tinterp_sim.exe ${param}_6h $param $np 1
    done ;;
  esac

#-------------------------------------------------------------------------------
# 2.6) Elaborazioni sulle grandezze estratte

  echo ""
  echo "*** Post-elaborazioni e interpolazione verticale"

# Interpolo o calcolo la pressione 3D (sui livelli meteo)
  case ${metmod} in
  LM)  
    $util_dir/exdom_grib.exe PRES_3D.grb PRES_3D $fncoord -ln $opt_int \
        2>&1  |grep -v "FORTRAN STOP" ;;
  ECMWF)
    $util_dir/pres_ecmwf.exe $np $nzmet TEMP_3D.grb  \
        2>&1  |grep -v  "FORTRAN STOP" ;;
  esac

# Calcolo l'altezza dalla superficie (dei livelli meteo)
  $util_dir/altipres_sim.exe $np $nzmet 2>&1 |grep -v "FORTRAN STOP"

# Calcolo mixing ratio
  $util_dir/mix_ratio.exe $np $nzmet 2>&1 |grep -v "FORTRAN STOP"

# Se richiesto, aggiungo cloud ice a cloud water
  if [ $clice = 0 ] ; then
    mv CLIQ_3D CLIQ_3D.org
    $util_dir/sum_unf_chimere.exe CLIQ_3D.org CICE_3D CLIQ_3D $np3d
  fi

# Correzioni solo per dati LM
  if [ $metmod = "LM" ] ; then

#   Soil Moisture: applico il fattore di riduzione e converto unita' di misura
    if [ $soilhum = 0 ] ; then
      mv SOIM_2D SOIM_2D.org
      $util_dir/scale_unf_chimere.exe SOIM_2D.org tmp.tmp $np $qsscale 0. \
        2>&1  |grep -v  "FORTRAN STOP" 
       
      $util_dir/scale_unf_chimere.exe tmp.tmp SOIM_2D $np 0.01 0.
        2>&1  |grep -v  "FORTRAN STOP" 
      rm tmp.tmp
    fi

#   Flussi di calore: cambio il segno per rispettare la convenzione Chimere
    if [ $flux = 0 ] ; then
      mv SSHF_2D SSHF_2D.org
      $util_dir/scale_unf_chimere.exe SSHF_2D.org SSHF_2D $np -1. 0.
        2>&1  |grep -v  "FORTRAN STOP" 
      mv SLHF_2D SLHF_2D.org
      $util_dir/scale_unf_chimere.exe SLHF_2D.org SLHF_2D $np -1. 0.
        2>&1  |grep -v  "FORTRAN STOP" 
    fi

  fi

#-------------------------------------------------------------------------------
# 2.7) Preparo le namelist e lancio diagmet

  echo ""
  echo "*** Diagmet"
  echo "Conversione NetCDF dei dati meteo di input"

  ln -sf $fncoord .
  cat <<EOF1 > lm2ncf.inp
${datac_ver}00 ${nhours} 
$nzmet
$fncoord
$nx,$ny
$uv10m
$ustar
$flux
$hmix
$clice
1
$lcloud
$mcloud
$hcloud
$soilhum
$cloudw
EOF1

  ${util_dir}/lm2ncf.exe

# METINFOS
  echo "Costruisco namelist"
  rm -f ${fnmetinfos}
  ln -sf ${proj_dir}/meteo/METINFOS-cosmo.nml.sed  .
  sed -e s/_START_/${datac_ver}00/          \
      -e s/_SHOW_/${show}/          \
      -e s/_UV10M_/${uv10m}/        \
      -e s/_USTA_/${ustar}/         \
      -e s/_FLUX_/${flux}/          \
      -e s/_HMIX_/${hmix}/          \
      -e s/_CLICE_/${clice}/        \
      -e s/_RAINWAT_/1/    \
      -e s/_URBOPT1_/${upblmin}/    \
      -e s/_URBOPT2_/${uflxadd}/    \
      -e s/_URBOPT3_/${uwinfac}/    \
      -e s/_EROS_/${eros}/          \
      -e s/_RESU_/${resu}/          \
      -e s/_URBOPT4_/${upblcor}/    \
      -e s/_OPT1_/${uframin}/       \
      -e s/_OPT2_/${ukzmin}/        \
      -e s/_OPT3_/${umomin}/        \
      -e s/_HMIOPT1_/${cldmix}/     \
      -e s/_HMIOPT2_/${kzexp}/      \
      -e s/_HMIOPT3_/${zimax}/      \
      ./METINFOS-cosmo.nml.sed > ${fnmetinfos}

# diagmet.nml
  echo "&args"                                 >  diagmet.nml
  echo "nhourrun    =   ${nhours}"             >> diagmet.nml
  echo "fniMETEO    = '"${diagmet_fniMETEO}"'" >> diagmet.nml
  echo "fniLDU      = '"$fnlanduse"'"          >> diagmet.nml
  echo "fniLAP      = '"$fnlandpar"'"          >> diagmet.nml
  echo "fniVCO      = '"${vcoord}"'"           >> diagmet.nml
  echo "fniMETINFOS = '"${fnmetinfos}"'"       >> diagmet.nml
  echo "fnoMET      = '"${fnmeteo}"'"          >> diagmet.nml
  echo "/"                                     >> diagmet.nml

# Lancio diagmet
  echo "Lancio diagmet (stdout ridirezionato su diagmet.log)"
  rm -f ${fnmeteo} 
  ln -sf  ${proj_dir}/src/diagmet.e .
  ./diagmet.e > diagmet.log 2>&1
  $NCDUMP -h ${fnmeteo} 2>/dev/null >/dev/null || \
    { echo "diagmet failure. Exiting" ; exit 4; }

# Salvo output 
  size_uc=`du -h $fnmeteo | awk '{print $1}'`
  mv $fnmeteo $meteo_dir/METEO.${proj}.${datac_ver}.nc
  rm -f $meteo_dir/METEO.${proj}.${datac_ver}.nc.gz
  gzip $meteo_dir/METEO.${proj}.${datac_ver}.nc
  echo "Salvati i dati di output: "$meteo_dir/METEO.${proj}.${datac_ver}.nc.gz
  echo "Dimensione del file non compresso: "$size_uc

done

#-------------------------------------------------------------------------------

echo "Elaborazioni terminate ",`date`
exit 0
