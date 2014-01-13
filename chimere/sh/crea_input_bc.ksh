#!/bin/ksh
#-------------------------------------------------------------------------------
# crea_input_bc.ksh
# Script per creare le BC chimiche (lat e top) per una serie di run di chimere.
#  
# Uso: crea_input_bc.ksh PROGETTO [-h]
#
# Input:
# - parametri di controllo del run (pre_chimere.inp)
# - griglia del run chimere e del run coarse (COORD, VCOORD)
# - elenco specie incluse nelle BC (run chimere coarse o dati climatologici)
# - archvio delle BC climatologiche oppure output binario del coarse run chimere
#
# NB:
# - Se si usano BC Chimere, LAT_CONCS e TOP_CONCS vengono creati direttamente da
#   prep*_chimere.e; se invece si usano BC climatologiche (MOZART2, LMDZINCA2, 
#   GOCART), vengono creati separatamente i files *_gas e *_aero, che sono poi 
#   concatenati dal programma concat.e
#
#                                     Versione 5.2 (V200501H), Enrico 12/12/2006
#-------------------------------------------------------------------------------
#set -x

#-------------------------------------------------------------------------------
# Scrive a schermo l'help della procedura
function write_help
{
  echo "Uso: crea_input_bc.ksh PROJ [-h]"
  echo "Tutti i files di input devono essere gia' pronti"
  return
}

################################################################################
# 1) Elaborazioni preliminari
#

echo "*** CREA_INPUT_BC.KSH ***"

#-------------------------------------------------------------------------------
# 1.1) Gestione parametri (nome progetto)
if [ $# -eq 0 ] ; then
  write_help
  exit
fi
if [ $1 = "-h" ] ; then
  write_help
  exit
fi
proj=$1

#-------------------------------------------------------------------------------
# 1.2) Definisco le variabili d'ambiente Chimere

#. $HOME_MINGUZZI/chimere/bin_dev/chimere_env.ksh $proj
. $HOME_MINGUZZI/chimere/bin/chimere_env.ksh $proj
getgrib=$HOME_MINGUZZI/gribarch_util/bin/getgrib.ksh
FNCOO="COORD"
FNCCOO="CCOORD"

ndays=`$delta_days $date_end $date_ini 2>/dev/null`
ndays=`expr $ndays + 1`

if [ $aerosols -eq 0 -a $latbounaer != "NONE" ] ; then
  echo "Simulazione solo gas, non utilizzo le BC aerosol"
  latbounaer="NONE"
  topbounaer="NONE"
fi

# Controlli sui parametri

# Lista combinazioni ammesse per le BC laterali
if [ $latbounaer = "NONE" ] ; then
  latok="Y"
elif [ $latboungas = "CHI_ARC" -a $latbounaer = "CHI_ARC" ] ; then
  latok="Y"
elif [ $latboungas = "CHI_RUN" -a $latbounaer = "CHI_RUN" ] ; then
  latok="Y"
elif [ $latboungas = "LMDZINCA2" -a $latbounaer = "GOCART" ] ; then
  latok="Y"
elif [ $latboungas = "MOZART2" -a $latbounaer = "GOCART" ] ; then
  latok="Y"
else
  echo "Combinazione illegale nelle BC laterali richieste:"
  echo "Gas: "$latboungas" aerosol: "$latbounaer
  exit
fi

# Lista combinazioni ammesse per le BC top
if [ $topbounaer = "NONE" ] ; then
  topok="Y"
elif [ $topboungas = "CHI_ARC" -a $topbounaer = "CHI_ARC" ] ; then
  topok="Y"
elif [ $topboungas = "CHI_RUN" -a $topbounaer = "CHI_RUN" ] ; then
  topok="Y"
elif [ $topboungas = "LMDZINCA2" -a $topbounaer = "GOCART" ] ; then
  topok="Y"
elif [ $topboungas = "MOZART2" -a $topbounaer = "GOCART" ] ; then
  topok="Y"
else
  echo "Combinazione illegale nelle BC top richieste:"
  echo "Gas: "$topboungas" aerosol: "$topbounaer
  exit
fi

# Se uso BC da Chimere, LAT_SPEC deve essere uguale a TOP_SPEC 
# (per gestire il fatto che siano diverse, raddoppiare le sez. 2.2 e 2.3)
if [ ${latboungas} = CHI_ARC -a ${topboungas} = CHI_ARC ] ; then
  diff $data_dir/LAT_SPEC.${mph} $data_dir/TOP_SPEC.${mph} >/dev/null 2>&1
  if [ $? -ne 0 ] ; then
    echo "Con BC CHI_ARC, le BC laterali e top devono avere le stess specie"
    exit
  fi
fi 

if [ ${latboungas} = CHI_RUN -a ${topboungas} = CHI_RUN ] ; then
  diff $data_dir/LAT_SPEC.${mph} $data_dir/TOP_SPEC.${mph} >/dev/null 2>&1
  if [ $? -ne 0 ] ; then
    echo "Con BC CHI_RUN, le BC laterali e top devono avere le stess specie"
    exit
  fi
fi 

#-------------------------------------------------------------------------------
# 1.3) Ripulisco la directory d'esecuzione e vi copio i files statici
#        
if [ $run_sync -eq 1 ] ; then
  tmp_dir=$tmp_root/bc
else
  tmp_dir=$tmp_root
fi
if [ ! -d $tmp_dir ] ; then
  echo "Directory di lavoro "$tmp_dir" inesistente"
  exit
fi
cd $tmp_dir
rm -f *

# Griglia di simulazione
cp $fis_dir/COORD_${dom}                ./COORD
cp $fis_dir/VCOORD.${vgname}            ./VCOORD

# Coarse run Chimere
if [ $latboungas = "CHI_ARC" -o $latboungas = "CHI_RUN" -o \
     $latbounaer = "CHI_ARC" -o $latbounaer = "CHI_RUN" -o \
     $topboungas = "CHI_ARC" -o $topboungas = "CHI_RUN" -o \
     $topbounaer = "CHI_ARC" -o $topbounaer = "CHI_RUN" ] ; then
  cp $domains_dir/COORD_${cdom}         ./CCOORD
  cp $domains_dir/VCOORD.${cvgname}     ./CVCOORD
  cp $data_dir/LAT_SPEC.${mph}          ./
  cp $data_dir/TOP_SPEC.${mph}          ./
fi

# BC gas by MOZART2
if [ $latboungas = "MOZART2" -o $topboungas = "MOZART2" ] ; then
  cp ${bspc_dir}/MOZART2_SPEC.${meca}    ./
fi

# BC gas by LMDZINCA2
if [ $latboungas = "LMDZINCA2" -o $topboungas = "LMDZINCA2" ] ; then
  cp ${bspc_dir}/LMDZINCA2_SPEC.${meca}    ./
fi

# BC aero by GOCART
if [ $latbounaer = "GOCART" -o $topbounaer = "GOCART" ] ; then
  cp ${bspc_dir}/GOCART_builder.${mph}  ./
  cp $data_dir/AEROSOL.${mph}           ./
fi

# I dataset Prev'Air operativi non contengono Ro, che deve essere inventata.
# NB: se i livelli verticali del run Chimere sono gli stessi di Prev'Air, il
#     grib di orografia non sarebbe necessario...
invent_ro="N"
if [ $latboungas = "CHI_ARC" -o $latbounaer = "CHI_ARC" -o \
     $topboungas = "CHI_ARC" -o $topbounaer = "CHI_ARC" ] ; then
  if [ $csim = "PRVBPA" -o $csim = "prvbpa" -o \
       $csim = "PRVEMR" -o $csim = "prvemr" -o \
       $csim = "PRV2000" -o $csim = "prv2000" -o \
       $csim = "PRV2010" -o $csim = "prv2010" -o \
       $csim = "PRV2020" -o $csim = "prv2020" ] ; then
    echo "Richiesta invenzione di Z e Ro"
    cp $domains_dir/orog_${cdom}.grb     ./
    invent_ro="Y"
  fi
fi

################################################################################
# 2) Ciclo sui run
# Nota: la variabile "datac" si riferisce all'inizio del run chimere; se sto 
#   elaborando un segmento di previsione (es da +24 a +48) questa NON coincide
#   con la data di emissione della previsione.

cnt=0
simp=dum
while [ $cnt -lt $ndays ] ; do
  cnt=`expr $cnt + 1`

#-------------------------------------------------------------------------------
# 2.1) Preliminari

# Calcolo la data corrente e i parametri per preplat che dipendono da questa
  incr=`expr $cnt - 1`
  datac=`$days $date_ini $incr 2>/dev/null`
  yy=`echo $datac | awk '{print substr($1,1,4)}'`
  mm=`echo $datac | awk '{print substr($1,5,2)}'`
  dd=`echo $datac | awk '{print substr($1,7,2)}'`
  datac_rev="$dd""$mm""$yy"
  sd0=${datac}00

  datacp1=`$days $datac +1 2>/dev/null`
  yyp1=`echo $datacp1 | awk '{print substr($1,1,4)}'`
  mmp1=`echo $datacp1 | awk '{print substr($1,5,2)}'`
  ddp1=`echo $datacp1 | awk '{print substr($1,7,2)}'`
  datacp1_rev="$ddp1""$mmp1""$yyp1"

  echo ""
  echo "***********************************************************************"
  echo "crea_input_bc.ksh: elaborazioni run "$datac " ("`date`")"

# Ripulisco la dir di lavoro dai files del run precedebte
  rm -f LAT_CONCS LAT_CONCS_gas LAT_CONCS_aer 
  rm -f TOP_CONCS TOP_CONCS_gas TOP_CONCS_aer 
  rm -f LAT_SPEC LAT_SPEC_gas LAT_SPEC_aer
  rm -f TOP_SPEC TOP_SPEC_gas TOP_SPEC_aer

#-------------------------------------------------------------------------------
# 2.2) Se sono richieste BC da archivio (opzione CHI_ARC), estraggo i dati 
#      e li riscrivo come se fossero l'output di un run Chimere

  if [ ${latboungas} = CHI_ARC -o ${topboungas} = CHI_ARC ] ; then
    echo "Estraggo da archivio GRIB"

#   Leggo la lista delle specie richieste
    dum=`wc -l LAT_SPEC.${mph}`
    nspc=`echo $dum | cut -d\  -f 1`
    cnt2=1
    spc=""
    while [ $cnt2 -lt $nspc ] ; do
      dum=`head -n $cnt2 LAT_SPEC.${mph} | tail -n 1 | awk '{print $1}'`
      spc=${spc}${dum},
      cnt2=`expr $cnt2 + 1`
    done
    spc=${spc}`head -n $cnt2 LAT_SPEC.${mph} | tail -n 1 | awk '{print $1}'`

#   Costruisco il .expa
    rm -f bc.expa
    cat <<EOF > bc.expa
      DS: $csim.
      DATES: $datac_rev.
      TIMES: 0000_2300/1.
      SCAD: an.
      LEV: *.
      VAR: $spc.
      DS: $csim.
      DATES: $datacp1_rev.
      TIMES: 0000.
      SCAD: an.
      LEV: *.
      VAR: $spc.
EOF

#   Estraggo i dati e li riscrivo nel formato Chimere, aggiungendo i campi Ro e
#   Z (valori fittizi i.e. costanti)
    $getgrib -d bc > getgrib.log
    cnlayers=`wc -l CVCOORD | gawk '{print $1}'`
    if [ $invent_ro = "Y" ] ; then
      $util_dir/grib2chimere.exe bc.grb bc.bin LAT_SPEC.${mph} $cnlayers \
        -roz CVCOORD orog_${cdom}.grb
    else
      $util_dir/grib2chimere.exe bc.grb bc.bin LAT_SPEC.${mph} $cnlayers
    fi

  fi

#-------------------------------------------------------------------------------
# 2.3) Se sono richieste BC da un altro run Chimere (opzione CHI_RUN), devo
#      riscrivere il file out.sim in modo che contenga le specie LAT_SPEC invece
#      di OUTPUT_SPECIES

  if [ ${latboungas} = CHI_RUN -o ${topboungas} = CHI_RUN ] ; then
    echo "Estraggo dall'output del coarse run le specie richieste per le BC"
    coarse_out=${csimul_dir}/out.${csim}.${datac}
    out_sp=${csimul_dir}/OUTPUT_SPECIES
    lat_sp=./LAT_SPEC.${mph}

    cnlayers=`wc -l CVCOORD | gawk '{print $1}'`
    nxc=`gawk '$1=="'${cdom}'" {print $2}' $domains_dir/DOMAINS`
    nyc=`gawk '$1=="'${cdom}'" {print $3}' $domains_dir/DOMAINS`
    npc=`expr $nxc \* $nyc \* $cnlayers`

    if [ ! -s $coarse_out ] ; then
      echo "Errore, file "$coarse_out" non trovato!"
      exit
    fi
    if [ ! -s $out_sp ] ; then
      echo "Errore, file "$out_sp" non trovato!"
      exit
    fi
    if [ ! -s $lat_sp ] ; then
      echo "Errore, file "$lat_sp" non trovato!"
      exit
    fi

    $util_dir/filtra_spec_out.exe $coarse_out $out_sp bc.bin $lat_sp $npc
  fi

#-------------------------------------------------------------------------------
# 2.4) BC laterali: interpolazione orizzontale 
#      Con BC climatologiche scrivo i files LAT_CONCS_gas e LAT_SPEC_gas.
#      Con BC Chimere scrivo direttamente LAT_CONCS e LAT_SPEC
#
# Standard input per preplat_CHIMERE.exe:
# sd0         data iniziale, nel formato YYYMMDDHH
# nsboun      n.ro di specie in lateral BC
# nxc,nyc     dimensioni coarse domain
# FNCCOO      file COORD del coarse domain
# FNCOO       file COORD
# coarsefile  output del coarse run
# VCOORD      livelli del run chimere
# CVCOORD     livelli del coarse run
  case ${latboungas} in

  MOZART2) 
    echo "Calcolo BC laterali gas (MOZART2)"
    if [ ! -s ${mozart2_dir}/MOZART1.${mm} ] ; then
      for file in `ls ${mozart2_dir}/MOZART1.?? 2>/dev/null` ; do
        gzip -v $file
      done
      gunzip -v ${mozart2_dir}/MOZART1.${mm}.gz
    fi
    input="${sd0} ${nhours} '"$FNCOO"' '"VCOORD"' '"${mozart2_dir}/MOZART1.${mm}"'"
    echo ${input} | $exe_dir/preplat_MOZART2.e
    cp MOZART2_SPEC.${meca} LAT_SPEC_gas ;;

  LMDZINCA2)
    echo "Calcolo BC laterali gas (LMDZINCA2)"
    if [ ! -s ${lmdzinca2_dir}/INCA.${mm} ] ; then
      for file in `ls ${lmdzinca2_dir}/INCA.?? 2>/dev/null` ; do
        gzip -v $file
      done
      gunzip -v ${lmdzinca2_dir}/INCA.${mm}.gz
    fi
    input="${sd0} ${nhours} '"$FNCOO"' '"VCOORD"' '"${lmdzinca2_dir}/INCA.${mm}"'"
    echo ${input} | $exe_dir/preplat_LMDZINCA2.e
    cp LMDZINCA2_SPEC.${meca} LAT_SPEC_gas ;;

  CHI_RUN | CHI_ARC)
    echo "Calcolo BC laterali (Chimere)"
    nxc=`gawk '$1=="'${cdom}'" {print $2}' $domains_dir/DOMAINS`
    nyc=`gawk '$1=="'${cdom}'" {print $3}' $domains_dir/DOMAINS`
    nsboun=`wc -l LAT_SPEC.${mph} | gawk '{print $1}'`
    coarsefile=./bc.bin
    input="${sd0} ${nsboun} ${nxc} ${nyc} '"$FNCCOO"' '"$FNCOO"' '"${coarsefile}"' VCOORD CVCOORD"
    echo ${input} 
    echo ${input} | $exe_dir/preplat_CHIMERE.e
    cp LAT_SPEC.${mph} LAT_SPEC ;;

  *)
    echo "*** Lateral boundaries type ${latboungas} undefined"
    exit ;;

  esac

#-------------------------------------------------------------------------------
# 2.5) BC laterali aero da GOCART.
#      Le BC gas e aero provengono da due fonti diverse: devo costruire i files 
#      LAT_CONCS_aer e LAT_SPEC_aer, e appenderli ai corrispondenti files *_gas

  case ${latbounaer} in
  GOCART)  
    echo "Calcolo BC laterali aero (Gocart)"

#   Interpolazione orizzontale (scrivo LAT_SPEC_aer e LAT_CONCS_aer)
    if [ ! -s ${gocart_dir}/GOCART_BCAV.${mm} ] ; then
      for file in `ls ${gocart_dir}/GOCART_BCAV.?? 2>/dev/null` ; do
        gzip -v $file
      done
      gunzip -v ${gocart_dir}/GOCART_BCAV.${mm}.gz
    fi
    input="${sd0} ${nhours} '"$FNCOO"' '"VCOORD"' '"${gocart_dir}/GOCART_BCAV.${mm}"' '"${bspc_dir}/GOCART_builder.${mph}"' '"AEROSOL.${mph}"' '"${gocart_dir}"'"
    echo ${input} | $exe_dir/preplat_GOCART.e

#   Appendo concentrazioni ed elenco specie a quelle gia' costruite per i gas
    input="'"LAT_SPEC_gas"' '"LAT_SPEC_aer"' '"LAT_CONCS_gas"' '"LAT_CONCS_aer"' '"LAT_CONCS"' "lat
    echo ${input} | $exe_dir/concat.e
    cat LAT_SPEC_gas LAT_SPEC_aer > LAT_SPEC ;;

  CHI_RUN | CHI_ARC)
    echo >/dev/null ;;

  LMDZINCA2 | MOZART2)
    cp LAT_SPEC_gas LAT_SPEC
    cp LAT_CONCS_gas LAT_CONCS ;;
  esac

  cp LAT_CONCS LAT_CONCS.${proj}.${datac}
  mv LAT_CONCS.${proj}.${datac} $latboun_dir
  cp LAT_SPEC tmp.spec 
  mv tmp.spec $latboun_dir/LAT_SPEC

#-------------------------------------------------------------------------------
# 2.6) BC top: interpolazione orizzontale 
#      Con BC climatologiche scrivo i files TOP_CONCS_gas e TOP_SPEC_gas.
#      Con BC Chimere scrivo direttamente TOP_CONCS e TOP_SPEC

  case ${topboungas} in

  MOZART2) 
    echo "Calcolo BC top gas (MOZART2)"
    if [ ! -s ${mozart2_dir}/MOZART1.${mm} ] ; then
      for file in `ls ${mozart2_dir}/MOZART1.?? 2>/dev/null` ; do
        gzip -v $file
      done
      gunzip -v ${mozart2_dir}/MOZART1.${mm}.gz
    fi
    input="${sd0} ${nhours} '"$FNCOO"' '"VCOORD"' '"${mozart2_dir}/MOZART1.${mm}"'"
    echo ${input} | $exe_dir/preptop_MOZART2.e
    cp MOZART2_SPEC.${meca} TOP_SPEC_gas ;;

  LMDZINCA2)
    echo "Calcolo BC top gas (LMDZINCA2)"
    if [ ! -s ${lmdzinca2_dir}/INCA.${mm} ] ; then
      for file in `ls ${lmdzinca2_dir}/INCA.?? 2>/dev/null` ; do
        gzip -v $file
      done
      gunzip -v ${lmdzinca2_dir}/INCA.${mm}.gz
    fi
    input="${sd0} ${nhours} '"$FNCOO"' '"VCOORD"' '"${lmdzinca2_dir}/INCA.${mm}"'"
    echo ${input} | $exe_dir/preptop_LMDZINCA2.e
    cp LMDZINCA2_SPEC.${meca} TOP_SPEC_gas ;;

  CHI_RUN | CHI_ARC)
    echo "Calcolo BC top (Chimere)"
    nxc=`gawk '$1=="'${cdom}'" {print $2}' $domains_dir/DOMAINS`
    nyc=`gawk '$1=="'${cdom}'" {print $3}' $domains_dir/DOMAINS`
    nsboun=`wc -l TOP_SPEC.${mph} | gawk '{print $1}'`
    coarsefile=./bc.bin

    input="${sd0} ${nsboun} ${nxc} ${nyc} '"$FNCCOO"' '"$FNCOO"' '"${coarsefile}"' VCOORD CVCOORD"
    echo ${input} | $exe_dir/preptop_CHIMERE.e
    cp TOP_SPEC.${mph} TOP_SPEC ;;

  *)
    echo "*** Top boundaries type ${topboungas} undefined"; exit 1 ;;

  esac

#-------------------------------------------------------------------------------
# 2.7) BC top aero da GOCART.
#      Le BC gas e aero provengono da due fonti diverse: devo costruire i files 
#      TOP_CONCS_aer e TOP_SPEC_aer, e appenderli ai corrispondenti files *_gas

  case ${topbounaer} in
  GOCART) 
    echo "Calcolo BC top aero (Gocart)"

#   Interpolazione orizzontale (scrivo TOP_SPEC_aer e TOP_CONCS_aer)
    if [ ! -s ${gocart_dir}/GOCART_BCAV.${mm} ] ; then
      for file in `ls ${gocart_dir}/GOCART_BCAV.?? 2>/dev/null` ; do
        gzip -v $file
      done
      gunzip -v ${gocart_dir}/GOCART_BCAV.${mm}.gz
    fi
    input="${sd0} ${nhours} '"$FNCOO"' '"VCOORD"' '"${gocart_dir}/GOCART_BCAV.${mm}"' '"${bspc_dir}/GOCART_builder.${mph}"' '"AEROSOL.${mph}"' '"${gocart_dir}"'"
    echo ${input} | $exe_dir/preptop_GOCART.e

#   Appendo concentrazioni ed elenco specie a quelle gia' costruite per i gas
    input="'"TOP_SPEC_gas"' '"TOP_SPEC_aer"' '"TOP_CONCS_gas"' '"TOP_CONCS_aer"' '"TOP_CONCS"' "top
    echo ${input} | $exe_dir/concat.e
    cat TOP_SPEC_gas TOP_SPEC_aer > TOP_SPEC ;;

  CHI_RUN | CHI_ARC)
    echo >/dev/null ;;

  *)
    cp TOP_SPEC_gas TOP_SPEC
    cp TOP_CONCS_gas TOP_CONCS ;;
  esac

  cp TOP_CONCS TOP_CONCS.${proj}.${datac}
  mv TOP_CONCS.${proj}.${datac} $topboun_dir
  cp TOP_SPEC tmp.spec
  mv tmp.spec $topboun_dir/TOP_SPEC

done

################################################################################
# 3) Conclusione

# Se richiesto, ricomprimo gli archivi BC
if [ ${latboungas} = "LMDZINCA2" -o ${topboungas} = "LMDZINCA2" ] ; then
  for file in `ls ${lmdzinca2_dir}/INCA2.?? 2>/dev/null` ; do
    gzip -v $file
  done
fi
if [ ${latboungas} = "MOZART2" -o ${topboungas} = "MOZART2" ] ; then
  for file in `ls ${mozart2_dir}/MOZART1.?? 2>/dev/null` ; do
    gzip -v $file
  done
fi
if [ ${latbounaer} = "GOCART" -o ${topbounaer} = "GOCART" ] ; then
  for file in `ls ${gocart_dir}/GOCART_BCAV.?? 2>/dev/null` ; do
    gzip -v $file
  done
fi
