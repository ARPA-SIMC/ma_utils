#!/bin/bash
unset LANG
#-------------------------------------------------------------------------------
# crea_input_bc.sh
# Script per creare le BC chimiche (lat e top) per una serie di run di chimere.
#  
# Uso: crea_input_bc.sh PROGETTO [-h]
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
#                                     Versione 6.0 (V200709C), Michele  14/03/2008
#-------------------------------------------------------------------------------
#set -x

#-------------------------------------------------------------------------------
# Scrive a schermo l'help della procedura
function write_help
{
  echo "Uso: crea_input_bc.sh PROJ [-h]"
  echo "Tutti i files di input devono essere gia' pronti"
  return
}

################################################################################
# 1) Elaborazioni preliminari
#

echo "*** CREA_INPUT_BC.SH ***"

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

.  $HOME_STORTINI/chimere/bin_fed16/chimere_env.sh $proj


NCDUMP=/usr/bin/ncdump
fncoord=${dom_dir}/HCOORD/COORD_${dom}
vcoord=${dom_dir}/VCOORD/VCOORD_${nlayer_chimere}_${first_layer_pressure}_${top_chimere_pressure}
fccoord=${domains_dir}/HCOORD/COORD_${cdom}
fnivco=${domains_dir}/VCOORD/VCOORD_${nlayer_chimere}_${first_layer_pressure}_${top_chimere_pressure}
#echo " vcoord" $vcoord
#echo $fnivco
ndays=`$delta_days $date_end $date_ini 2>/dev/null`
ndays=`expr $ndays + 1`

echo $ndays

nczonal=`awk 'BEGIN{stop=0}{x[FNR]=$1}END{for(k=2;k<=FNR;k++) {if(x[k-1]>x[k]&&stop==0){print k-1;stop=1}}}' ${fccoord}`

echo 'nczonal '$nczonal
nctotal=`wc -l  ${fccoord} | gawk '{print $1}' `
echo 'nctotal '$nctotal
ncmerid=`expr ${nctotal} /  ${nczonal}`
nptotal=`wc -l  ${fncoord} | gawk '{print $1}' `
echo 'ncmeridl '$ncmerid



if [ $aero -eq 0 -a $boundaer != "NONE" ] ; then
  echo "Simulazione solo gas, non utilizzo le BC aerosol"
  boundaer="NONE"
fi

if [ ${mecachim} -gt 0 ] ; then
reactive=1
fi

nested="yes"
ibound=1
if [ ${reactive} -eq 0 -a ${nbins} -eq 0 ] ; then
   if [ "${nested}" = "no" ] ; then
      ibound=0
   fi
fi


#if [ $2 != " " ] ; then
#latboun_dir=$path$pathoutput/bc
#fi
echo " dir lat" $latboun_dir  

#echo $boundgas 
#echo $nbins 
#echo $reactive 
#echo $cdom
cd ${tmp_run}
echo ${tmp_run}
#ripulisco
#rm -f *
#
invent_ro="N"
if [ $boundgas = "CHI_ARC" -o $boundaer = "CHI_ARC" -o  $boundgas = "CHI_RUN" -o $boundaer = "CHI_RUN" ] ; then
nested="y"
invent_ro="Y"
echo "richiesta invenzione Z e ro"    
cp  $HOME_STORTINI/chimere/dat/orog_${cdom}.grb . 
fi

#############################################################################o #
# 2) Ciclo sui run
# Nota: la variabile "datac" si riferisce all'inizio del run chimere; se sto 
#   elaborando un segmento di previsione (es da +24 a +48) questa NON coincide
#   con la data di emissione della previsione.
###!chempreffi=chem_dir
function checkexist {
    [ -s $1 ]|| { echo "non existent $1 ; exiting." ; exit 1; }
}
#########################

#echo $chem_dir/inputdata.${labchem}
fnaerosol=$chem_dir/inputdata.${labchem}/AEROSOL

cnt=0
simp=dum
   data5=`$days $date_ini +5 2>/dev/null`
   datein=${date_ini} 
        datafilein=${datein} 
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
  echo "crea_input_bc.sh: elaborazioni run "$datac " ("`date`")"   $cnt
   if [ ${csim} == 2007E2007 -o ${csim} == 2007E2020 ] ; then
   if [ $datac -lt ${data5} ] ; then
#        datafilein=${datein} 
  echo " 0 elaborazioni run  fino alla data "$data5
  echo " 0 elaborazioni run dal file   "$datafilein
     else
        datafilein=${datac} 
     data5=`$days $datafilein +5 2>/dev/null`
  echo " 1 elaborazioni run  fino alla data "$data5
  echo " 1 elaborazioni run dal file   "$datafilein
     fi
   fi



   chemprepfic=${chem_dir}/inputdata.${labchem}
####### echo ${boundgas}
  if [ ${boundgas} != "CHI_ARC"  -a ${boundgas} != "CHI_RUN" ] ; then


if [ ${ibound} == 1 ] ; then
    fnbounsel=${chemprepfic}/source_selector
    checkexist ${fnbounsel}
fi

#GOCART
   echo aqqq  $fnbounsel 
[ ${nbins} -ne 0 ] && grep -v ^\s*# ${fnbounsel} | \
   awk '{print $2}' | grep -q GOCART && { \
   fngocart_builder=${chemprepfic}/GOCART_builder
   checkexist ${fngocart_builder}
   fngocart_boun=${gocartdir}/GOCART_BCAV.${mm}
   checkexist ${fngocart_boun}
}
#LMDzINCA2 data
[ ${reactive} == 1 ] &&  grep -v ^\s*# ${fnbounsel} | \
    awk '{print $2}' | grep -q LMDZINCA2 && { \

   fnlmdzinca2_spec=${chemprepfic}/LMDZINCA2_SPEC
   checkexist ${fnlmdzinca2_spec}
   fnlmdzinca2_boun=${lmdzinca2dir}/INCA.${mm}
   checkexist ${fnlmdzinca2_boun}
}
# MOZART2 data
[ ${reactive} == 1 ] &&  grep -v ^\s*# ${fnbounsel} | \
    awk '{print $2}' | grep -q MOZART2 && { \
   fnmozart2_spec=${chemprepfic}/MOZART2_SPEC
   checkexist ${fnmozart2_spec}
   fnmozart2_boun=${mozart2dir}/MOZART1.${mm}
   checkexist ${fnmozart2_boun}
}

    fi
#

#-------------------------------------------------------------------------------
# 2.2) Se sono richieste BC da archivio (opzione CHI_ARC), estraggo i dati 
#      e li riscrivo come se fossero l'output di un run Chimere
cd ${tmp_run}
  if [ ${boundgas} == CHI_ARC -o ${boundaer} == CHI_ARC ] ; then
    echo "Estraggo da archivio GRIB"
     echo $AWK
     LAT_SPEC_GRB_2005=${chem_dir}/inputdata.${labchem}/PRVBPA_SPEC_2005
     LAT_SPEC_GRB_2008=${chem_dir}/inputdata.${labchem}/PRVBPA_SPEC_2008
#   Leggo la lista delle specie richieste
     if [ $datac  -lt 20080521 ] ; then
        if [ $soatyp -eq 1 ]  ; then 
            LAT_SPEC_GRB=${LAT_SPEC_GRB_2005}
        elif  [ $soatyp -eq 2 ]  ; then 
           echo " attenzione mancano i SOA  per soatyp " =$soaptyp 
           LAT_SPEC_GRB=${LAT_SPEC_GRB_2005}
        fi
     elif [ $datac  -ge 20080521  ] ; then 
     LAT_SPEC_GRB=${LAT_SPEC_GRB_2008}
     fi
      
      echo "lat_spec_grb" $LAT_SPEC_GRB
    dum=`wc -l $LAT_SPEC_GRB`
    nspc=`echo $dum | cut -d\  -f 1`
   echo "nspec " $nspc
    cnt2=1
    spc=""
    
    while [ $cnt2 -lt $nspc ] ; do
      dum=`head -n $cnt2 $LAT_SPEC_GRB | tail -n 1 | awk '{print $1}'`
      spc=${spc}${dum},
      cnt2=`expr $cnt2 + 1`
    done
    spc=${spc}`head -n $cnt2 $LAT_SPEC_GRB | tail -n 1 | awk '{print $1}'`
echo "costruisco query "

#   Estraggo i dati e li riscrivo nel formato Chimere, aggiungendo i campi Ro e
#   Z (valori fittizi i.e. costanti)
   rm -f bc.akq bc.grb tmp.grib
   echo $PWD 
   echo "reftime:>= ${yy}-${mm}-${dd} 00, <=${yyp1}-${mmp1}-${ddp1} 00" >bc.akq
   arki-mergeconf http://arkimet.metarpa:8090/dataset/prvbpaope  >prvbpaope.conf
   arki-query --data --file=bc.akq -C prvbpaope.conf > tmp.grib  
   arki-scan --data --sort=reftime,level,product tmp.grib > bc.grb
     
#    cnlayers=`wc -l $nvcoord | gawk '{print $1}'`
#    echo "nlay"  
   if [ $invent_ro = "Y" ] ; then
   echo "invento ororo"
    ${chimere_root}/bin_fed16/grib2chimere.exe bc.grb bc.bin $LAT_SPEC_GRB    $nvcoord  -roz $fnivco orog_${cdom}.grb
#    /usr/libexec/ma_utils/grib2chimere.exe bc.grb bc.bin $LAT_SPEC_GRB    $nvcoord \
    else
   echo "noi invento ororo"
#     ${chimere_root}/bin_fed16/grib2chimere.exe bc.grb bc.bin $lspc $nvcoord
     /usr/libexec/ma_utils/grib2chimere.exe bc.grb bc.bin $lspc $nvcoord
    fi

####tentativo di  gestire versione diverse NOTA BENE da testare
if [ $datac -ge 20080521 -a  $nbins -eq 6  -a  $soatyp -eq 1 ]   ; then
      echo "devo riassegnare i bin e soa"
      npc=`expr ${nctotal} \* ${nvcoord}`
      echo $npc
#      ${chimere_root}/bin/filtra_spec_out.exe  bc.bin  $LAT_SPEC_GRB_2008  bcn.bin $LAT_SPEC_GRB_2005  $npc
      /usr/libexec/ma_utils/filtra_spec_out.exe  bc.bin  $LAT_SPEC_GRB_2008  bcn.bin $LAT_SPEC_GRB_2005  $npc
     else
     mv bc.bin bcn.bin
     fi   


    if [ $datac -lt 20080521  ]  ; then
      if [ $nbins -eq 6  -a $soatyp -eq 1 ]   ; then
        LAT_SPEC=${chem_dir}/inputdata.${labchem}/LAT_SPEC_2005
         echo " trasformo file bc in netcdf  usando  lat_spec= " $LAT_SPEC " nbins= " $nbins " e soatyp= " $soatyp
      fi
      if [ $nbins -eq 6  -a $soatyp -eq 2 ]   ; then
        LAT_SPEC=${chem_dir}/inputdata.${labchem}/LAT_SPEC_2005_SOA
         echo " attenzione mancano i SOA "
         echo " trasformo file bc in netcdf usando  lat_spec= " $LAT_SPEC " nbins= " $nbins " e soatyp= " $soatyp
      fi
    else 
        if [ $nbins -eq 8  -a $soatyp -eq 2 ]   ; then
        LAT_SPEC=${chem_dir}/inputdata.${labchem}/LAT_SPEC_2008
         echo " trasformo file bc in netcdf  usando  lat_spec= " $LAT_SPEC " nbins= " $nbins " e soatyp= " $soatyp
        fi
        if [ $nbins -eq 8  -a $soatyp -eq 1 ]   ; then
        LAT_SPEC=${chem_dir}/inputdata.${labchem}/LAT_SPEC_2008
         echo " trasformo file bc in netcdf  usando  lat_spec= " $LAT_SPEC " nbins= " $nbins " e soatyp= " $soatyp
        fi
        if [ $nbins -eq 6  -a $soatyp -eq 1 ]   ; then
        LAT_SPEC=${chem_dir}/inputdata.${labchem}/LAT_SPEC_2005
        echo " trasformo file bc in netcdf  usando  lat_spec= " $LAT_SPEC " nbins= " $nbins " e soatyp= " $soatyp
        fi
     fi
    dum=`wc -l $LAT_SPEC`
    nspc=`echo $dum | cut -d\  -f 1`


#for f in ${files}; do
#    if [ -s ${bindir}/$f ]; then
        fnout=$tmp_run/bc.bin.nc
        fnin=$tmp_run/bcn.bin
        echo "&args"                          > chmbc2cdf.nml
        echo "fnout     = '"${fnout}"'"      >> chmbc2cdf.nml
        echo "fnin      = '"${fnin}"'"       >> chmbc2cdf.nml
        echo "fnoutspec = '"${LAT_SPEC}"'"  >> chmbc2cdf.nml
        echo "fcoord   = '"${fccoord}"'"    >> chmbc2cdf.nml
        echo "fvcoord     = '"${fnivco}"'"    >> chmbc2cdf.nml
        echo "fnaerosol   = '"${fnaerosol}"'"    >> chmbc2cdf.nml
        echo "nzonal    =   ${nczonal}"       >> chmbc2cdf.nml
        echo "nmerid    =   ${ncmerid}"       >> chmbc2cdf.nml
        echo "nivout    =   ${nvcoord}"       >> chmbc2cdf.nml
        echo "nspectot  =   ${nspc}"     >> chmbc2cdf.nml
        echo "domain    = '"${cdom}"'"     >> chmbc2cdf.nml
        echo "nphour    =   6"       >> chmbc2cdf.nml
        echo "nprint    =   6"       >> chmbc2cdf.nml
        echo "/"                             >> chmbc2cdf.nml

        echo

 echo
        echo "   ***   processing $f ... "
        echo

#         ${util_dir}/chmbc2cdf.e
         /usr/libexec/ma_utils/chmbc2cdf.exe

    coarsefile=${tmp_run}/bc.bin.nc
   echo "ho scritto il file nc"
 fi
#-------------------------------------------------------------------------------
# 2.3) Se sono richieste BC da un altro run Chimere (opzione CHI_RUN), devo
#      riscrivere il file out.sim in modo che contenga le specie LAT_SPEC invece
#      di OUTPUT_SPECIES

  if [ ${boundgas} == CHI_RUN  -o ${boundaer} == CHI_RUN ] ; then
    echo "Estraggo dall'output del coarse run le specie richieste per le BC"

    if [ ${csim} == 2007E2007  -o ${csim} == 2007E2020 ] ; then
     
     coarsefile=${csimul_dir}/out-COARSE_all.${datafilein}.nc
     
    else
     coarsefile=${csimul_dir}/out.${csim}.${datac}.nc
    fi

#    coarsefile=bc.bin.nc
    echo ${coarsefile}

  fi
#



function checkexist {
  [ -s $1 ] || { echo "non existent $1 ; exiting. " ; exit 1;   }
}


#-------------------------------------------------------------------------------
echo 
cd ${exe_dir}
echo $exe_dir
echo "o Preparing  boundary concentrations..."
if [ ${ibound} == 1 ];then
#rm -f ${fnlatconc}
if [ ${nested} = "no" ] ; then
      echo "  o COARSE run,  lateral B. C. are taken from: climatology "
#      rm -f ${fnlatconc_gas}
    ln -sf  ${proj_root}/${proj}/src/prep_bound.e .
	    [ -x prep_bound.e ]||\
		{ echo "Non existent prep_bound.e. Exiting. "; exit 1; }
#checkexec prep_bound.e
        # Build args namelist for prep_bound.e
        echo "&args"                                         > prep_bound.nml
        echo "id0               = ${sd0}"                    >>prep_bound.nml
        echo "nhourrun          = ${nhours}"                 >>prep_bound.nml
        echo "fniCOO            = '"${fncoord}"'"            >>prep_bound.nml
        echo "fniVCO            = '"${vcoord}"'"             >>prep_bound.nml
        echo "fniBOUN_mozart2   = '"${fnmozart2_boun}"'"     >>prep_bound.nml
        echo "fniSPEC_mozart2   = '"${fnmozart2_spec}"'"     >>prep_bound.nml
        echo "fniBOUN_lmdzinca2 = '"${fnlmdzinca2_boun}"'"   >>prep_bound.nml
        echo "fniSPEC_lmdzinca2 = '"${fnlmdzinca2_spec}"'"   >>prep_bound.nml
        echo "fniBOUN_gocart    = '"${fngocart_boun}"'"      >>prep_bound.nml
        echo "gocart_builder    = '"${fngocart_builder}"'"   >>prep_bound.nml
        echo "fniBOUN_lmdzaero  = '"${fnlmdzaero_boun}"'"    >>prep_bound.nml
        echo "lmdzaero_builder  = '"${fnlmdzaero_builder}"'" >>prep_bound.nml
        echo "aerosol           = '"${fnaerosol}"'"          >>prep_bound.nml
        echo "gocartdir         = '"${gocartdir}"'"          >>prep_bound.nml
        echo "fno               = '"${fnbounconc}"'"         >>prep_bound.nml
	    echo "/"                                  >>prep_bound.nml
	    ./prep_bound.e
	    $NCDUMP -h ${fnbounconc} 2>/dev/null >/dev/null || \
		{ echo "prep_bound.e failure. Exiting";  }
#		{ echo "prep_bound.e failure. Exiting"; exit 1; }
    else
    echo "+ This is a  NEST run"
    ln -sf  ${proj_root}/${proj}/src/prep_chimere.e .
    [ -x prep_chimere.e ]||{ echo "Non existent prep_CHIMERE.e. Exiting. "; exit 1; }
    echo "+ coarse file = "${coarsefile}
     fnbounconc="BOUN_CONCS.nc"
     fniniconc="INIT_CONC.nc"
    # Build args namelist for preptop_chimere
    echo "&args"                          >  prep_chimere.nml
    echo "id0      =   ${sd0}"            >> prep_chimere.nml
    echo "nhourrun =   ${nhours}"         >> prep_chimere.nml
    echo "fniCOO   = '"${fncoord}"' "     >> prep_chimere.nml
    echo "fni      = '"${coarsefile}"' "  >> prep_chimere.nml
    echo "fniVCO   = '"${vcoord}"'"       >> prep_chimere.nml
    echo "fnaerosol =  '"${fnaerosol}"'"  >> prep_chimere.nml
    echo "fno     =  '"${fnbounconc}"'"    >> prep_chimere.nml
    echo "fnini =  '"${fniniconc}"'"  >> prep_chimere.nml
    echo "/"                              >> prep_chimere.nml
    
    ./prep_chimere.e 

$NCDUMP -h ${fnbounconc} 2>/dev/null >/dev/null || \
            { echo "prep_chimere.e failure." ; }
#            { echo "prep_chimere.e failure. Exiting"; exit 1; }


    fi
    fi



######################################################
# Initialization 
######################################################
     if [ $cnt = 1  -a $init = 1 ] ; then
       echo "o Initial conditions are an interpolation of global fields "
       mv ${exe_dir}/INIT_CONC.nc  ${output_dir}/INIT_CONC.${proj}.${datac}.nc
     fi
if [ $cnt = 1 -a "${init}" = "0" ] ; then
  echo "  o Initialization from Boundary Conditions"
fi

  mv ${exe_dir}/BOUN_CONCS.nc ${latboun_dir}/BOUN_CONCS.${proj}.${datac}.nc


#  gzip ${latboun_dir}/BOUN_CONCS.${proj}.${datac}.nc

done
