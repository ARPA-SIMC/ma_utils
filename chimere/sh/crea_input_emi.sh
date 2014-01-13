#/bin/ksh
#-------------------------------------------------------------------------------
# crea_input_emi.sh 
# Script per creare l'input relativo alle emissioni antropogeniche per una serie
# di run di chimere.
#
# Uso: crea_input_emi.sh PROGETTO [-h]
#
# Input:
# - parametri di controllo del run (pre_chimere.inp)
# - inventario delle emissioni
#
# Note:
#
#                                     Versione 6.0 (V200709C), Michele 01/04/2008
#-------------------------------------------------------------------------------
#set -x

#-------------------------------------------------------------------------------
# Scrive a schermo l'help della procedura
function write_help
{
  echo "Uso: crea_input_emi.sh PROJ  bemi antroemi [-h]"
  echo "Tutti i files di input devono essere gia' pronti"
  return
}

################################################################################
# 1) Elaborazioni preliminari
#

echo "*** CREA_INPUT_EMI.SH ***"

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
opt1=$2
opt2=$3

echo $opt1
#-------------------------------------------------------------------------------
# 1.2) Definisco le variabili d'ambiente Chimere

#. $HOME_MINGUZZI/chimere/bin_dev/chimere_env.ksh $proj
#. $HOME_MINGUZZI/chimere/bin/chimere_env.ksh $proj
.  $HOME_STORTINI/chimere/bin_fed16/chimere_env.sh $proj

ndays=`$delta_days $date_end $date_ini 2>/dev/null`
ndays=`expr $ndays + 1`

if [ ${mecachim} -gt 0 ] ; then
reactive=1
fi


NCDUMP=/usr/bin/ncdump
fncoord=${domains_dir}/HCOORD/COORD_${dom}
fnanthro=${chemprepfic}/ANTHROPIC
echo $fncoord

fnanthro=${chem_dir}/inputdata.${labchem}/ANTHROPIC



#if [ ${aero} -eq 1 -o ${reactive} -gt 0 -o ${carb} -eq 1 -o ${trc} -ge 1 ] ; then
# checkexist ${fnanthro}
#fi


# Controlli sui parametri
if [ $plmrise -eq 1 -a $nlevemis -le 1 ] ; then
  echo "Lo schema di Plum Rise richiede emissioni su piu di un livello"
  exit
fi
 if [ $plmrise -eq 2 ] ; then
  echo "Lo schema di Plum Rise 2"
  fi
if [ $scad0 != -1 -a $scad0 -ne `expr $scad0 / 24 \* 24` ] ; then
  echo "la scadenza iniziale deve essere -1 oppure un multiplo di 24"
  exit
fi

#-------------------------------------------------------------------------------
# 1.3) Ripulisco la directory d'esecuzione

if [ $run_sync -eq 1 ] ; then
  tmp_dir=$tmp_run/emi
else
  tmp_dir=$tmp_run
fi
if [ ! -d $tmp_dir ] ; then
  echo "Directory di lavoro "$tmp_dir" inesistente"
  exit
fi
cd $tmp_dir
echo "tmp_dir" $tmp_dir
#rm -f *

################################################################################
# 2) Ciclo sui run
# Nota: la variabile "datac" si riferisce all'inizio del run chimere; se sto 
#   elaborando un segmento di previsione (es da +24 a +48) questa NON coincide
#   con la data di emissione della previsione.

cnt=0
while [ $cnt -lt $ndays ] ; do
  cnt=`expr $cnt + 1`

#-------------------------------------------------------------------------------
# 2.1) Preliminari

# Calcolo la data corrente e i parametri che dipendono da questa
  incr=`expr $cnt - 1`
  datac=`$days $date_ini $incr 2>/dev/null`
  mm=`echo $datac | awk '{print substr($1,5,2)}'`
  dd=`echo $datac | awk '{print substr($1,7,2)}'`
  sd0=${datac}00
  datacp1=`$days $datac 1 2>/dev/null`
  ddp1=`echo $datacp1 | awk '{print substr($1,7,2)}'`

  echo ""
  echo "***********************************************************************"
  echo "crea_input_emi.sh: elaborazioni run "$datac " ("`date`")"

# Ripulisco la dir di lavoro dai files del run precedente
#   rm -f AEMISSIONS*
#  rm -f BEMISSIONS*

## Se l'inventario e' in formato compresso, lo scompatto
#  if [ ! -s ${eminv_dir}/EMISSIONS-GRID-${dom}.${mm} ] ; then
#    if [ -s ${eminv_dir}/EMISSIONS-GRID-${dom}.${mm}.gz ] ; then
#      gunzip -v ${eminv_dir}/EMISSIONS-GRID-${dom}.${mm}.gz
#    else
#      echo "Errore, inventario "${eminv_dir}/EMISSIONS-GRID-${dom}.${mm}" non trovato"
#      exit
#    fi
#  fi
#  if [ ! -s ${eminv_dir}/EMISSIONS-PUNT-${dom}.${mm} ] ; then
#    if [ -s ${eminv_dir}/EMISSIONS-PUNT-${dom}.${mm}.gz ] ; then
#      gunzip -v ${eminv_dir}/EMISSIONS-PUNT-${dom}.${mm}.gz
#    else
#      echo "Errore, inventario "${eminv_dir}/EMISSIONS-PUNT-${dom}.${mm}" non trovato"
#      exit
#    fi
#  fi
echo ${dom}

#fnemis=${eminv_dir}/EMISSIONS-${dom}.${mm}.s.nc
echo "emissioni" $fnemis
fnstack=${eminv_dir}/EMISSIONS-PUNT${dom}.${mm}.s
fnemisa=${tmp_run}/AEMISSIONS.nc
fnemis_trc=${chem_dir}/inputdata.${labchem}/TRACER_PROFILE
fntrc=${chem_dir}/inputdata.${labchem}/TRACER
fnmeteo=${meteo_dirl_ispra}/METEO.${proj}.${datac}.nc
fnmeteo=${meteo_dir_ispra}/METEO.${proj}.${datac}.nc
#fnmeteo=${meteo_dir_opera}/METEO.${proj}.${datac}.nc

echo "file met"  $fnmeteo  
fnerror=${exe_dir}/ERROR.txt
echo 'aaa' $fnerror
rm $fnerror
#   gunzip  ${meteo_dir_ispra}/METEO.${proj}.${datac}.nc.gz
#   gunzip -c  ${meteo_dir_ispra}/METEO.${proj}.${datac}.nc.gz >${meteo_dir_ispra}/METEO.${proj}.${datac}.nc


# controlla se esiste file meteo
  status="WAIT"
    first="TRUE"
    while [ $status = "WAIT" ] ; do
if [ ! -s ${fnmeteo} ] ; then
    if [ $first = "TRUE" ] ; then
    echo "aspetto file meteo !! "$proj   ${fnmeteo}
     fi
   sleep 30s
     first="FALSE"
  else
     status="OK"
    echo "File meteo "$proj" ok          "`date -u +"%d-%b-%Y %T"`
  fi

 done


#m now on you are in the ${chimere_tmp} directory #
#######################################################

cd ${exe_dir}

echo "sono qui"
#######################################
# Definition of simulation parameters #
#######################################
echo $exe_dir

#sd0=${year}${month}${day}${hour}
nzonal=`$AWK 'BEGIN{stop=0}{x[FNR]=$1}END{for(k=2;k<=FNR;k++) {if(x[k-1]>x[k]&&stop==0){print k-1;stop=1}}}' ${fncoord}`
ntotal=`wc -l  ${fncoord} | gawk '{print $1}' `
nmerid=`expr ${ntotal} /  ${nzonal}`
show=`expr ${nzonal} \* ${nmerid}`
show=`expr ${show} / 2`
show=`expr ${show} + ${nzonal}`
echo "o simulation over horizontal domain: " ${dom}
echo "  "${dom}" >> "${nzonal}"x"${nmerid}" cells"
[ -z $nzonal ]&& \
    { echo "${myself}: No such domain ${dom}. Exiting."; exit 1; }

nemisa=` cat ${fnanthro}   |grep -v "^ *$"|wc -l | $AWK '{print $1}'`
#nemisb=` cat ${fnbiogen}   |grep -v "^ *$"|wc -l | $AWK '{print $1}'`

echo $nemisa
echo $emis
echo $aero
echo $reactive
echo $carb
echo $trc
echo $flatreduc



fnemisb=$emi_dir_ispra/BEMISSIONS.${proj}.${datac}.nc
#fnemisb=$emi_dir/BEMISSIONS.${proj}.${datac}.nc
fnmetinfos=$tmp_run/METINFOS.nml
fnlanduse=${dom_dir}/${dom}/LANDUSE_${dom}


####
# MEGAN Emission Factors and LAI
fnmgnef=${dom_dir}/${dom}/EFMAP_LAI_${dom}.nc
#checkexist ${fnmgnef}

#fnemis_trc=${chemprepfic}/TRACER_PROFILE
#fntrc=${chemprepfic}/TRACER
#if [ ${trc} -ge 1 ] ;  then
#   checkexist ${fnemis_trc}
#   checkexist ${fntrc}
#fi
 if [ $opt1 == "bemi" ] ; then

# It constructs in the temporary directory, a file named    #
# BEMISSIONS.nc containing biogenic emissions               #
#############################################################

echo -e "\033[34m o Biogenic emissions diagnostic model \033[0m"



ln -s ${proj_dir}/meteo/METINFOS-cosmo.nml.sed  .

echo $PWD

rm -f ${fnmetinfos}
sed -e s/_START_/${sd0}/      \
    -e s/_SHOW_/${show}/      \
    -e s/_UV10M_/${uv10m}/     \
    -e s/_USTA_/${ustar}/     \
    -e s/_FLUX_/${flux}/     \
    -e s/_HMIX_/${hmix}/     \
    -e s/_CLICE_/${clice}/     \
    -e s/_RAINWAT_/${rainwat}/     \
    -e s/_HMIOPT1_/${cldmix}/     \
    -e s/_HMIOPT2_/${kzexp}/     \
    -e s/_HMIOPT3_/${zimax}/     \
    -e s/_URBOPT1_/${upblmin}/     \
    -e s/_URBOPT2_/${uflxadd}/     \
    -e s/_URBOPT3_/${uwinfac}/     \
    -e s/_URBOPT4_/${upblcor}/     \
    -e s/_OPT1_/${uframin}/     \
    -e s/_OPT2_/${ukzmin}/     \
    -e s/_OPT3_/${umomin}/     \
    -e s/_EROS_/${eros}/ \
    -e s/_RESU_/${resu}/ \
    -e s/_CLICE_/${cloudw}/ \
    METINFOS-cosmo.nml.sed > ${fnmetinfos}
#!esac



if [ "$emis" = "airp" ] ; then
   echo "   Air Parif emissions, BEMISSIONS are already done"
   ln -s ${emissdir}/BEMISSIONS_${dom}.nc ${fnemisb}
else
ln -sf  ${proj_root}/${proj}/src/diagbio.e .

# Build args namelist for diagbio
echo "&args"                           >  diagbio.nml
echo "nhourrun    =   ${nhours}"       >> diagbio.nml
echo "fniMETEO    = '"${fnmeteo}"'"    >> diagbio.nml
echo "fniLDU      = '"$fnlanduse"'"    >> diagbio.nml
echo "fniMETINFOS = '"${fnmetinfos}"'" >> diagbio.nml
echo "fnoBIO      = '"${fnemisb}"'"    >> diagbio.nml
echo "fniMGNEF    = '"${fnmgnef}"'"    >> diagbio.nml
echo "/"                               >> diagbio.nml

rm -f ${fnemisb}
./diagbio.e

fi

#$NCDUMP -h ${fnemisb} 2>/dev/null >/dev/null || \
#    { echo "diagbio failure. Exiting" ; exit 1; }
$NCDUMP -h ${fnemisb} 2>/dev/null >/dev/null || \
    { echo "diagbio failure. Exiting" ;}
 echo " diabio ok"
echo $fnemisb
echo $bemi_dir_ispra
 else
   echo "    BEMISSIONS are already done"
fi
#####
 if [ $opt2 == "antroemi" ] ; then

##############################
# Hourly anthropic emissions #
##############################

case ${emifmt} in
        emep)

  if [ ${aero} -eq 1 -o ${reactive} -gt 0 -o ${carb} -eq 1 -o ${trc} -ge 1 ] ; then
  echo "----------------------------------------"
echo -e "\033[34m o EMEP anthropogenic emissions \033[0m"
#ln -sf  ${proj_root}/${proj}/modello/src/prepemis_emep.e .

rm -f ${fnemisa}
# Build args namelist for prepemis_emep
        echo "&args"                        >  prepemis_emep.nml
        echo "ids      = ${sd0}"            >> prepemis_emep.nml
        echo "nhourrun = ${nhours}"         >> prepemis_emep.nml
        echo "fno      = '"${fnemisa}"' "   >> prepemis_emep.nml
        echo "fnemis    = '"${fnemis_trc}"'"     >> prepemis_emep.nml
        echo "fntrc    = '"${fntrc}"'"      >> prepemis_emep.nml
        echo "fnmeteo  = '"${fnmeteo}"' "   >> prepemis_emep.nml
        echo "fnanthro  = '"${fnanthro}"'"  >> prepemis_emep.nml
        echo "indir = '"${emissdir}"'"   >> prepemis.nml
        echo "surface_emissions = ${surface_emissions}" >> prepemis.nml
        echo "point_emissions = ${point_emissions}" >> prepemis.nml
        echo "fire_emissions = ${fire_emissions}" >> prepemis.nml
        echo "/"                            >> prepemis_emep.nml

        ./prepemis_emep.e

        $NCDUMP -h ${fnemisa} 2>/dev/null >/dev/null || \
            { echo "prepemis_emep.e failure. Exiting"; exit 1; }
  fi
        ;;

        ctn)
  if [ ${aero} -eq 1 -o ${reactive} -gt 0 -o ${carb} -eq 1 -o ${trc} -ge 1 ] ; then
  echo "----------------------------------------"
echo -e "\033[34m o CTN anthropogenic emissions \033[0m"
ln -sf  ${proj_root}/${proj}/src/prepemis.e .
 surface_emissions=1
 point_emissions=1
 fire_emissions=0
 emissdirs=/autofs/scratch2/mstortini/chimere2008/EMISSIONS/$emissr/
 emissdirp=/autofs/scratch2/mstortini/chimere2008/EMISSIONS/$emissp/
 echo "emissdirs "$emissdirs
 echo "emissdirp "$emissdirp

rm -f ${fnemisa}
# Build args namelist for prepemis_ctn
        echo "&args"                        >  prepemis.nml
        echo "ids      = ${sd0}"            >> prepemis.nml
        echo "nhourrun = ${nhours}"         >> prepemis.nml
        echo "fno      = '"${fnemisa}"' "   >> prepemis.nml
        echo "fnmeteo  = '"${fnmeteo}"' "   >> prepemis.nml
        echo "fnemis      = '"${fnemis_trc}"'"     >> prepemis.nml
        echo "fnanthro = '"${fnanthro}"'"  >> prepemis.nml
        echo "fntrc    = '"${fntrc}"'"      >> prepemis.nml
#   if [ ${plmrise} -eq 2 ] ; then
#        echo "fnstack  = '"${fnstack}"'"  >> prepemis_ctn.nml
#        echo "fntheta  = '"${fntheta}"'"  >> prepemis_ctn.nml
#   fi
        echo "indirs = '"${emissdirs}"'"   >> prepemis.nml
        echo "indirp = '"${emissdirp}"'"   >> prepemis.nml
        echo "surface_emissions = ${surface_emissions}" >> prepemis.nml
        echo "point_emissions = ${point_emissions}" >> prepemis.nml
        echo "fire_emissions = ${fire_emissions}" >> prepemis.nml

        echo "/"                            >> prepemis.nml

        ./prepemis.e

#        $NCDUMP -h ${fnemisa} 2>/dev/null >/dev/null || \
#            { echo "prepemis.e failure. Exiting"; exit 1; }

        $NCDUMP -h ${fnemisa} 2>/dev/null >/dev/null || \
            { echo "prepemis.e failure. Exiting";}
          if [  -s $fnerror  ] ; then
            { echo "emissioni <0  Exiting"; exit 1; }
          fi
  fi
        ;;
        *)


# For experimented users : You have to implement here your own interface prepemis_<your_interface>.f90
# for Anthropic species , Fires, Point sources, Tracers ....
           echo "*** The interface to prepemis_${emis}  must be built" ; iexit 1
esac


# 2.3) Archivio il file con le emissioni; se necessario ri-zippo l'inventario
    echo  "emissi"  ${tmp_run}/AEMISSIONS.nc  
    echo  " to "   ${emi_dir_ispra}
#    scp ${tmp_run}/AEMISSIONS.nc  ma_aria@maialinux:/${emi_dir_ispra}/AEMISSIONS.${proj}.${datac}.nc
#    rsync -v  ${tmp_run}/AEMISSIONS.nc  ma_aria@maialinux:/${emi_dir_ispra}/AEMISSIONS.${proj}.${datac}.nc
#     gzip  -v -f   ${tmp_run}/AEMISSIONS.nc
     rsync -v  ${tmp_run}/AEMISSIONS.nc  ${emi_dir_ispra}/AEMISSIONS.${proj}.${datac}.nc

    gzip -f ${emi_dir_ispra}/AEMISSIONS.${proj}.${datac}.nc
#    gzip -f ${meteo_dir}/METEO.${proj}.${datac}.nc
#  if [ $cnt -eq $ndays -o $ddp1 -eq 1 ] ; then
#    echo "Ricompatto l'inventario"
#    gzip -v ${eminv_dir}/EMISSIONS*${dom}.${mm}
#  fi
   else
   echo "non devo fare le emissioni antropogeniche"
    fi
#   gzip  ${meteo_dir_ispra}/METEO.${proj}.${datac}.nc
#   rm ${meteo_dir_ispra}/METEO.${proj}.${datac}.nc
done
echo "Elaborazioni terminate, "`date`
