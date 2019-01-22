#!/bin/bash
unset LANG
#-------------------------------------------------------------------------------
# compile_chimere_chain.ksh
# Script per compilare tutti i progammi della catena chimere per un progetto.
#
# Uso: run_chimere.ksh PROGETTO [-h]
#
# Note:
# Siccome chimere e i programmi che creano gli input vengono compilati con
# chimere.h che dipende dalla griglia, devono essere compilati per ogni
# progetto.
# Questo script deve essere lanciato prima di creare gli input, e mette gli
# eseguibili nell'area scratch del progetto, subdir. bin
#
#                                   Versione 7.0.0, Enrico & Michele 29/11/2012
#-------------------------------------------------------------------------------

function checkexist {
  [ -s $1 ] || { echo "non existent $1 ; exiting. " ; exit 1;   }
}

################################################################################
# 1) Elaborazioni preliminari

#-------------------------------------------------------------------------------
# 1.1) Definisco le variabili d'ambiente relative a questo run Chimere 
#      (dipendono  pre_chimere.inp e dalle dir di installazione)

if [ $HOSTNAME = "fileserver" ] ; then          # maialinux fed16
  chimere_env=$HOME/ver16/ope/ninfa/bin/chimere_env.sh
else                                            # lattuga e PCs
  chimere_env=~eminguzzi/chimere/bin/chimere_env.sh
fi
. $chimere_env $proj

# Alias relativi ai path
export EXEDIR=$exe_dir
export SRCDIR=$src_dir
chimere_tmp=$proj_dir

# Nomi dei files principali
chemfic=${chem_dir}/inputdata.${labchem}
fncoord=${dom_dir}/HCOORD/COORD_${dom}
vcoord=${dom_dir}/VCOORD/VCOORD_${nlayer_chimere}_${first_layer_pressure}_${top_chimere_pressure}
fnspec=${chemfic}/ACTIVE_SPECIES
fnchem=${chemfic}/CHEMISTRY
fnfamilies=${chemfic}/FAMILIES
fnanthro=${chemfic}/ANTHROPIC
fnbiogen=${chemfic}/BIOGENIC

# Parametri relativi al pre-processing chimico
if [ ${aero} = 1 -o ${dustout} = 1 -o ${seasalt} = 1 -o ${carb} = 1 -o ${dust} = 1 ] ;  then
  bins=1
  fnaerosol=${chemfic}/AEROSOL
else
  bins=0
  nbins=0
  boundaer=NONE      # Only for coarse domanin (???)
fi

if [ ${mecachim} -gt 0 ] ; then
  reactive=1
else
  reactive=0
  boundgas=NONE      # Only for coarse domanin (???)
fi

if [ ${reactive} -eq 0 -a ${bins} -eq 0 ] ; then
  ibound=0
else
  ibound=1
fi

if [ ${seasalt} = 2 ] ;  then
  nequil=1
  echo "+++ Because of active sea salts, _nequil_ is forced to 1 : use of ISORROPIA on line"
fi

#-------------------------------------------------------------------------------
# 1.2) controlli

# Esistenza dei files fondamentali
checkexist ${fncoord}
checkexist ${vcoord}
checkexist ${fnspec}
checkexist ${fnchem}
checkexist ${fnfamilies}
checkexist ${fnaerosol}
checkexist ${fnbiogen}
if [ ${aero} -eq 1 -o ${reactive} -gt 0 -o ${carb} -eq 1 -o ${trc} -ge 1 ] ; then
   checkexist ${fnanthro}
fi

# Check correctness of subdomains definition
[ ${nzdoms} -eq 0 ]&& { echo "Bad nzdoms setting ! "; echo "Bye ..."; exit 1; }
[ ${nmdoms} -eq 0 ]&& { echo "Bad nmdoms setting ! "; echo "Bye ..."; exit 1; }

# Copio i files relativi allo schema chimico richiesto
if [ ! -d $chemfic ] ; then
   if [ -d  ${chimere_home}/chemprep/inputdata.${labchem} ] ; then
     echo "Copio i files relativi allo schema chimico richiesto in "$chem_dir
     mkdir -p $chemfic
     cp -r ${chimere_home}/chemprep/inputdata.${labchem}/.  $chemfic/.
  fi
fi     

# Log a schermo
echo ""
echo "Opzioni principali reltivie alla configurazione Chimere:"
echo "  mecachim "${mecachim}
echo "  aero     "${aero}
echo "  seasalt  "${seasalt}
echo "  pops     "${pops}
echo "  dustout  "${dustout}
echo "  carb     "${carb}
echo "  trc      "${trc}
echo "  soatyp   "${soatyp}
echo "  dust     "${dust}
echo "  nbins    "${nbins} 
echo ""
echo "Path:"
echo "  install. chimere "${chimere_home}
echo "  sorgenti chimere "${src_dir}
echo "  dir di lavoro    "${chimere_tmp}/src
echo "  dom_dir          "${dom_dir}
echo "  fncoord          "${fncoord}
echo "  vcoord           "${vcoord}
echo ""

################################################################################
# 2) Compilazione

cd ${chimere_tmp}/src

cp ${chimere_tmp}/Makefile.hdr .

#######################################
# Definition of simulation parameters #
#######################################
sd0=${year}${month}${day}${hour}
nzonal=`awk 'BEGIN{stop=0}{x[FNR]=$1}END{for(k=2;k<=FNR;k++) {if(x[k-1]>x[k]&&stop==0){print k-1;stop=1}}}' ${fncoord}`
ntotal=`wc -l  ${fncoord} | gawk '{print $1}' `
nmerid=`expr ${ntotal} /  ${nzonal}`
show=`expr ${nzonal} \* ${nmerid}`
show=`expr ${show} / 2`
show=`expr ${show} + ${nzonal}`
echo "o simulation over horizontal domain: " ${dom}
echo "  "${dom}" >> "${nzonal}"x"${nmerid}" cells, total "${ntotal}

[ -z $nzonal ]&& \
    { echo "${myself}: No such domain ${dom}. Exiting."; exit 1; }

nlayers=`cat ${vcoord}     |grep -v "^ *$"|wc -l | $AWK '{print $1}'`
nspec=`  cat ${fnspec}     |grep -v "^ *$"|wc -l | $AWK '{print $1}'`
nreac=`  cat ${fnchem}     |grep -v "^ *$"|wc -l | $AWK '{print $1}'`
nfam=`   cat ${fnfamilies} |grep -v "^ *$"|wc -l | $AWK '{print $1}'`
nemisa=` cat ${fnanthro}   |grep -v "^ *$"|wc -l | $AWK '{print $1}'`
nemisb=` cat ${fnbiogen}   |grep -v "^ *$"|wc -l | $AWK '{print $1}'`

if [ ${bins} -eq 1  ] ; then
  ms=`$AWK '{if(FNR==2) print $1}' ${fnaerosol}`
  ms1=`expr $ms + 1`
  d1=`$AWK '{if(FNR==3) print $1}' ${fnaerosol}`
  d2=`$AWK '{if(FNR==3) print $'${ms1}'}' ${fnaerosol}`
  nl=`cat ${fnaerosol} | grep -v "^ *$"|wc -l | $AWK '{print $1}'`
  nk=`expr $nl - 5`
else
  ms=1
  d1=1.d-3
  d2=1.d+3
  nk=1
fi
[ $levout -gt $nlayers ]&& \
    { echo "***ERROR, levout=$levout > chimere_layers=$nlayers"; exit 1; }
[ $levout -lt $nlayers ]&& \
    { echo "+++WARNING, this simulation can not be used for a nest run :  levout=$levout < chimere_layers=$nlayers"; }

############################################
# Construction of chimere_params.F90 files #
############################################

mkdir -p model ||  { echo "${myself}: Cannot create subdirectory model"; exit 1; }

cat << EOD > chimere.h.sed-commands
s,_DOM_,${dom},
s,_NVE_,${nlayers},
s,_NIO_,${levout},
s,_SHO_,${show},
s,_NZD_,${nzdoms},
s,_NMD_,${nmdoms},
s,_NPH_,${phys},
s,_NST_,${step},
s,_NGS_,${ngs},
s,_NSU_,${nsu},
s,_IRS_,${irs},
s,_NHR_,${nhours},
s,_NSP_,${nspec},
s,_NRE_,${nreac},
s,_NFA_,${nfam},
s,_NEA_,${nemisa},
s,_NEB_,${nemisb},
s,_NLE_,${nlayers},
s,_MSE_,${ms},
s,_NKC_,${nk},
s,_DP1_,${d1},
s,_DP2_,${d2},
s,_NEQ_,${nequil},
s,_PEQ_,${npeq},
s,_SAC_,${nsconcs},
s,_SAD_,${nsdepos},
s,_AERO_,${aero},
s,_SEASALT_,${seasalt},
s,_POPS_,${pops},
s,_DUSTOUT_,${dustout},
s,_DUST_,${dust},
s,_CARB_,${carb},
s,_TRC_,${trc},
s,_PLMRISE_,${plmrise},
s,_SOATYP_,${soatyp},
s,_BINS_,${bins},
s,_REACTIVE_,${reactive},
s,_IBOUND_,${ibound},
s,_IDEEPCONV_,${ideepconv},
EOD
# chimere_params for the master process
sed -f ${chimere_tmp}/src/chimere.h.sed-commands ${chimere_tmp}/src/chimere_params.F90.sed > modules/chimere_params.F90.tmp
[ -s modules/chimere_params.F90.tmp ]|| \
    { echo "${myself}: Cannot create chimere_params.F90.tmp"; exit 1; }

# worker_params for worker processes
sed -f ${chimere_tmp}/src/chimere.h.sed-commands ${chimere_tmp}/src/worker_params.F90.sed > model/worker_params.F90.tmp
    [ -s model/worker_params.F90.tmp ]|| \
	{ echo "${myself}: Cannot create model/worker_params.F90.tmp"; exit 1; }

# calculating nzonal and nmerid for each shape
nzonal_o=$(($nzonal/$nzdoms))
nzonal_r=$(( $nzonal - ${nzonal_o} * $(( $nzdoms-1 )) ))
nmerid_o=$(($nmerid/$nmdoms))
nmerid_u=$(( $nmerid - ${nmerid_o} * $(( $nmdoms-1 )) ))

echo "o Parallel run configuration:"
echo "  Worker  = " ${nzonal_o}
echo "  Worker  = " ${nzonal_r}
# compute the max of nzonal_*
nzonalmax=${nzonal_o}
[ ${nzonal_r} -gt ${nzonalmax} ] &&  nzonalmax=${nzonal_r}
echo "  Worker nzonalmax = " ${nzonalmax}


echo "o Parallel run configuration:"
echo "  Worker  = " ${nmerid_o}
echo "  Worker = " ${nmerid_u}

# compute the max of nmerid_*
nmeridmax=${nmerid_o}
[ ${nmerid_u} -gt ${nmeridmax} ] &&  nmeridmax=${nmerid_u}
echo "  Worker nmeridmax = " ${nmeridmax}

echo "o Parallel run configuration finaler:"
echo "  Worker nzonalmax = " ${nzonalmax}
echo "  Worker nmeridmax = " ${nmeridmax}

# passing nzonal and nmerid to chimere_params and worker_params
sed -e s,_NZO_,${nzonal}, \
    -e s,_NME_,${nmerid}, \
    modules/chimere_params.F90.tmp > modules/chimere_params.F90
#rm -f chimere_params.F90.tmp

sed -e s,_NZOM_,${nzonalmax}, \
    -e s,_NMEM_,${nmeridmax}, \
    model/worker_params.F90.tmp > model/worker_params.F90
#rm -f model/worker_params.F90.tmp

###########################################################
# Compilation of Programs in the ${chimere_tmp} directory #
###########################################################
compiler=`grep ^REALFC ${chimere_tmp}/Makefile.hdr|sed s,REALFC,,|sed s,=,,|tr -s " "|tr -d "\t"`
echo "Compilation of all programs with ${compiler} ..."

${MAKE} clean >/dev/null 2>&1
${MAKE} > ${garbagedir}/make.log 2>&1
if [ $? -ne 0 ] ; then
    echo
    echo "CHIMERE compilation aborted"
    echo "Check file ${garbagedir}/make.log"
    echo
    exit 1
fi









