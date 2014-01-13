#/bin/bash
unset LANG 
#-------------------------------------------------------------------------------
# crea_progetto_chimere.sh
# Procedura per creare un nuovo progetto per un run chimere
#
# Uso: crea_progetto_chimere.sh [-h]
#
# NOTE:
# Nel costruire la catena, ho seguito la logica di mettere:
# - i files relativi ai domini (griglia orizzontale, livelli verticali, campi 
#   fisiografici) in $domains_dir
# - tutti gli altri files di controllo nella home del progetto
# - tutti i dati di input nelle sottodir. della home di esecuzione (area scratch)
#
#                     Versione 7.0.1 (chimere2008b), Michele & Enrico 24/01/2013
#-------------------------------------------------------------------------------
#set -x

#===============================================================================
# 1) Parametri costanti e default

# Path
chimere_root=$HOME_STORTINI/chimere              # root dell'albero chimere
scratch_root=/autofs/scratch2/mstortini/chimere  # root dell'albero scratch
proj_root=$HOME_STORTINI/chimere/progetti        # root delle home dei progetti
dat_dir=$HOME_STORTINI/chimere/dat               # files statici SIM

# Default
dvers="chimere2008bsim"
dgmeca="2"
dbins="8"
daero="1"
dsalt="1"
ddust="1"
dpops="0"
ddustout="1"
dcarb="1"
dsoatyp="2"
dtrc="0"
ddom="CTNBP1"
dvcoord="8_995_500"

#===============================================================================
# 2) Input interattivo

if [ $# -gt 0 ] ; then
if [ $1 = "-h" ] ; then
  echo "Uso: crea_progetto_chimere.sh"
  exit
fi
fi

#-------------------------------------------------------------------------------
# 2.1) Progetto

echo "Nome del progetto"
read proj

#-------------------------------------------------------------------------------
# 2.2) Versione chimere

cd $chimere_root
echo "*** Versione della release Chimere da utilizzare"
echo "disponibili: "`ls -d chimere*`
echo "default: "$dvers
echo "Per versioni antecedenti il 2007, usare le proc. della dir. chimere/bin"
echo "Per versioni antecedenti il 2005, usare le proc. della dir. chimere/bin2004"
read dum
echo $dum
if [ -z $dum ] ; then
  vers=$dvers
else
  vers=$dum
fi
if [ ! -d ${chimere_root}/ ] ; then
  echo "Errore, versione non disponibile: "$dum
  exit
fi
domains_dir=$chimere_root/$vers/domains                # files fisiografici

#-------------------------------------------------------------------------------
# 2.3) Meccanismo chimico gas
echo ""
echo "*** Meccanismo chimico da utilizzare per la fase gas"
echo "disponibili: MELCHIOR1 (completo), MELCHIOR2 (ridotto)"
echo "default: "$dgmeca
read dum

if [ -z $dum ] ; then
  mecachim=$dgmeca
else
  mecachim=$dum
fi

if [ $mecachim -ne 1 -a $mecachim -ne 2 ] ; then
  echo "Errore, meccanismo non gestito: "$mecachim
  exit
fi

#-------------------------------------------------------------------------------
# 2.4) Meccanismo chimico aerosol
echo ""
echo "*** Meccanismo chimico da utilizzare per la fase aerosol"
echo "disponibili: 0 (none), 1 (aero)"
echo "default: "$daero
read dum

if [ -z $dum ] ; then
  aero=$daero
else
  aero=$dum
fi

echo "disponibili: nbins"
echo "default: "$dbins
read dum
echo 'bin '$dum
if [ -z $dum ] ; then
  nbins=$dbins
else
  nbins=$dum
fi


echo "disponibili: 0 (none), 1 (salt)"
echo "default: "$dsalt
read dum

if [ -z $dum ] ; then
  seasalt=$dsalt
else
  seasalt=$dum
fi


echo "disponibili: 0 (none), 1 (dust)"
echo "default: "$ddust
read dum

if [ -z $dum ] ; then
  dust=$ddust
else
  dust=$dum
fi

echo "disponibili: 0 (none), 1 (pops)"
echo "default: "$dpops
read dum

if [ -z $dum ] ; then
  pops=$dpops
else
  pops=$dum
fi

echo "disponibili: 0 (none), 1 (dustout)"
echo "default: "$ddustout
read dum

if [ -z $dum ] ; then
  dustout=$ddustout
else
  dustout=$dum
fi

echo "disponibili: 0 (none), 1 (carb)"
echo "default: "$dcarb
read dum

if [ -z $dum ] ; then
  carb=$dcarb
else
  carb=$dum
fi

echo "*** Meccanismo chimico da utilizzare per secondari organici"


echo "disponibili: 0 (none), 1 (schema semplice ) , 2 (schema medio)"
echo "default: "$dsoatyp
read dum

if [ -z $dum ] ; then
  soatyp=$dsoatyp
else
  soatyp=$dum
fi


echo '"********numero di traccianti per sorgenti puntuali*'
echo "disponibili: 0 (none), 1 (trc)"
echo "default: "$dtrc
read dum

if [ -z $dum ] ; then
  trc=$dtrc
else
  trc=$dum
fi

#-------------------------------------------------------------------------------
# 2.5) Griglia
echo ""
echo $domains_dir
echo "*** Griglia orizzontale"
echo "disponibili: "`ls ${domains_dir}/HCOORD/COORD_* | cut -d _ -f 2`
echo "default: "$ddom
#echo 'griglia '$dum
read dum

if [ -z $dum ] ; then
  dom=$ddom
else
  dom=$dum
fi

echo ""
echo "*** Livelli verticali"
echo "disponibili: "`cd  ${domains_dir}/VCOORD/; ls VCOORD_* | cut -c 8-`
#echo "disponibili: "`ls ${domains_dir}/VCOORD/VCOORD_* | cut -d _ _ _  -f 2`
#echo "disponibili: "`ls ${domains_dir}/VCOORD/VCOORD_* `
echo "default: "$dvcoord
read dum

if [ -z $dum ] ; then
  vcoord=$dvcoord
else
  vcoord=$dum
fi

#-------------------------------------------------------------------------------
# 2.6) Permessi di scrittura
echo ""
echo "*** Si vuole che l'area scratch sia scrivibile dal gruppo ambiente (Y/N)"
read dum
if [ -z $dum ] ; then
  wamb="Y"
else
  if [ $dum = "N" -o $dum = "n" ] ; then
    wamb="N"
  else
    wamb="Y"
  fi
fi

echo "Path di lavoro:"
echo "chmere_root         "$chimere_root
echo "scratch_root        "$scratch_root
echo "domains_dir         "$domains_dir  
echo "progetto salvato in "$proj_root/$proj

#===============================================================================
# 3) Operazioni automatiche

#set -x

# 3.1) Creo la home del progetto
cd $proj_root
if [ -d $proj ] ; then
  echo "Il progetto "${proj}" esiste gia: "
  echo "(M)odifico, (R)icopro, (T)ermino"
  echo "Con opzione (M) i files gia' esistenti non verranno sovrascritti"
  read dum
  if [ -z $dum ] ; then
    dum="m"
  fi 
  if [ $dum = "t" -o $dum = "T" ] ; then
    exit
  elif [ $dum = "r" -o $dum = "R" ] ; then
    ovw="Y"
    rm -fR ${proj}/*
  elif [ $dum = "m" -o $dum = "M" ] ; then
    ovw="N"
  else
    exit
  fi
else
  mkdir $proj
  ovw="Y"
fi
cd $proj

# CHEMISTRY PRE-PROCESSING
bins=0
reactive=0
if [ ${aero} = 1 -o ${dustout} = 1 -o ${seasalt} = 1 -o ${carb} = 1 -o ${dust} = 1 ] ;  then
    bins=1
fi
if [ ${bins} = 0 ] ; then
  nbins=0
fi
if [ ${mecachim} -gt 0 ] ; then
reactive=1
fi

labchem=${mecachim}${aero}${seasalt}${pops}${dustout}${carb}${trc}${soatyp}${dust}.${nbins}
chemprepdir=${chimere_root}/$vers/chemprep
export chemprepfic=${chemprepdir}/inputdata.${labchem}
export DATADIR=${chemprepfic}

#---------------------
# calendar management
#month=`date -u -d ${di} +%m`    # Month of simulation start
#
#sim=${di}_${de}_${lab}       # Simulation label: 
                             # output files will be named out.<sim>

#psim=${dib}_${deb}_${lab}    # Simulation label of previous run (if continuation run)

# Flag for boundary conditions
ibound=1
if [ ${reactive} -eq 0 -a ${bins} -eq 0 ] ; then
  ibound=0
fi

# Options only valid for the coarse domain
if [ ${reactive} -eq 0 ] ; then
   boundgas=NONE
fi
if [ ${bins} -eq 0 ] ; then
   boundaer=NONE
fi


if [ ${seasalt} = 2 ] ;  then
    nequil=1
    echo "+++ Because of active sea salts, _nequil_ is forced to 1 : use of ISORROPIA on line"
fi

# model version
version=`basename ${chimere_root}`


#########################################
# Building files and directories names  #
#########################################


### Chemistry
# fnoutspec  : File containing the name of output species
# fnspec     : File of names of all active species
# fnchem     : File containing preprocessed reactions
# fnstoi     : File containing stoichiometric coefficients
# fnrates    : File containing tables of reaction rates
# fnfamilies : Definition of families
# fnphot     : Zenith angles tabulated




if [ ! -d src ] ; then
  mkdir src
fi
if [ ! -d chemprep ] ; then
  mkdir chemprep
  mkdir -p chemprep/inputdata.$labchem   
fi
if [ ! -d domains ] ; then
  mkdir domains
  mkdir  domains/HCOORD
  mkdir  domains/VCOORD
  mkdir  domains/$dom
fi

if [ ! -d meteo ] ; then
  mkdir meteo
fi
#-------------------------------------------------------------------------------
# 3.2) Copio i files 

# 3.2.1 Sorgenti chimere
indir=${chimere_root}/${vers}
cd $indir
echo "root dir chimere   "$indir
#cp  ${indir}/Mak* ${indir}/mak*  ${proj_root}/${proj}/.  
cp ${indir}/Mak* ${proj_root}/${proj}/.  
cp -r ${indir}/src/* ${proj_root}/${proj}/src/.  
cp  ${indir}/meteo/METINFOS-cosmo.nml.sed   ${proj_root}/${proj}/meteo/.  
#
cd ${proj_root}/${proj}
cp ${dat_dir}/pre_chimere.inp.template ./pre_chimere.inp

############################
if [ ! -d ${chemprepfic} ] ; then
echo "ATTENZIONE"
   echo "--> Bisogna creare i file della chimica."
   echo "--> Bisogna lanciare make-chemgeom.sh."
else
#ln -s -t ${proj_root}/$proj/{chemprepfic}  ${chemprepfic}  
cp -r   ${chemprepfic}/.  ${proj_root}/${proj}/chemprep/inputdata.$labchem/.   
cp ${chimere_root}/$vers/chemprep/boundaries_spec/*2008  ${proj_root}/${proj}/chemprep/inputdata.$labchem/.   
cp ${chimere_root}/$vers/chemprep/boundaries_spec/*2005  ${proj_root}/${proj}/chemprep/inputdata.$labchem/.   

fi

if [ ! -e ${domains_dir}/HCOORD/COORD_${dom}  ] ; then
echo "ATTENZIONE"
    echo " -->  Bisogna creare i file  fisiografici"
   echo "--> E' necessario modificare pre_chimere.inp e  lanciare make-chemgeom_v2008.sh."
else
#ln -s -t  ${proj_dir}/fis  ${domains_dir}/HCOORD/COORD_${dom}  
 cp  ${domains_dir}/HCOORD/COORD_${dom}   ${proj_root}/${proj}/domains/HCOORD/.
 cp -r  ${domains_dir}/${dom}/*    ${proj_root}/${proj}/domains/$dom/.

fi

if [ ! -e ${domains_dir}/VCOORD/VCOORD_${dvcoord}  ] ; then
echo "ATTENZIONE"
    echo " -->  Bisogna creare i file  coordinate  verticali"
   echo "-->  E' necessario modificare pre_chimere.inp e  lanciare make-chemgeom_v2008.sh."
fi
cp   ${domains_dir}/VCOORD/VCOORD_${dvcoord}    ${proj_root}/${proj}/domains/VCOORD/.
#domains/VCOORD/VCOORD_.

#

# 3.3) Creo le dir. del progetto nell'area scratch (se non esistono)
cd $scratch_root/
if [ ! -d $proj ] ; then
  mkdir $proj
fi
cd $proj
#
for subdir in bc bin emi meteo out run log tmp ; do
  if [ ! -d $subdir ] ; then
    mkdir $subdir
  fi
done
#
#cd tmp
#if [ ! -f gribba_io.ksh -o $ovw = "Y" ] ; then
#  cp $chimere_root/bin/gribba_io.ksh ./
#fi
#
#for file in `ls $proj_root/$proj/CHIMERE_INFO.DAT*` ; do
#  cp $file ./
#done

#-------------------------------------------------------------------------------
# 3.4) Apro le dir. scratch al gruppo ambiente
if [ $wamb = "Y" ] ; then
  cd $scratch_root
  chgrp -R ambiente $proj 
  chmod g+s $proj/*
fi

#===============================================================================
# 4) Lista delle operazioni manuali:

clear

#     123456789012345678901234567890123456789012345678901234567890123456789012345
echo "==========================================================================="
echo "                     Operazioni da compiere a mano"
echo ""
echo "1) modificare pre_chimere.inp"
echo ""
echo "2) Compilare i programmi (compile_chimere_chain.sh)"
echo ""
echo "3) Creare gli input (lanciare: crea_input_emi_v2008.sh crea_input_meteo_v2008.sh,"
echo "   crea_input_bc_v2008.sh"
echo ""
echo "4) lanciare le simulazioni (run_chimere.sh) da sistemare"
echo ""


#     123456789012345678901234567890123456789012345678901234567890123456789012345

exit
