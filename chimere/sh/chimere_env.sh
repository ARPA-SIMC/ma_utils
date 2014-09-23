#-------------------------------------------------------------------------------
# chimere_env.sh
# Script che legge pre_chimere.inp e assegna i path e le altre varibili 
# d'ambiente per l'esecuzione di un progetto Chimere.
# 
# Uso: chimere_env.ksh PROGETTO
#
# Note:
# Chiamato da tutti gli script della catena Chimere-SIM
#
# TODO:
# Aggiornamento di tutti i path delle utility simc, elimiando le variabili:
# - util_dir, grib_util, lm_util, split_out: introdurre in tutti gli script la 
#   gestione di /usr/libexec/ma_utils o MA_UTILS_SVN
# - lm_lsm_dir: mettere in svn le LSM COSMO
# - dat_dir:  sostituire con varibile chimere_inp_template (verificare che 
#   venga usata solo per riferirsi a pre_chimere.inp.template)
# - date_util, days, delta_days, cong: verificare che nessuna procedura le usi 
#   piu' ed eventualmente sostituire con utility piu' recenti
# - valutare se mettere negli script che lavorano sui grib un comando per 
#   scaricare il file degli alias (arki-scan --aliases)
#
#                         Versione 7.0.0 (V200709C), Michele & Enrico 03/10/2012
#-------------------------------------------------------------------------------
#set -x
unset LANG

#-------------------------------------------------------------------------------
# 1) Gestione parametro (nome progetto)
if [ $# -eq 0 ] ; then
  echo "Uso: chimere_env.sh progetto "
  exit 1
elif [ $1 = "-h" ] ; then
  echo "Uso: chimere_env.sh progetto  "
  exit 1
else
  proj=$1
fi

#-------------------------------------------------------------------------------
# 2) Variabili d'ambiente indipendenti da pre_chimere.inp

if [ $HOSTNAME = "maialinux" ] ; then             # maialinux fed16
  chimere_root=$HOME/ver16/chimere/               # root installazione chimere
  scratch_root=/scratch/ma_aria/chimere/          # root albero scratch
  lm_lsm_dir=/lhome/ma_aria/grib/lm_ope           # files lsm COSMO

else                                            # lattuga e PCs
  chimere_root=/home/eminguzzi/chimere/           # root installazione chimere
  scratch_root=/autofs/scratch2/mstortini/chimere # root albero scratch
  lm_lsm_dir=/home/eminguzzi/util/grib/lm_ope     # files lsm COSMO
  mozart2dir=$scratch_root/BIGFILES/MOZART2/      # dati Mozart
  gocartdir=$scratch_root/BIGFILES/GOCART/        # dati Gocart
  lmdzinca2dir=$scratch_root/BIGFILES/LMDZINCA/   # dati INCA
  eminv_root=$scratch_root/BIGFILES/EMI_2004/     # root inventari emissioni
  meteo_root=$chimere_root/meteo/                 # root archivi grib (files ECMWF)

fi

proj_root=/home/mstortini/chimere/progetti        # root home dei progetti
#proj_root=$chimere_root/progetti/                # root home dei progetti
dat_dir=$chimere_root/dat                         # files statici SIM
util_dir=$chimere_root/bin/                       # utility SIM per Chimere

# Programmi di utilita' non in path o con alias
MAKE="/usr/bin/make -j"
AWK=/bin/gawk
NCDUMP=/usr/bin/ncdump
split_out=${util_dir}/split_output.e

# Assegnazioni che saranno obsolete con l'introduzione del pacchetto metamb_util
if [ $HOSTNAME = "maialinux" ] ; then         # maialinux fed16
  grib_util=$HOME/ver16/grib/bin                  # vecchie utility per grib
  lm_util=$HOME/ver16/grib/bin                    # vecchie utility postproc COSMO
  date_util=$HOME/ver16/bin                       # vecchie utility per date
else                                          # lattuga e PCs
  grib_util=/home/eminguzzi/util/grib/bin         # vecchie utility per grib
  lm_util=/home/eminguzzi/post_lm/bin             # vecchie utility postproc COSMO
  date_util=/home/eminguzzi/bin                   # vecchie utility per date
fi
days=$date_util/days.exe
delta_days=$date_util/delta_days.exe
cong=/usr/bin/cong

#-------------------------------------------------------------------------------
# 3) Lettura di pre_chimere.inp
 
proj_dir=$proj_root/${proj}/                       # home del progetto
nml=${proj_dir}/pre_chimere.inp                    # namelist di controllo
if [ ! -f $nml ] ; then
  echo "chimere_env.sh: WARNING, file "$nml" non trovato"
  echo "Assegno solo le varibili indipendenti dal progetto Chimere"
fi

# Sezione 1: Run
proj_nml=`grep %PROJ%       $nml | cut -f 2 -d ! | sed 's/^ *//g;s/ *$//g'`
date_ini=`grep %DATE_INI%   $nml | cut -f 2 -d ! | sed 's/^ *//g;s/ *$//g'`
date_end=`grep %DATE_END%   $nml | cut -f 2 -d ! | sed 's/^ *//g;s/ *$//g'`
hh_ini=`grep %HH_INI%       $nml | cut -f 2 -d ! | sed 's/^ *//g;s/ *$//g'`
nhours=`grep %RUN_LEN%      $nml | cut -f 2 -d ! | sed 's/^ *//g;s/ *$//g'`
scad0=`grep %SCAD0%         $nml | cut -f 2 -d ! | sed 's/^ *//g;s/ *$//g'`
run_sync=`grep %RUN_SYNC%   $nml | cut -f 2 -d ! | sed 's/^ *//g;s/ *$//g'`
vers=`grep %VERS%           $nml | cut -f 2 -d ! | sed 's/^ *//g;s/ *$//g'`
nbins=`grep %NBINS%         $nml | cut -f 2 -d ! | sed 's/^ *//g;s/ *$//g'`
mecachim=`grep %MECACHIM%   $nml | cut -f 2 -d ! | sed 's/^ *//g;s/ *$//g'`
aero=`grep %AEROSOLS%       $nml | cut -f 2 -d ! | sed 's/^ *//g;s/ *$//g'`
seasalt=`grep %SALT%        $nml | cut -f 2 -d ! | sed 's/^ *//g;s/ *$//g'`
pops=`grep %POPS%           $nml | cut -f 2 -d ! | sed 's/^ *//g;s/ *$//g'`
dustout=`grep %DUSTOUT%     $nml | cut -f 2 -d ! | sed 's/^ *//g;s/ *$//g'`
carb=`grep %CARB%           $nml | cut -f 2 -d ! | sed 's/^ *//g;s/ *$//g'`
trc=`grep %TRC%             $nml | cut -f 2 -d ! | sed 's/^ *//g;s/ *$//g'`
dust=`grep %DUST%           $nml | cut -f 2 -d ! | sed 's/^ *//g;s/ *$//g'`
soatyp=`grep %SOATYP%       $nml | cut -f 2 -d ! | sed 's/^ *//g;s/ *$//g'`
dom=`grep %DOM%             $nml | cut -f 2 -d ! | sed 's/^ *//g;s/ *$//g'`
nlayer_chimere=`grep %NLAYER%          $nml | cut -f 2 -d ! | sed 's/^ *//g;s/ *$//g'`
first_layer_pressure=`grep %FLAYPRES%  $nml | cut -f 2 -d ! | sed 's/^ *//g;s/ *$//g'`
top_chimere_pressure=`grep %TLAYPRES%  $nml | cut -f 2 -d ! | sed 's/^ *//g;s/ *$//g'`
cdebug=`grep %CDEBUG%       $nml | cut -f 2 -d ! | sed 's/^ *//g;s/ *$//g'`

# Sezione 2: Output
nsconcs=`grep %NSCONCS%     $nml | cut -f 2 -d ! | sed 's/^ *//g;s/ *$//g'`
nsdepos=`grep %NSDEPOS%     $nml | cut -f 2 -d ! | sed 's/^ *//g;s/ *$//g'`
levout=`grep %LEVOUT%       $nml | cut -f 2 -d ! | sed 's/^ *//g;s/ *$//g'`
unfout=`grep %UNFOUT%       $nml | cut -f 2 -d ! | sed 's/^ *//g;s/ *$//g'`
grbout=`grep %GRBOUT%       $nml | cut -f 2 -d ! | sed 's/^ *//g;s/ *$//g'`
bcout=`grep %BCOUT%         $nml | cut -f 2 -d ! | sed 's/^ *//g;s/ *$//g'`
accur=`grep %ACCUR%         $nml | cut -f 2 -d ! | sed 's/^ *//g;s/ *$//g'`

# Sezione 3: Chimere
nzdoms=`grep %NZDOMS%       $nml | cut -f 2 -d ! | sed 's/^ *//g;s/ *$//g'`
nmdoms=`grep %NMDOMS%       $nml | cut -f 2 -d ! | sed 's/^ *//g;s/ *$//g'`
phys=`grep %PHYS%           $nml | cut -f 2 -d ! | sed 's/^ *//g;s/ *$//g'`
step=`grep %STEP%           $nml | cut -f 2 -d ! | sed 's/^ *//g;s/ *$//g'`
ngs=`grep %NGS%             $nml | cut -f 2 -d ! | sed 's/^ *//g;s/ *$//g'`
nsu=`grep %NSU%             $nml | cut -f 2 -d ! | sed 's/^ *//g;s/ *$//g'`
irs=`grep %IRS%             $nml | cut -f 2 -d ! | sed 's/^ *//g;s/ *$//g'`
nequil=`grep %NEQUIL%       $nml | cut -f 2 -d ! | sed 's/^ *//g;s/ *$//g'`
npeq=`grep %NPEQ%           $nml | cut -f 2 -d ! | sed 's/^ *//g;s/ *$//g'`

# Sezione 4: Emissioni
emis=`grep %EMINV%          $nml | cut -f 2 -d ! | sed 's/^ *//g;s/ *$//g'`
emifmt=`grep %EMIFMT%       $nml | cut -f 2 -d ! | sed 's/^ *//g;s/ *$//g'`
ivs=`grep %IVS%             $nml | cut -f 2 -d ! | sed 's/^ *//g;s/ *$//g'`
plmrise=`grep %PLMRISE%     $nml | cut -f 2 -d ! | sed 's/^ *//g;s/ *$//g'`
nlevemis=`grep %NLEVEMIS%   $nml | cut -f 2 -d ! | sed 's/^ *//g;s/ *$//g'`
flatreduc=`grep %FLATREDUC% $nml | cut -f 2 -d ! | sed 's/^ *//g;s/ *$//g'`
eros=`grep %EROS%           $nml | cut -f 2 -d ! | sed 's/^ *//g;s/ *$//g'`
resu=`grep %RESU%           $nml | cut -f 2 -d ! | sed 's/^ *//g;s/ *$//g'`
qsscale=`grep %QSSCALE%     $nml | cut -f 2 -d ! | sed 's/^ *//g;s/ *$//g'`

# Sezione 5: IC/BC
init=`grep %INIT%           $nml | cut -f 2 -d ! | sed 's/^ *//g;s/ *$//g'`
nested=`grep %NESTED%       $nml | cut -f 2 -d ! | sed 's/^ *//g;s/ *$//g'`
boundgas=`grep %BOUNDGAS%   $nml | cut -f 2 -d ! | sed 's/^ *//g;s/ *$//g'`
boundaer=`grep %BOUNDAER%   $nml | cut -f 2 -d ! | sed 's/^ *//g;s/ *$//g'`
cdom=`grep %CDOM%           $nml | cut -f 2 -d ! | sed 's/^ *//g;s/ *$//g'`
csim=`grep %CSIM%           $nml | cut -f 2 -d ! | sed 's/^ *//g;s/ *$//g'`
nvcoord=`grep %NVCOORD%     $nml | cut -f 2 -d ! | sed 's/^ *//g;s/ *$//g'`
cflaypres=`grep %CFLAYPRES% $nml | cut -f 2 -d ! | sed 's/^ *//g;s/ *$//g'`
ctlaypres=`grep %CTLAYPRES% $nml | cut -f 2 -d ! | sed 's/^ *//g;s/ *$//g'`

# Sezione 6: Meteorologia
metmod=`grep %METMOD%       $nml | cut -f 2 -d ! | sed 's/^ *//g;s/ *$//g'`
dataset=`grep %DATASET%     $nml | cut -f 2 -d ! | sed 's/^ *//g;s/ *$//g'`
metdom=`grep %METDOM%       $nml | cut -f 2 -d ! | sed 's/^ *//g;s/ *$//g'`
met_len=`grep %MET_LEN%     $nml | cut -f 2 -d ! | sed 's/^ *//g;s/ *$//g'`
ptopmet=`grep %PTOPMET%     $nml | cut -f 2 -d ! | sed 's/^ *//g;s/ *$//g'`
ideepconv=`grep %IDEEPCONV% $nml | cut -f 2 -d ! | sed 's/^ *//g;s/ *$//g'`
iadv=`grep %IADV%           $nml | cut -f 2 -d ! | sed 's/^ *//g;s/ *$//g'`

flux=`grep %FLUX%           $nml | cut -f 2 -d ! | sed 's/^ *//g;s/ *$//g'`
ustar=`grep %USTAR%         $nml | cut -f 2 -d ! | sed 's/^ *//g;s/ *$//g'`
hmix=`grep %HMIX%           $nml | cut -f 2 -d ! | sed 's/^ *//g;s/ *$//g'`
uv10m=`grep %UV10M%         $nml | cut -f 2 -d ! | sed 's/^ *//g;s/ *$//g'`

lcloud=`grep %LCLOUD%       $nml | cut -f 2 -d ! | sed 's/^ *//g;s/ *$//g'`
mcloud=`grep %MCLOUD%       $nml | cut -f 2 -d ! | sed 's/^ *//g;s/ *$//g'`
hcloud=`grep %HCLOUD%       $nml | cut -f 2 -d ! | sed 's/^ *//g;s/ *$//g'`
cloudw=`grep %CLOUDW%       $nml | cut -f 2 -d ! | sed 's/^ *//g;s/ *$//g'`
clice=`grep %CLICE%         $nml | cut -f 2 -d ! | sed 's/^ *//g;s/ *$//g'`

cldmix=`grep %CLDMIX%       $nml | cut -f 2 -d ! | sed 's/^ *//g;s/ *$//g'`
kzexp=`grep %KZEXP%         $nml | cut -f 2 -d ! | sed 's/^ *//g;s/ *$//g'`
zimax=`grep %ZIMAX%         $nml | cut -f 2 -d ! | sed 's/^ *//g;s/ *$//g'`

upblmin=`grep %UPBLMIN%     $nml | cut -f 2 -d ! | sed 's/^ *//g;s/ *$//g'`
uflxadd=`grep %UFLXADD%     $nml | cut -f 2 -d ! | sed 's/^ *//g;s/ *$//g'`
uwinfac=`grep %UWINFAC%     $nml | cut -f 2 -d ! | sed 's/^ *//g;s/ *$//g'`
upblcor=`grep %UPBLCOR%     $nml | cut -f 2 -d ! | sed 's/^ *//g;s/ *$//g'`
uframin=`grep %UFRAMIN%     $nml | cut -f 2 -d ! | sed 's/^ *//g;s/ *$//g'`
ukzmin=`grep %UKZMIN%       $nml | cut -f 2 -d ! | sed 's/^ *//g;s/ *$//g'`
umomin=`grep %UMOMIN%       $nml | cut -f 2 -d ! | sed 's/^ *//g;s/ *$//g'`

#-------------------------------------------------------------------------------
# 4) Variabili d'ambiente dipendenti dal contenuto di pre_chimere.inp

# 4.1 Controlli. 
# NB: i test sulle stringhe vuote servono solo per evitare messaggi d'errore 
# quando si compila il progetto operativo nin_da con ul pre_chimere.inp generico.
okk=0

# 4.1.1
if [ -z $proj_nml ] ; then
  echo "Warning: progetto non specificato!"
elif [ $proj != $proj_nml ] ; then
  echo "WARNING: Il file pre_chimere.inp si riferisce al progetto "$proj_nml 
  echo "  (valore atteso: "$proj")"
fi
if [ -z $hh_ini ] ; then
  echo "Ora di inizio run non specificata, metto 00Z"
  hh_ini="00"
else
  if [ $hh_ini -ne 0 -a $hh_ini -ne 12 ] ; then
    echo "L'ora di inizio dei run deve essere 00 oppure 12"
    exit 2
  elif [ $hh_ini -eq 12 -a $date_ini -ne $date_end ] ; then
    echo "Warning: richiesta una catena di run con inizio alle ore 12"
  fi
fi

# 4.1.2
 if [ $vers != V200709DSIM  -a  $vers != chimere2008asim  -a $vers != chimere2008bsim  -a $vers != chimere2008bcarb ] ; then
  echo "Release Chimere "$vers" non gestita"
  echo "Per release antecedenti il 2005, usare le procedure in chimere/bin2004"
  exit 2
fi

# 4.1.3
if [ -z $scad0 ] ; then
  echo "Warning: scad0 non specificata!"
  okk=1
elif [ $scad0 -ne -1 -a $scad0 -ne `expr $scad0 / 24 \* 24` ] ; then
  echo "la scadenza iniziale deve essere -1 oppure un multiplo di 24"
  exit 2
fi

# 4.1.4
if [ -z $date_ini ] ; then
  echo "Warning: date_ini non specificata!"
  okk=1
fi
if [ -z $date_end ] ; then
  echo "Warning: date_end non specificata!"
  okk=1
fi
if [ $okk -eq 0 ] ; then
  if [ $scad0 -ne -1 -a $date_ini -ne $date_end ] ; then
    echo "Warning: E' stata richiesta una serie concatenata di run in previsione:"
    echo "configurazione non testata, i risultati potrebbero essere imprevedibili"
  fi
fi

# 4.2 Path dipendenti dal progetto Chimere
data_dir=$proj_dir/data                 # dati statici chimere
fis_dir=$proj_dir/fis                   # griglia e files fisiografici
chem_dir=$proj_dir/chemprep             # file_schema chimico 
dom_dir=$proj_dir/domains               # files relativi alla griglia di calcolo
grb_dir=$proj_dir/grb                   # files per conversione output grib
src_dir=$proj_dir/src                   # root dei sorgenti chimere

run_root=$scratch_root/${proj}/         # home di esecuzione
tmp_root=$run_root/run/                 # root delle dir. di esecuzione
tmp_dir=$tmp_root                       # default dir. di esecuzione (sovrarscritta se run_sync=1)
exe_dir=$run_root/bin/                  # Dir. eseguibili catena chimere
meteo_dir=$run_root/meteo/              # Dir. input meteo
emi_dir=$run_root/emi/                  # Dir. input emissioni
latboun_dir=$run_root/bc/               # Dir. BC laterali
topboun_dir=$run_root/bc/               # Dir. BC top
output_dir=$run_root/out/               # Dir. output chimere
init_dir=$run_root/out/                 # Dir. IC per il primo run STD
log_dir=$run_root/log/                  # Dir. in cui vengono archiviati i log
garbagedir=$run_root/log/
csimul_dir=$scratch_root/$csim/out      # Dir. con output del coarse run
simuldir=$scratch_root/OUTPUTS          # Dir. con output del coarse run

chimere_home=${chimere_root}/${release} # dir della release Chimere

labchem=${mecachim}${aero}${seasalt}${pops}${dustout}${carb}${trc}${soatyp}${dust}.${nbins}
chemprepdir=${chimere_root}/chemprep
chemprepfic=${chemprepdir}/inputdata.${labchem}

bspc_dir=$chimere_home/data/boundaries_spec # elenco specie delle BC climatologiche
eminv_dir=$eminv_root/${dom}/${emis}    # Dir. inventario emissioni

# 4.3 Parametri dipendenti dal progetto Chimere
case ${dataset} in
  COSMO_I7 | LM7TMPC) lm_lsm=${lm_lsm_dir}/LMSMR4_lfrac_2012.grb ;;
  LAMAZ) lm_lsm=${lm_lsm_dir}/LAMAZ_lfrac.grb ;;
  *) ;;                                                    # File LSM non richiesto
esac

if [ $eros -ne 0 -o $resu -ne 0 ] ; then
  soilhum=0
else
  soilhum=1
fi

if [ -z $ptop_met ] ; then
  ptopmet=500
fi
