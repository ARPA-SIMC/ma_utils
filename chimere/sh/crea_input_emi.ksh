#/bin/ksh
#-------------------------------------------------------------------------------
# crea_input_emi.ksh 
# Script per creare l'input relativo alle emissioni antropogeniche per una serie
# di run di chimere.
#
# Uso: crea_input_emi.ksh PROGETTO [-h]
#
# Input:
# - parametri di controllo del run (pre_chimere.inp)
# - inventario delle emissioni
#
# Note:
#
#                                     Versione 5.0 (V200501H), Enrico 15/08/2005
#-------------------------------------------------------------------------------
#set -x

#-------------------------------------------------------------------------------
# Scrive a schermo l'help della procedura
function write_help
{
  echo "Uso: crea_input_emi.ksh PROJ [-h]"
  echo "Tutti i files di input devono essere gia' pronti"
  return
}

################################################################################
# 1) Elaborazioni preliminari
#

echo "*** CREA_INPUT_EMI.KSH ***"

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

ndays=`$delta_days $date_end $date_ini 2>/dev/null`
ndays=`expr $ndays + 1`

# Controlli sui parametri
if [ $plmrise -eq 1 -a $nlevemis -le 1 ] ; then
  echo "Lo schema di Plum Rise richiede emissioni su piu di un livello"
  exit
fi
if [ $scad0 != -1 -a $scad0 -ne `expr $scad0 / 24 \* 24` ] ; then
  echo "la scadenza iniziale deve essere -1 oppure un multiplo di 24"
  exit
fi

#-------------------------------------------------------------------------------
# 1.3) Ripulisco la directory d'esecuzione

if [ $run_sync -eq 1 ] ; then
  tmp_dir=$tmp_root/emi
else
  tmp_dir=$tmp_root
fi
if [ ! -d $tmp_dir ] ; then
  echo "Directory di lavoro "$tmp_dir" inesistente"
  exit
fi
cd $tmp_dir
rm -f *

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
  echo "crea_input_emi.ksh: elaborazioni run "$datac " ("`date`")"

# Ripulisco la dir di lavoro dai files del run precedente
  rm -f AEMISSIONS*

# Se l'inventario e' in formato compresso, lo scompatto
  if [ ! -s ${eminv_dir}/EMISSIONS-GRID-${dom}.${mm} ] ; then
    if [ -s ${eminv_dir}/EMISSIONS-GRID-${dom}.${mm}.gz ] ; then
      gunzip -v ${eminv_dir}/EMISSIONS-GRID-${dom}.${mm}.gz
    else
      echo "Errore, inventario "${eminv_dir}/EMISSIONS-GRID-${dom}.${mm}" non trovato"
      exit
    fi
  fi
  if [ ! -s ${eminv_dir}/EMISSIONS-PUNT-${dom}.${mm} ] ; then
    if [ -s ${eminv_dir}/EMISSIONS-PUNT-${dom}.${mm}.gz ] ; then
      gunzip -v ${eminv_dir}/EMISSIONS-PUNT-${dom}.${mm}.gz
    else
      echo "Errore, inventario "${eminv_dir}/EMISSIONS-PUNT-${dom}.${mm}" non trovato"
      exit
    fi
  fi

#-------------------------------------------------------------------------------
# 2.2) Calcolo le emissioni per il run corrente. Se e' richiesto il calcolo del
#      plume rise, elabora separatmente le sorgenti puntuali, 
#
# Standard input per prepemis_std.exe:
# sd0       data iniziale, nel formato YYYYMMDDHH
# nhours    durata del run 
# flatreduc eventuale fattore di riduzione (moltiplicativo) per le emissioni 

  case ${emifmt} in
  std)
    if [ $plmrise -eq 0 ] ; then
      echo "${sd0} ${nhours} ${flatreduc}" > tmp.inp
      echo '"'${eminv_dir}/EMISSIONS-${dom}.${mm}'"' >> tmp.inp
      echo "AEMISSIONS" >> tmp.inp
      $exe_dir/prepemis_std.e < tmp.inp

    else
      echo "${sd0} ${nhours} ${flatreduc}" > tmp.inp
      echo '"'${eminv_dir}/EMISSIONS-GRID-${dom}.${mm}'"' >> tmp.inp
      echo "AEMISSIONS_GRID" >> tmp.inp
      $exe_dir/prepemis_std_1l.e < tmp.inp

      echo "${sd0} ${nhours} ${flatreduc}" > tmp.inp
      echo '"'${eminv_dir}/EMISSIONS-PUNT-${dom}.${mm}'"' >> tmp.inp
      echo "AEMISSIONS_PUNT" >> tmp.inp
      $exe_dir/prepemis_std_pts.e < tmp.inp
    fi ;;
  esac

#-------------------------------------------------------------------------------
# 2.3) Archivio il file con le emissioni; se necessario ri-zippo l'inventario

  if [ $plmrise -eq 0 ] ; then
    cp AEMISSIONS AEMISSIONS.${proj}.${datac}
    mv AEMISSIONS.${proj}.${datac} $emi_dir
  else
    cp AEMISSIONS_GRID AEMISSIONS_GRID.${proj}.${datac}
    mv AEMISSIONS_GRID.${proj}.${datac} $emi_dir
    cp AEMISSIONS_PUNT AEMISSIONS_PUNT.${proj}.${datac}
    mv AEMISSIONS_PUNT.${proj}.${datac} $emi_dir
  fi

  if [ $cnt -eq $ndays -o $ddp1 -eq 1 ] ; then
    echo "Ricompatto l'inventario"
    gzip -v ${eminv_dir}/EMISSIONS*${dom}.${mm}
  fi

done
echo "Elaborazioni terminate, "`date`
