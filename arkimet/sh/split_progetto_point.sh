#/bin/bash
#==========================================================================
# Procedura per suddividere in segmenti, eseguire e riunire gli ouput di
# un progetto di estrazione su punti.
# Pensato per lanciare a segmenti le estrazioni "massicce" che non riescono
# ad arrivare alla fine.
#
# Todo:
# - aggiungere opzioni per ak_seriet
# - aggiungere controllo e rilancio dei segmenti incompleti
#
#                                         Versione 1.0.2, Enrico 06/08/2020
#==========================================================================
#set -x 

#==========================================================================
# 1) Preliminari

#--------------------------------------------------------------------------
# 1.0 Path utility, costanti
work_root=/autofs/scratch2/eminguzzi/arkimet/tmp_point
if [ ! -d $work_root ] ; then
  echo "Dir "$work_root" non trovata"
  exit
fi
  
if [ -z $MA_UTILS_SVN ] ; then
  ak_seriet=/usr/libexec/ma_utils/ak_seriet.ksh
else
  echo "(split_progetto_point.ksh) Eseguibili ma_utils: copia di lavoro in "$MA_UTILS_SVN
  ak_seriet=${MA_UTILS_SVN}/arkimet/sh/ak_seriet.ksh
fi

ntry=3
pctrq=95

#--------------------------------------------------------------------------
# 1.1 Scrive a schermo l'help della procedura

function write_help
{
#       123456789012345678901234567890123456789012345678901234567890123456789012345
  echo "Uso: split_progetto_point.ksh progetto nseg action [-seropt=\"OPT1 OPT2 ...\"]"
  echo "progetto: nome del progetto di estrazione da suddividere"
  echo "nseg:     numero dei segmenti in cui dividere il progetto"
  echo "action:   split (suddivido il progetto), run (lancio i segmenti), merge "
  echo "          (ricompongo l'output del progetto originale)"
  echo
  echo "Sequenza delle operazioni:"
  echo "1) creare il progetto originale (crea_progetto_punti)"
  echo "2) lanciare 3 volte questo script, con le action split, run, merge"
  echo "3) rilanciare il progetto originale per archiviazione"
  echo ""
  echo "Questo script agisce nelle subir di "$work_root
  echo ""
}

#--------------------------------------------------------------------------
# 1.2) Parametri da riga comando

# default
seropt=""

# parsing
if [ $# -lt 3 ] ; then
  write_help
  exit
fi
mand_par=0
while [ $# -ge 1 ] ; do
  if [ `echo $1 | awk '{print $1}'` = '-h' ] ; then
    write_help
    exit
  elif [ `echo $1 | awk '{print substr($1,1,7)}'` = '-seropt' ] ; then
    seropt=$seropt" "`echo $1 | cut -d = -f 2-`
    shift
  elif [ $mand_par -eq 0 ] ; then
    proj=$1
    mand_par=1
    shift
  elif [ $mand_par -eq 1 ] ; then
    nseg=$1
    mand_par=2
    shift
  elif [ $mand_par -eq 2 ] ; then
    action=$1
    mand_par=3
    shift
  else
    echo "Skip parametro non gestito "$1
    shift
  fi
done

if [ $mand_par -ne 3 ] ; then
  write_help
  exit
fi
if [ $action != "split" -a $action != "run" -a $action != "merge" ] ; then
  write_help
  exit
fi

logfile1=${work_root}/${proj}/split.log.seg
logfile2=${work_root}/${proj}/split.log.seriet
rm -f $logfile1 $logfile2

#==========================================================================
# 2) Action = split

if [ $action = "split" ] ; then
  cd $work_root
  modif="R"
  nseg2=$(ls -1d ${proj}_#p? | wc -l 2>/dev/null)
  if [ $nseg2 -gt 0 ] ; then
    echo "Il progetto "$proj" e' gia stato segmentato: "
    echo "(M)odifico, (R)icopro, (T)ermino"
    read yn
    if [ -z $yn ] ; then
      exit
    elif [ $yn != "m" -a $yn != "M" -a $yn != "r" -a $yn != "R" ] ; then
      exit
    elif [ $yn = "r" -o $yn = "R" ] ; then
      rm -Rf ${proj}_#p*
    else
      if [ $nseg != $nseg2 ] ; then
	echo "Warning: segmenti richiesti "$nseg" presenti "$nseg2"!!!"
      fi
      modif="M"
    fi
  fi

  query_list=""
  for seg in $(seq $nseg) ; do
    echo "Costruisco segmento "$seg
    projs=${proj}_#p${seg}
    if [ $modif = "R" ] ; then
      mkdir $projs
    fi	
    for suffix in .akq .pts.csv .ds .fisiog.grb ; do
      if [ ! -s ${proj}/${proj}${suffix} ] ; then
        echo "File "${work_root}/${proj}/${proj}${suffix}" non trovato"
        exit
      else
        cp ${proj}/${proj}${suffix} ${projs}/${projs}${suffix}
      fi
    done
    query_list=$query_list" "${projs}/${projs}".akq"
  done

  vim $query_list
fi

#==========================================================================
# 3) Action = run

if [ $action = "run" ] ; then
  
# 3.1) Chiedo se ci sono dati su griglia staggherata
  echo $seropt | grep destag >/dev/null
  if [ $? -ne 0 ] ; then
    echo "La query contiene dati su griglia staggerata (es. vento sui model levels)? (Y/N)"
    read yn
    if [ -z $yn ] ; then
      echo ""
    elif [ $yn = "y" -o $yn = "Y" ] ; then
      seropt=$seropt" "-destag
    fi
  fi

# 3.2) Lancio l'estrazione (ciclo sui segmenti)
  for seg in $(seq $nseg) ; do
    projs=${proj}_#p${seg}
    cd ${work_root}/$projs
    ds=$(cat ${projs}.ds)
    echo "Dir di lavoro del progetto: ${work_root}/${projs}"
    echo "Comando di estrazione: "$ak_seriet $projs $ds -inpdata=arkimet -reqdata=pts $seropt -fis=${projs}.fisiog.grb

    for try in $(seq $ntry) ; do
      echo "### Estrazione: "$projs" tentativo "$try
      echo "" >> $logfile2
      echo "### Estrazione: "$projs" tentativo "$try >> $logfile2
      rm -f tmp.log
      $ak_seriet $projs $ds -inpdata=arkimet -reqdata=pts $seropt -fis=${projs}.fisiog.grb > tmp.log
      pctok=$(grep "validi in output" tmp.log | cut -d \( -f 2 | cut -d . -f 1)
      cat tmp.log >> $logfile2
      if [ $pctok -ge $pctrq ] ; then
        echo $projs $try $pctok "ok!" $(date +"%Y%m%d:%H%M") >> $logfile1
        break
      else
        echo $projs $try $pctok "bad" $(date +"%Y%m%d:%H%M") >> $logfile1
      fi
    done
  done
fi
 
#==========================================================================
# 4) Action = merge

if [ $action = "merge" ] ; then
  cd $work_root
  nl=$(wc -l ${proj}/${proj}.pts.csv | awk {'print $1'})
  npts=$[$nl-1]
  echo "Punti richiesti: "$npts
  for npt in $(seq $npts) ; do
    lab=$(printf "%03d" $npt)
    echo "Elaboro il punto "$lab
    outfile=${proj}/${proj}_punti_${lab}.txt
    rm -f ${outfile}
    for seg in $(seq $nseg) ; do
      projs=${proj}_#p${seg}
      infile=${projs}/${projs}_punti_${lab}.txt
      if [ $seg -eq 1 ] ; then      
        cp $infile $outfile
      else
        tail -n +7 $infile >> $outfile
      fi
    done
  nl=$(wc -l $outfile | awk {'print $1'})
  nist=$[$nl-6]
  nl=$(grep -vce -9999. $outfile)
  nist_ok=$[$nl-6]
  echo "Istanti in output "$nist" di cui completi "$nist_ok
  done

  cp ${proj}_#p1/${proj}_#p1_fisiog.csv ${proj}/${proj}_fisiog.csv 

fi
