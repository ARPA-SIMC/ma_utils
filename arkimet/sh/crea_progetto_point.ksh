#!/bin/bash
#==========================================================================
# Procedura interattiva per costurire un nuovo progetto di estrazione su 
# punti da un qualunque archivio Arkimet.
# Versioni <7.0 come crea_progetto_estra.ksh
#
# Per modificare i path del pacchetto ma_utils, assegnare le variabili:
# MA_UTILS_DAT: tabelle ma_utils (ad esempio: ~eminguzzi/git/ma_utils/data;
#               se non specificato, usa: /usr/share/ma_utils)
# MA_UTILS_SVN: esegubili (ad esempio ~eminguzzi/git/ma_utils; se non 
#   specificato, usa gli eseguibili in /usr/libexec/ma_utils)
#
# Per modificare i path di pacchetti libsim/dballe, assegnare le variabili:
# LIBSIM_DATA:  tabelle libsim (se non specificato usa: /usr/share/libsim)
# DBA_TABLES:   tabelle dballe (se non specificato usa: /usr/share/wreport)
# LIBSIM_SVN:   esegubili (ad esempio: ~eminguzzi/svn/libsim; se non 
#               specificato, usa gli eseguibili in path)
#
# Per estrazioni molto pesanti, modificare la dir di lavoro col comando:
#   export AK_TEMP={path locale} (di deafult e' in ${TEMP}/akq.*)
#
# Di deafult, l'estrazione arkimet avviene un reftime alla volta; per
#   estrarre lunghe serie di pochi parametri, conviene usare l'opzione:
#   -seropt="-split=day"
#
# Todo:
# Gestire il caso in cui $doc_file contiene progetti senza il carattere "_"
#
#                                                 V9.1.1, Enrico 02/07/2020
#==========================================================================
# set -ex   # per attivare set -e, bisogna scommentare tutte le rige #e 
# set -x

#==========================================================================
# 0) Funzioni

#----------------------------------------------------------------------
# 0.1 Scrive a schermo l'help della procedura

function write_help
{
#       123456789012345678901234567890123456789012345678901234567890123456789012345
  echo "Uso: crea_progetto_point.ksh PROGETTO [-seropt=\"OPT1 OPT2 ...\"] "
  echo "  [-template adms/calpuff] [-batch] [-list] [-h]"
  echo
  echo "PROGETTO: nome del progetto di estrazione"
  echo "-seropt:  eventuali opzioni della chiamata ad ak_seriet (vedi: "
  echo "          /usr/libexec/ma_utils/ak_seriet.ksh -h)"
  echo "-template imposta template per estrazioni specifiche"
  echo "-batch:   estrae e archivia, senza input interattivo"
  echo "-list:    elenca i progetti salvati e termina"
  echo "-h        visualizza questo help e termina"
  echo ""
  echo "Note"
}

#----------------------------------------------------------------------
# 0.2 Ritorna la lista dei progetti archiviati

function list_proj
{
tmp_dir=`mktemp -d`
if [ -d ${arc_root} ] ; then
  cd ${arc_root}
  ls -1 | cut -d _ -f 1 | uniq | tail -n +2 > $tmp_dir/tmp.ls
  while read line ; do
    ls -1d $line* | tail -n 1
  done < $tmp_dir/tmp.ls
else
  echo "dir "${arc_root}" non trovata"
fi
}

#----------------------------------------------------------------------
# 0.3 Ritorna la variabile d'ambiente str_out, che contiene len_req
#     caratteri, di cui i primi sono "0" e gli ulitmi conicidono con 
#     str_in. USO: intfill str_in len_req

function intfill
{
  str_in=$1
  len_req=$2

  len_in=`echo $str_in | awk '{print length($1)}'`
  nzeri=`expr $len_req - $len_in`

  str_out=""
  cnt_private=0
  while [ $cnt_private -lt $nzeri ] ; do
    str_out=${str_out}"0"
    cnt_private=`expr $cnt_private + 1`
  done
  str_out=${str_out}${str_in}
}

#==========================================================================
# 1) Preliminari

# 1.1) Path fissi
work_root=/autofs/scratch-mod/eminguzzi/arkimet/tmp_point # root dir lavoro
arc_root=~eminguzzi/arkimet/progetti_point        # root arc. estrazioni
doc_file=${arc_root}/_doc/progetti_estra.list     # elenco progetti archiviati

# 1.2) Utility e files di appoggio (da ma_utils)
# Alcuni files di appoggio reltivi a vecchi archivi sono in:
# ~eminguzzi/arkimet/templates (templates per estrazioni arkimet)
# ~eminguzzi/util/grib/lm_ope  (dati fisiografici)

if [ -z $MA_UTILS_DAT ] ; then
  arkimet_aree=/usr/share/ma_utils/arkimet_aree.dat
  template_dir=/usr/share/ma_utils/
  fisiog_dir=/usr/share/ma_utils/
else
  echo "(crea_progetto_point.ksh) Path tabelle ma_utils: "$MA_UTILS_DAT
  arkimet_aree=${MA_UTILS_DAT}/arkimet_aree.dat
  template_dir=${MA_UTILS_DAT}
  fisiog_dir=${MA_UTILS_DAT}
fi

if [ -z $MA_UTILS_SVN ] ; then
  ak_seriet=/usr/libexec/ma_utils/ak_seriet.ksh
  sel_punti=/usr/libexec/ma_utils/sel_punti.exe
  gacsv2seriet=/usr/libexec/ma_utils/gacsv2seriet.exe
  plot_local_orog=/usr/libexec/ma_utils/plot_local_orog.gs
  stat_orari=/usr/libexec/ma_utils/stat_orari.exe
  windrose=/usr/libexec/ma_utils/windrose.sh
  GASCRP=/usr/libexec/ma_utils/
else 
  echo "(crea_progetto_point.ksh) Eseguibili ma_utils: copia di lavoro in "$MA_UTILS_SVN
  ak_seriet=${MA_UTILS_SVN}/arkimet/sh/ak_seriet.ksh
  sel_punti=${MA_UTILS_SVN}/arkimet/src/sel_punti.exe
  gacsv2seriet=${MA_UTILS_SVN}/arkimet/src/gacsv2seriet.exe
  plot_local_orog=${MA_UTILS_SVN}/arkimet/sh/plot_local_orog.gs
  stat_orari=${MA_UTILS_SVN}/osservazioni/src/stat_orari.exe
  windrose=${MA_UTILS_SVN}/osservazioni/sh/windrose.sh
  GASCRP=${MA_UTILS_SVN}/util/grads/sh
fi

# Scelgo il gruppo per le dir di lavoro e di archiviazione
arc_grp=$(groups | awk '{print $1}')
groups | grep sim-modellisti > /dev/null && arc_grp=sim-modellisti
groups | grep sim-aria > /dev/null && arc_grp=sim-aria
echo "Grupppo per files da archiviare: "$arc_grp

if [ ! $EDITOR ] ; then
  editor=emacs
elif [ $EDITOR = "vi" ] ; then
  editor=vi
else
  editor=emacs
fi

# 1.3) Parametri da riga comando

# default
proj="nil"
seropt=""
batch="N"
force_tmpl="nil"

# parsing
if [ $# -eq 0 ] ; then
  write_help
  exit
fi
mand_par=0
while [ $# -ge 1 ] ; do
  if [ `echo $1 | awk '{print $1}'` = '-h' ] ; then
    write_help
    exit
  elif [ `echo $1 | awk '{print $1}'` = '-list' ] ; then
    list_proj
    exit
  elif [ `echo $1 | awk '{print $1}'` = '-template' ] ; then
    shift
    force_tmpl=$1
    shift
  elif [ `echo $1 | awk '{print $1}'` = '-batch' ] ; then
    batch="Y"
    shift
  elif [ `echo $1 | awk '{print $1}'` = '-debug' ] ; then
    seropt=$seropt" -debug"
    shift
  elif [ `echo $1 | awk '{print substr($1,1,7)}'` = '-seropt' ] ; then
    seropt=$seropt" "`echo $1 | cut -d = -f 2-`
    shift
  elif [ $mand_par -eq 0 ] ; then
    proj=$1
    mand_par=1
    shift
  else
    echo "Skip parametro non gestito "$1
    shift
  fi
done

if [ $mand_par -ne 1 ] ; then
  write_help
  exit
fi
if [ $force_tmpl != "nil" -a $force_tmpl != "adms" -a $force_tmpl != "calpuff" ] ; then
  echo "Template "$force_tmpl" non gestito"
  exit
fi

# 1.4) Se questo progetto non ha un numero d'ordine ed esistono progetti
#      archiviati con lo stesso nome, trovo il primo numero d'ordine libero
if [ $batch = "N" ] ; then
  nfields=`echo $proj | awk '{print gsub("_","###",$1)}'`
  if [ $nfields -eq 0 ] ; then
    line=`grep $proj $doc_file | grep -i progetto | tail -n 1`
    if [ ! -z "$line" ] ; then
      last_id=`echo ${line##*_}`
      new_id=`expr $last_id + 1`
      intfill $new_id 2
      proj_new=${proj}_${str_out}
      echo "Esistono dei progetti archiviati con nome: "$proj
      echo "Uso come nome del progetto: "$proj_new" (Y/N)?"
      read yn
      if [ -z $yn ] ; then
        echo "Elaboro il progetto "$proj
      elif [ $yn = "y" -o $yn = "Y" ] ; then
        proj=$proj_new
        echo "Elaboro il progetto "$proj
      fi
    fi
  fi
fi

# 1.5) Variabili dipendenti dai parametri
arc_dir=${arc_root}/${proj}                     # dir. archiviazione
work_dir=${work_root}/${proj}                   # dir. di lavoro

# 1.6) Se e' richiesta l'elaborazione batch, controllo che ci siano tutti
#      i files necessari
if [ $batch = "Y" ] ; then
  if [ ! -d $work_dir ] ; then
    echo "Dir non trovata "$work_dir
    exit
  fi
  if [ ! -s $work_dir/$proj.ds ] ; then
    echo "File non trovato "$work_dir/$proj.ds
    exit
  fi
  if [ ! -s $work_dir/$proj.akq ] ; then
    echo "File non trovato "$work_dir/$proj.akq
    exit
  fi
  if [ ! -s $work_dir/$proj.pts.csv ] ; then
    echo "File non trovato "$work_dir/$proj.pts.csv
    exit
  fi

  modif="M"
fi

#==========================================================================
# 2) Costruzione delle directory e dei files di appoggio per l'estrazione

#--------------------------------------------------------------------------
# 2.1) Directory di lavoro e gestione di un progetto gia' esistente

if [ $batch = "N" ] ; then
  if [ -d $work_dir ] ; then
    echo "Il progetto "$proj" esiste gia: "
    echo "(M)odifico, (R)icopro, (S)tatistiche e archiviazione, (T)ermino"
    read yn
    if [ -z $yn ] ; then
      exit
    elif [ $yn != "m" -a $yn != "M" -a $yn != "r" -a $yn != "R" -a \
           $yn != "s" -a $yn != "S" ] ; then
      exit
    elif [ $yn = "s" -o $yn = "S" ] ; then
      modif="S"
    elif [ $yn = "r" -o $yn = "R" ] ; then
      modif="R"
      chgrp $arc_grp $work_dir
      chmod g+s $work_dir
    else
      modif="M"
    fi
  
  else
    modif="R"
    if [ ! -d $work_root ] ; then
      echo "Dir. non trovata: "$work_root
      exit
    fi
    cd $work_root
    mkdir $proj
    if [ $? -ne 0 ] ; then
      echo "Errore creando la dir "${work_root}/${proj}
      exit
    fi
    chgrp $arc_grp $proj
    chmod g+s $proj
  fi
fi

cd $work_dir
echo "Directory di lavoro del progetto: "`pwd`
if [ $modif = "R" ] ; then
  rm -f * > /dev/null
fi

#--------------------------------------------------------------------------
# 2.2) Scelta del dataset

if [ $batch = "N" -a $modif != "S" ] ; then
  if [ -f ${proj}.ds ] ; then
    old_ds=`head -n 1 ${proj}.ds`
    echo "Il progetto "$proj" e' gia' stato lanciato sul dataset "$old_ds
    echo "  - premi invio per estrarre ancora da "$old_ds
    echo "  - altrimenti immetti il nome di un altro dataset"
    read dummy
    if [ -z $dummy ] ; then
      dataset=$old_ds
    else
      dataset=$dummy
    fi
    
  else
    echo "Immetti il nome del dataset da cui estrarre"
    read dataset
    if [ -z $dataset ] ; then
      echo "Occorre specificare il dataset Arkimet da cui estrarre"
      exit
    fi
  fi
else
  dataset=`head -n 1 ${proj}.ds`
fi

# 01/06/2018: nell'archivo arkimet adesso i nomi dei dataset mescolano maiuscole e
# minuscole (es: cosmo_5M_vol_ita), quindi tolgo il cambio di caso da qui e da
# ak_seriet.ksh
# dataset=`echo $dataset | tr '[:upper:]' '[:lower:]'`

# salvo il nome del dataset
echo $dataset > ${proj}.ds

# Cerco le informazioni relative al dataset richiesto
dataset1=$(echo $dataset | cut -d % -f 1)
line=`grep ^$dataset1, $arkimet_aree`
if [ $? -eq 0 ] ; then
  dsarea=`echo $line | cut -d , -f 2 | tr '[:lower:]' '[:upper:]'`
  dsproj=`echo $line | cut -d , -f 3 | tr '[:lower:]' '[:upper:]'`
  fisiog=`echo $line | cut -d , -f 4 | tr '[:lower:]' '[:upper:]'`
  destag=`echo $line | cut -d , -f 5 | tr '[:lower:]' '[:upper:]'`
  gredit=`echo $line | cut -d , -f 6`
  ak_sop=`echo $line | cut -d , -f 7 | tr '[:lower:]' '[:upper:]'`
  aktmpl=`echo $line | cut -d , -f 8`
else
  dsarea="NIL"  
  dsproj="NIL"
  fisiog="N"
  destag="N"
  gredit=1
  ak_sop="N"
  aktmpl="def"
fi

nmltmpl="def"
if [ $force_tmpl = "adms" ] ; then
  aktmpl=${aktmpl}.adms
  nmltmpl="adms"
elif [ $force_tmpl = "calpuff" ] ; then
  aktmpl=${aktmpl}.calpuff
fi

iret=$(echo $seropt | awk '{print match($1,"fop")}')
if [ $ak_sop = "Y" -o $iret -gt 0 ] ; then
  fop_req="Y"
else
  fop_req="N"
fi

echo "Area: "$dsarea"; proiezione: "$dsproj"; edition: "$gredit
echo "Fisiog: "$fisiog"; destag: "$destag"; sop: "$fop_req
echo "Template: "${template_dir}/template.akq.${aktmpl}

#--------------------------------------------------------------------------
# 2.3) Scelta dei campi

if [ $batch = "N" -a $modif != "S" ] ; then
  echo "Scegli i campi:"

  if [ $modif = "R" -o ! -f ${proj}.akq ] ; then
    rm -f ${proj}.akq

    if [ -f ${template_dir}/template.akq.${aktmpl} ] ; then
      cp ${template_dir}/template.akq.${aktmpl} ./${proj}.akq
    else
      echo "Template non trovato: template.akq."${aktmpl}
      cp $template_dir/template.akq.def ./${proj}.akq
    fi
  fi
  
  $editor ${proj}.akq 2>/dev/null
fi

#--------------------------------------------------------------------------
# 2.4) Scelta dei punti

# Gestisco il caso in cui il file dei punti esiste gia'
if [ $batch = "N" -a $modif != "S" ] ; then
  echo "Scegli i punti:"
  if [ $modif = "M" -a -f ${proj}.pts.csv ] ; then
    nl=`wc -l ${proj}.pts.csv | awk '{print $1}'`
    nprq=`expr $nl - 1`
    echo "Punti selezionati:"
    echo ""
    cat "$proj".pts.csv
    echo ""
    echo "Totale: "$nprq" punti"
    echo "Li modifico? (Y/N)" 
    read yn
    if [ -z $yn ] ; then
      new_pts="N"
    elif [ $yn = "y" -o $yn = "Y" ] ; then
      new_pts="Y"
    else
      new_pts="N"
    fi
  else
    new_pts="Y"
  fi
fi

# Scelgo i punti
if [ $batch = "N" -a $modif != "S" ] ; then
  if [ $new_pts = "Y" ] ; then
    if [ $dsarea = "NIL" ] ; then
      echo "WARNING: nella scelta dei punti, verificare l'area!!"
      echo "(premere invio per continuare)"
      read
    fi
  
    rm -f ${proj}.pts.csv sel_punti.inp sel_punti.org tail.txt
    $sel_punti -c
    mv sel_punti.inp sel_punti.org
    echo $proj                                            > sel_punti.inp
    head -n 5 sel_punti.org | tail -n 4                   >> sel_punti.inp
  
    if [ $dsproj = "GEO" -o $dsproj = "ROT" -o $dsproj = "ROT1" -o $dsproj = "ROT2" ] ; then
      echo "1          ! tipo di grigliato modello"       >> sel_punti.inp
    elif [ $dsproj = "UTM" ] ; then
      echo "0          ! tipo di grigliato modello"       >> sel_punti.inp
    else
      echo "1          ! tipo di grigliato modello"       >> sel_punti.inp
    fi
    echo $dsarea                                          >> sel_punti.inp
    tail -n +8 sel_punti.org                              >> sel_punti.inp
  
    $editor sel_punti.inp 2>/dev/null
    $sel_punti
    echo "punti selezionati:"
    cat "$proj".pts.csv
  fi
fi

#--------------------------------------------------------------------------
# 2.5) Scelta delle opzioni di output

if [ $batch = "N" -a $modif != "S" ] ; then
  echo "Seleziona le opzioni per il formato di output"

  if [ $modif = "R" -o ! -f gacsv2seriet.nml ] ; then
    rm -f gacsv2seriet.nml

    if [ ! -z $nmltmpl ] ; then
      if [ -f $template_dir/gacsv2seriet.nml.${nmltmpl} ] ; then
        cp $template_dir/gacsv2seriet.nml.${nmltmpl} ./gacsv2seriet.nml
      else
        echo "Namelist non trovata "gacsv2seriet.nml.${nmltmpl}
        $gacsv2seriet -c
      fi
    else
      $gacsv2seriet -c
    fi
  fi
  
  $editor gacsv2seriet.nml 2>/dev/null
fi

#--------------------------------------------------------------------------
# 2.6) Se necessario, gestisco i dati su griglia staggherata

iret=$(echo $seropt | awk '{print match($1,"destag")}')

if [ $destag = "Y" -o  $iret -gt 0 ] ; then
  if [ $batch = "N" -a $modif != "S" ] ; then
    echo "La query contiene dati su griglia staggerata (es. vento sui model levels)? (Y/N)"
    read yn
    if [ -z $yn ] ; then
      echo ""
    elif [ $yn = "y" -o $yn = "Y" ] ; then
      seropt=$seropt" "-destag
    fi
  else
    seropt=$seropt" "-destag
  fi
fi

#--------------------------------------------------------------------------
# 2.7) Se necessario, gestisco i dati con codifica al secondo ordine


if [ $fop_req = "Y" ] ; then
  if [ $batch = "N" -a $modif != "S" ] ; then
    echo "La query contiene dati con codifica al 2o ordine (qcr,qis,tke)? (Y/N)"
    read yn
    if [ -z $yn ] ; then
      echo ""
    elif [ $yn = "y" -o $yn = "Y" ] ; then
      seropt=$seropt" "-fop
    fi
  else
    seropt=$seropt" "-fop
  fi
fi

#==========================================================================
# 3) Estrazione e post processing

#--------------------------------------------------------------------------
# 3.1) Estrazione

# Cerco e raduno i parametri fisiografici
if [ $modif = "S" ] ; then
  fis_list="orog"
else
  fis_list="orog layers"
fi

if [ $dsarea != "NIL" -a $fisiog = "Y" ] ; then
  if [ $gredit = 2 ] ; then
    ext="grib2"
  else
    ext="grb"
  fi

  rm -f $proj".fisiog."${ext}
  for field in $fis_list ; do
    nf=`ls -1 ${fisiog_dir}/${dsarea}_${field}_*.${ext} 2>/dev/null | wc -l`
    if [ $nf -eq 0 ] ; then
      file="nil"
      echo "Dati fisiografici non disponibili per l'area "$dsarea": "$field
    elif [ $nf -eq 1 ] ; then
      file=${fisiog_dir}/${dsarea}_${field}_*.${ext}
      echo "Dati fisiografici ok per l'area "$dsarea": "$field
    elif [ $nf -gt 1 -a $batch = "N" ] ; then
      echo "Per questa area sono disponibili diversi files di dati fisiografici:"
      echo "selezionare quello da usare per il parametro "$field
      ls -r1 ${fisiog_dir}/${dsarea}_${field}_*.${ext}
      read file
    elif [ $nf -gt 1 -a $batch = "Y" ] ; then
      file=`ls -r1 ${fisiog_dir}/${dsarea}_${field}_*.${ext} | head -n 1`
      echo "uso dati fisiografici da "$file
    fi

    if [ $file != "nil" ] ; then
      cat $file >> $proj".fisiog.${ext}"
    fi
    if [ $field = "orog" ] ; then
      if [ $file != "nil" ] ; then
        ctl_orog="${fisiog_dir}/"`basename $file $ext`"ctl"
      else
        ctl_orog="nil"
      fi
    fi

  done
  if [ $fisiog = "Y" ] ; then
    seropt=$seropt" -fis="$proj".fisiog."${ext}
  fi

else
  ctl_orog="nil"

fi

# Lancio ak_seriet
if [ $modif != "S" ] ; then
#e set +e
  echo "Comando di estrazione:"
  echo $ak_seriet $proj $dataset -inpdata=arkimet -reqdata=pts $seropt
  $ak_seriet $proj $dataset -inpdata=arkimet -reqdata=pts $seropt
  errc=$? 
  if [ $errc -ne 0 -a $errc -lt 100 ] ; then
    echo "Errore ak_seriet.ksh, termino"
    exit
  fi
#e set -e
fi

#--------------------------------------------------------------------------
# 3.2) Post-processing: a richiesta, calcolo statistiche di base, rosa dei
#      venti, orografia locale

# 3.2.1 Gestione dei punti da elaborare

if [ $modif = "S" ] ; then
  nfp=`ls ${proj}_punti_???.txt -1 | wc -l`
  if [ $nfp -eq 0 -o ! -s ${proj}_punti_001.txt ] ; then
    echo "Nessun file di output trovato per il progetto "$proj
    exit
  fi
  nlin=`wc -l ${proj}_punti_001.txt| awk '{print $1}'`
  echo "Progetto "$proj", dati estratti: "$nfp" punti, "`expr $nlin - 6`" istanti"
fi

if [ $batch = "N" ] ; then
  echo ""
  echo "Si vogliono calcolare le statistiche relative ai dati estratti?"
  echo "(0: No; 1: solo punto centrale; 2: tutti i punti)"
  read yn
else
  yn=1
fi

if [ -z $yn ] ; then
  plt=1
elif [ $yn != "0" -a $yn != "1" -a $yn != "2" ] ; then
  plt=1
else
  plt=$yn
fi
out_form=`grep ^out_form gacsv2seriet.nml | head -n 1 | sed 's/ //g' | \
  cut -d = -f 2 | cut -d , -f 1`

# 3.2.2 Elaboro solo il punto centrale
if [ $plt -eq 1 ] ; then
  nlines=`wc -l ${proj}.pts.csv | awk '{print $1}'`
  nlinerq=`expr $nlines / 2 + 1`    # n.ro riga corrispondente al punto centrale
  nprq=`expr $nlinerq - 1`          # n.ro progressivo del punto centrale
  strrq=`head -n $nlinerq ${proj}.pts.csv | tail -n 1`
  labprq=`echo $strrq | cut -d , -f 3`  
  idxprq=`echo $strrq | cut -d , -f 6`

  echo "Elaboro il punto "$nprq": "$labprq
  intfill $nprq 3
  rm -f wrose.log grads.log ${proj}_punti_*_stats.sta ${proj}_orog_*.png ${proj}_wrose_*.png
  
# Stat e Windrose
  if [ $out_form = "1" ] ; then
    $stat_orari -s -liv -prod=stats ${proj}_punti_${str_out}.txt

#e  set +e
    for lev in 10 9 8 7 ; do
      $windrose -s -lev $lev ${proj}_punti_${str_out}.txt >> wrose.log 2>&1
      wrret=$?
      if [ $wrret -eq 1 ] ; then
        echo "Windrose impossibile, in genere per dati insufficienti (vedi file wrose.log)"
        break
      elif [ $wrret -gt 1 ] ; then
        echo "Windrose a ${lev} m fallita"
      else
        echo "Windrose a ${lev} m ok"
        break
      fi
    done
#e  set -e

    if [ $wrret -eq 0 ] ; then
      wrfile=`ls -1rt wrose_*.png | tail -n 1`
      mv $wrfile ${proj}_wrose_${str_out}.png
    fi
  fi

  if [ $dsarea = "NIL" ] ; then
    echo "Plot orografia impossibile (area sconosciuta per il dataset "$dataset")"
  elif [ $ctl_orog = "nil" ] ; then
    echo "Plot orografia impossibile (nessun file di ororgrafia per l'area "$dsarea")"
  elif [ ! -s $ctl_orog ] ; then
    echo "Plot orografia impossibile (file "$ctl_orog" non trovato)"
  else
    echo "Plot orografia, punto "$idxprq
    echo "Comando: grads -clb 'run '$plot_local_orog' '$ctl_orog' '$dsproj' '$idxprq >> grads.log 2>&1"
    grads -clb 'run '$plot_local_orog' '$ctl_orog' '$dsproj' '$idxprq >> grads.log 2>&1
    mv orog.png ${proj}_orog_${str_out}.png
  fi

# 3.2.3 Elaboro tutti i punti
elif [ $plt -eq 2 ] ; then
  tmp_file=`mktemp XXX.pts.csv`
  tail -n +2 ${proj}.pts.csv > $tmp_file
  rm -f wrose.log grads.log ${proj}_punti_*_stats.sta ${proj}_orog_*.png ${proj}_wrose_*.png
  nprq=0
  while read line ; do
    nprq=`expr $nprq + 1`
    labprq=`echo $line | cut -d , -f 3`  
    idxprq=`echo $line | cut -d , -f 6`
    echo "Elaboro il punto "$nprq": "$labprq
    intfill $nprq 3
    idp3=$str_out

#   Stat e Windrose
    if [ $out_form = "1" ] ; then
      $stat_orari -s -liv -prod=stats ${proj}_punti_${idp3}.txt

#e    set +e
      for lev in 10 9 8 7 ; do
        $windrose -s -lev $lev ${proj}_punti_${idp3}.txt >> wrose.log 2>&1
        wrret=$?
        if [ $wrret -eq 1 ] ; then
          echo "Windrose impossibile, in genere per dati insufficienti (vedi file wrose.log)"
          break
        elif [ $wrret -gt 1 ] ; then
          echo "Windrose a ${lev} m fallita"
        else
          echo "Windrose a ${lev} m ok"
          break
	fi
      done

      if [ $wrret -eq 0 ] ; then
        wrfile=`ls -1rt wrose_*.png | tail -n 1`
        mv $wrfile ${proj}_wrose_${idp3}.png
      fi
    fi
      
#   Orografia locale
    if [ $dsarea = "NIL" ] ; then
      echo "Plot orografia impossibile (area sconosciuta per il dataset "$dataset")"
    elif [ -z $ctl_orog ] ; then
      echo "Plot orografia impossibile (nessun file di ororgrafia per l'area "$dsarea")"
    elif [ ! -s $ctl_orog ] ; then
      echo "Plot orografia impossibile (file "$ctl_orog" non trovato)"
    else
      echo "Plot orografia, punto "$idxprq
      echo " Comando: grads -clb 'run '$plot_local_orog' '$ctl_orog' '$dsproj' '$idxprq  >> grads.log 2>&1"
      grads -clb 'run '$plot_local_orog' '$ctl_orog' '$dsproj' '$idxprq  >> grads.log 2>&1
      mv orog.png ${proj}_orog_${str_out}.png
    fi

#   File con le medie sui punti di tutti i parametri
    if [ $nprq -eq 1 ] ; then  
      echo "" > tmp.1.dat
      echo "id-pts" >> tmp.1.dat
      head -n 2 ${proj}_punti_${idp3}_stats.sta | cut -c 7- > tmp.2.dat
    fi
    intfill $nprq 6
    idp6=$str_out
    echo $idp6 >> tmp.1.dat
    head -n 6 ${proj}_punti_${idp3}_stats.sta | tail -n 1 | cut -c 7- >> tmp.2.dat

  done < $tmp_file

  paste tmp.1.dat tmp.2.dat > ${proj}_allave.dat
fi

#==========================================================================
# 4) Archiviazione: se richiesto, salvo i files utili nella directory di 
#    archiviazione e aggiorno il log file dei progetti

if [ $batch = "N" ] ; then
  echo "Si vuole archiviare l'estrazione? (Y/N)"
  read yn
else
  yn="Y"
fi
if [ -z $yn ] ; then
  exit

elif [ $yn = "y" -o $yn = "Y" ] ; then

  if [ -d $arc_dir ] ; then
    if [ $batch = "N" ] ; then
      echo "Il progetto "$proj" e gia stato archviato: "
      echo "(C)ancello files esistenti e sovrascrivo, (S)ovrascrivo, (T)ermino"
      read yn
    else
      yn="s"
    fi
    if [ -z $yn ] ; then
      exit
    elif [ $yn = "C" -o $yn = "c" ] ; then
      rm -f $arc_dir/*
      chgrp $arc_grp $arc_dir
      chmod g+s $arc_dir
    elif [ $yn != "S" -a $yn != "s" ] ; then
      exit
    fi

  else
    mkdir $arc_dir
    if [ $? -ne 0 ] ; then
      echo "Errore creando la dir "$arc_dir
      exit
    fi
    chgrp $arc_grp $arc_dir
    chmod g+s $arc_dir
 
  fi
  
# Piano di estrazione
#e set +e
  cp ${proj}.ds $arc_dir
  cp ${proj}.akq $arc_dir
  cp ${proj}.varliv.csv $arc_dir
  cp ${proj}.pts.csv $arc_dir
  cp ${proj}.datasca.csv $arc_dir
  cp ${proj}_fisiog.csv $arc_dir
  cp ${proj}_qcnt.log $arc_dir

# Serie temporali modello
  cp ${proj}_punti_???.txt $arc_dir
  for file in $(ls ${arc_dir}/${proj}_punti_???.txt) ; do
    gzip -f $file
  done

# Statistiche, orografia, windrose
  if [ $plt -eq 1 -o $plt -eq 2 ] ; then
    cp ${proj}_punti_*_stats.sta $arc_dir
    cp ${proj}_orog_*.png $arc_dir
    cp ${proj}_wrose_*.png $arc_dir
  fi
  if [ $plt -eq 2 ] ; then
    cp ${proj}_allave.dat $arc_dir
  fi

  if [ $batch = "N" ] ; then
    echo "Progetto salvato in "$arc_dir"; aggiorno log file"
    $editor $doc_file 2>/dev/null
  else
    echo "Progetto salvato in "$arc_dir
    echo "***** RICORDARSI DI AGGIORNARE IL LOG DELLE ESTRAZIONI *****"
  fi
fi

exit
