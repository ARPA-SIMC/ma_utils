#!/bin/ksh
#==========================================================================
# crea_progetto_grid.ksh
#
# Procedura interattiva per costurire un nuovo progetto di estrazione su 
# sottoarea da un dataset Arkimet.
#
# Per modificare i path del pacchetto ma_utils, assegnare le variabili:
# MA_UTILS_DAT: tabelle ma_utils (ad esempio: /home/eminguzzi/svn/ma_utils/data;
#               se non specificato, usa: /usr/share/ma_utils)
# MA_UTILS_SVN: esegubili (ad esempio: /home/eminguzzi/svn/ma_utils; se non 
#               specificato, usa gli eseguibili in: /usr/libexec/ma_utils)
#
# Per modificare i path di pacchetti libsim/dballe, assegnare le variabili:
# LIBSIM_DATA:  tabelle libsim (se non specificato usa: /usr/share/libsim)
# DBA_TABLES:   tabelle dballe (se non specificato usa: /usr/share/wreport)
# LIBSIM_SVN:   esegubili (ad esempio: /home/eminguzzi/svn/libsim; se non 
#               specificato, usa gli eseguibili in path)
#
# NB: 
# - Ancora da implemntare: gestione dell'output GRIB1 e AFA per dataset 
#   GRIB2 (opzioni -afa e -grib)
# - la conversione FOP dovrebbe diventare superflua quando sara' stato
#   riallineato il dataset LAMAZ
# - quando tutto funzionera' ricordasi di togliere set -x da xargs.ksh
#
#                                                 V4.3.1, Enrico 19/11/2013
#==========================================================================
#set -x 

#==========================================================================
# 0) Funzioni interne

#--------------------------------------------------------------------------
# 0.1 Scrive a schermo l'help della procedura
function write_help
{
  echo "Uso: crea_progetto_grid.ksh PROGETTO [-afa/-3dd] [-unsort]"
  echo ""
  echo "Procedura interattiva per estrarre da un dataset Arkimet una serie storica"
  echo "di dati su griglia, ritagliati su una sottoarea."
  echo "Produce un file per ogni giornata con dati (reference time), in formato GRIB"
  echo "Se possible, costruisce anche orografia, quote e coordiate relativi"
  echo "alla sottoarea richiesta."
  echo ""
  echo "-unsort:  non ordina i grib in output (def: ordina per reftime, trange, level)"
  echo "-3dd:     converte in formato 3D.DAT i dati prodotti (per Calmet)"
  echo "-afa:     converte in formato afa i dati prodotti (obsolescente)"
  echo ""
  echo "Per usare versioni di lavoro di programmi e tabelle, esportare le variabili:"
  echo "          MA_UTILS_DAT, MA_UTILS_SVN, LIBSIM_DATA, DBA_TABLES, LIBSIM_SVN"
  echo ""
}

#==========================================================================
# 1) Preliminari

# 1.0) HOME_MINGUZZI
if [ -z $HOME_MINGUZZI ] ; then
  export HOME_MINGUZZI=/autofs/nethomes/eminguzzi
  if [ -d $HOME_MINGUZZI ] ; then
    echo "Variabile d'environment HOME_MINGUZZI non settata, uso "$HOME_MINGUZZI
  else
    echo "Path "$HOME_MINGUZZI" irraggiungibile"
    exit 1
  fi
fi

# 1.1) Path
template_dir=${HOME_MINGUZZI}/arkimet/templates       # templates .akq
fisiog_dir=${HOME_MINGUZZI}/util/grib/lm_ope          # dati fisiografici
arc_root=${HOME_MINGUZZI}/arkimet/progetti_grid       # root arc. progetti
work_root=/autofs/scratch2/eminguzzi/arkimet/tmp_grid # root dir lavoro
#work_root=/scratch/eminguzzi/tmp_grid
akurl="http://arkimet.metarpa:8090"                   # URL archivio Arkimet

# 1.2) Utility e files di appoggio non in PATH
doc_file=${arc_root}/_doc/progetti_grid.doc       
grb2grads=${HOME_MINGUZZI}/util/grads/bin/grb2grads.ksh
plot_grid_orog=${HOME_MINGUZZI}/arkimet/bin/plot_grid_orog.gs

if [ -z $MA_UTILS_DAT ] ; then
  export MA_UTILS_DAT=/usr/share/ma_utils
else
  echo "(crea_progetto_grid.ksh) Uso le tabelle ma_utils da "$MA_UTILS_DAT
fi
arkimet_aree=${MA_UTILS_DAT}/arkimet_aree.dat
aree_geo=${MA_UTILS_DAT}/aree_geo.dat
aree_utm=${MA_UTILS_DAT}/aree_utm.dat

if [ -z $MA_UTILS_SVN ] ; then
  sel_punti=/usr/libexec/ma_utils/sel_punti.exe
  ma_grib2_grib1=/usr/libexec/ma_utils/ma_grib2_grib1.exe
  grib2latlon=/usr/libexec/ma_utils/grib2latlon.exe
  grib23ddat=/usr/libexec/ma_utils/grib23ddat.exe
  grib2afa=/usr/libexec/ma_utils/grib2afa.exe
  grib_s2f=/usr/libexec/ma_utils/grib_s2f.exe

else 
  echo "(crea_progetto_grid.ksh) Eseguibili ma_utils: copia di lavoro in "$MA_UTILS_SVN
  sel_punti=${MA_UTILS_SVN}/arkimet/src/sel_punti.exe
  ma_grib2_grib1=${MA_UTILS_SVN}/util/grib/src/ma_grib2_grib1.exe
  grib2latlon=${MA_UTILS_SVN}/util/grib/src/grib2latlon.exe
  grib23ddat=${MA_UTILS_SVN}/calmet/src/grib23ddat.exe
  grib2afa=${MA_UTILS_SVN}/util/grib/src/grib2afa.exe
  grib_s2f=${MA_UTILS_SVN}/util/grib/src/grib_s2f.exe

fi

if [ ! $EDITOR ] ; then
  editor=emacs
elif [ $EDITOR = "vi" ] ; then
  editor=vi
else
  editor=emacs
fi

# 1.3) Parametri da riga comando

# default
outfmt="grb"
proj=""
sort="Y"
xargs_opt="--time-interval=hour"

# parsing
if [ $# -eq 0 ] ; then
  write_help
  exit
fi
mand_par=0
while [ $# -ge 1 ] ; do
  if [ $1 = '-h' ] ; then
    write_help
    exit
  elif [ $1 = '-afa' ] ; then
    outfmt="afa"
    shift
  elif [ $1 = '-3dd' ] ; then
    outfmt="3dd"
    shift
  elif [ $1 = '-unsort' ] ; then
    sort="N"
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

# 1.4) Variabili dipendenti dai parametri
arc_dir=${arc_root}/${proj}                   # dir. archiviazione progetto
work_dir=${work_root}/${proj}                 # dir. di lavoro
out_dir=${work_root}/${proj}/out              # dir. dati da inviare

#==========================================================================
# 2) Costruzione delle directory e dei files di appoggio per l'estrazione

#--------------------------------------------------------------------------
# 2.1 Gestione di un progetto gia' esistente

if [ -d $work_dir ] ; then
  new="N"
  echo "Il progetto "$proj" esiste gia: "
  echo "(E)straggo, (M)odifico, (R)icopro, (T)ermino"
  read yn
  if [ -z $yn ] ; then
    exit
  elif [ $yn != "e" -a $yn != "m" -a $yn != "r" -a \
         $yn != "E" -a $yn != "M" -a $yn != "R" ] ; then
    exit
  elif [ $yn = "e" -o $yn = "E" ] ; then
    modif="E"
  elif [ $yn = "r" -o $yn = "R" ] ; then
    modif="R"
  else
    modif="M"
  fi

else
  new="Y"
  modif="R"
  if [ ! -d $work_root ] ; then
    echo "Dir. "$work_root" inesistente"
    exit
   fi 
  cd $work_root  
  mkdir $proj
fi

#--------------------------------------------------------------------------
# 2.2 Ripulisco la dir. di lavoro e quella dei dati di output

# dir. di lavoro
cd $work_dir
echo "Directory di lavoro: "`pwd`

if [ $modif = "R" ] ; then
  rm -f * >/dev/null 2>/dev/null
else
  rm -f arki.log* ${proj}.query* xargs.ksh tmp*
fi

# dir. dei dati di output
if [ ! -d out ] ; then
  mkdir out
else
  nfout=`ls -1 out/${proj}_*.grb out/${proj}_*.grib2 out/${proj}_*.afa \
    2>/dev/null | wc -l`
  if [ $nfout -gt 0 ] ; then
    echo "Cancello i dati gia' estratti ($nfout files)? (Y/N)"
    echo "NB: rispondendo N, se vengono estratte giornate gia' elaborate"
    echo "    i dati saranno appesi ai files esistenti"
    read yn
    if [ -z $yn ] ; then
      echo ""
    elif [ $yn = "y" -o $yn = "Y" ] ; then
      rm -f out/${proj}_*.grb out/${proj}_*.grib2 out/${proj}_*.afa
    fi
  fi
fi

#--------------------------------------------------------------------------
# 2.3) Scelta del dataset

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
  echo "Immetti il nome del dataset da cui estrarre (sintassi per ds multipli: DS1%DS2)"
  read dataset
  if [ -z $dataset ] ; then
    exit
  fi
fi

# passo a lowercase
dataset=`echo $dataset | tr '[:upper:]' '[:lower:]'`

# salvo il nome del dataset
echo $dataset > ${proj}.ds

#--------------------------------------------------------------------------
# 2.4 Scelta dei campi

echo "Scegli i campi:"
if [ $modif = "R" -o ! -f ${proj}.akq ] ; then
  rm -f ${proj}.akq
  if [ $outfmt = "3dd" -a -f $template_dir/template.akq.${dataset}.3dd ] ; then
    cp $template_dir/template.akq.${dataset}.3dd ./${proj}.akq
  elif [ -f $template_dir/template.akq.${dataset} ] ; then
    cp $template_dir/template.akq.${dataset} ./${proj}.akq
  else
    echo "Template non trovato per il dataset "$dataset
    cp $template_dir/template.akq.dummy ./${proj}.akq
  fi
fi

$editor ${proj}.akq 2>/dev/null

if [ $outfmt = "3dd" ] ; then
  if [ ! -f grib23ddat.inp ] ; then
    $grib23ddat -c
  fi
  $editor grib23ddat.inp  2>/dev/null
fi

#--------------------------------------------------------------------------
# 2.5 Cerco le informazioni relative all'area del dataset richiesto

# Area, proiezione, etc
line=`grep ^$dataset, $arkimet_aree`
if [ $? -eq 0 ] ; then
  dsarea=`echo $line | cut -d , -f 2 | tr '[:lower:]' '[:upper:]'`
  dsproj=`echo $line | cut -d , -f 3 | tr '[:lower:]' '[:upper:]'`
  fisiog=`echo $line | cut -d , -f 4 | tr '[:lower:]' '[:upper:]'`
  destag=`echo $line | cut -d , -f 5 | tr '[:lower:]' '[:upper:]'`
  gredit=`echo $line | cut -d , -f 6 | tr '[:lower:]' '[:upper:]'`
else
  dsarea="NIL"  
  dsproj="NIL"
  fisiog="N"
  destag="N"
  gredit=1
fi
if [ $gredit = 2 ] ; then
  ext="grib2"
else
  ext="grb"
fi

echo "Area: "$dsarea"; proiezione: "$dsproj"; parametri fisiografici: "\
$fisiog"; destag: "$destag"; edizione grib "$gredit

if [ $gredit = 2 -a  $outfmt = "afa" ] ; then
  echo "Output afa possibile sono con dataset GRIB1"
  exit
fi

# Estensione files di ouptut
# Numero di punti
if [ $dsproj = "GEO" -o $dsproj = "ROT" ] ; then
  dstr=`grep -i ^$dsarea $aree_geo`
  nxds=`echo $dstr | awk '{print $2}'`
  nyds=`echo $dstr | awk '{print $3}'`
elif [ $dsproj = "UTM" ] ; then
  dstr=`grep -i ^$dsarea $aree_utm`
  nxds=`echo $dstr | awk '{print $2}'`
  nyds=`echo $dstr | awk '{print $3}'`
else
  nxds=-999
  nyds=-999
fi

# Necessita' di destag
rq_destag="N"
if [ $destag = "Y" ] ; then
  echo "La query contiene dati su griglia staggerata (es. vento sui model levels)? (Y/N)"
  read yn
  if [ -z $yn ] ; then
    echo ""
  elif [ $yn = "y" -o $yn = "Y" ] ; then
    rq_destag="Y"
  fi
fi

# Necessita' di conversione al First Order Packing (transitoria!!)
rq_fop="N"
if [ $dataset = "lamaz" ] ; then
  echo "La query contiene dati con codifica al secondo ordine (cloud water/ice, TKE)? (Y/N)"
  read yn
  if [ -z $yn ] ; then
    echo ""
  elif [ $yn = "y" -o $yn = "Y" ] ; then
    rq_fop="Y"
  fi
fi

#--------------------------------------------------------------------------
# 2.6 Scelta della sottoarea

if [ $modif = "E" -a ! -f ${proj}.zoom ] ; then
  echo "File non trovato "${work_dir}/${proj}.zoom
  exit
elif [ $modif != "E" ] ; then
  echo ""
  echo "Gli estremi della sottoarea sono gia noti? (Y/N)"
  read yn
  if [ $yn = "n" -o $yn = "N" ] ; then
    rm -f tmp.zoom tmp.inp
    echo "Specificare i punti estremi della sottoarea richiesta:"
    echo "verra' selezionata la piu' piccola area rettangolare che li comprende tutti"
    $sel_punti -c
    echo $proj > tmp.inp
    tail -n +2 sel_punti.inp >> tmp.inp
    mv tmp.inp sel_punti.inp
    $editor sel_punti.inp
    $sel_punti
    mv ${proj}.zoom tmp.zoom
    echo "Indici estremi sottoarea (W S E N)" > ${proj}.zoom
    cat tmp.zoom >> ${proj}.zoom
  else
    if [ $modif = "R" -o \( $modif = "M" -a ! -f "$proj".zoom \) ] ; then
      echo "Indici estremi sottoarea (W S E N);  intero dominio: 0 0 0 0" > ${proj}.zoom
    fi
    $editor ${proj}.zoom 2>/dev/null
  fi
fi

zoom_str=`head -n 2 ${proj}.zoom | tail -n 1`

zW=`echo $zoom_str | awk '{print $1}'`
zS=`echo $zoom_str | awk '{print $2}'`
zE=`echo $zoom_str | awk '{print $3}'`
zN=`echo $zoom_str | awk '{print $4}'`

if [ -z $zW -o -z $zS -o -z $zE -o -z $zN ] ; then
  echo "Estremi sottoarea illegali "$zoom_str
  exit
elif [ $zW -eq 0 -a $zS -eq 0 -a $zE -eq 0 -a $zN -eq 0 ] ; then
  echo "Estremi sottoarea 0, estraggo l'intero dominio"
  rq_zoom="N"  
elif [ $zW -lt 1 -o $zS -lt 1 -o $zE -lt 1 -o $zN -lt 1 -o \
       $zE -lt $zW -o $zN -lt $zS ] ; then
  echo "Estremi sottoarea illegali "$zoom_str
  exit
else
  rq_zoom="Y"  
fi

if [ $nxds -gt 0 -a $nyds -gt 0 ] ; then
  if [ $zE -gt $nxds ] ; then
    echo "Estremo Est fuori dall'area "$dsarea", uso "$nxds
    zE=$nxds
  fi
  if [ $zN -gt $nyds ] ; then
    echo "Estremo Nord fuori dall'area "$dsarea", uso "$nyds
    zN=$nyds
  fi
fi

#--------------------------------------------------------------------------
# 2.7 Costruisco gli output statici in base ai dati disponibili (in formato
#     GRIB1 o AFA)

if [ $dsarea != "NIL" -a $fisiog = "Y" -a $modif != "E" ] ; then

# Campi di quote model layers e orografia
  for field in orog levels layers ; do
    rm -f ${field}.${ext}
    nf=`ls -1 ${fisiog_dir}/${dsarea}_${field}*.${ext} 2>/dev/null | wc -l`
    if [ $nf -eq 0 ] ; then
      echo "Dati fisiografici non disponibili per l'area "$dsarea": "$field
    elif [ $nf -eq 1 ] ; then
      ln -s ${fisiog_dir}/${dsarea}_${field}*.${ext} ./${field}.${ext}
    elif [ $nf -gt 1 ] ; then
      echo "Per questa area sono disponibili diversi files di dati fisiografici:"
      echo "selezionare quello da usare per il parametro "$field" (default: il primo)"
      ls -r1 ${fisiog_dir}/${dsarea}_${field}*.${ext}
      read file
      if [ -z $file ] ; then
        file=`ls -r1 ${fisiog_dir}/${dsarea}_${field}*.${ext} | head -n 1`
      fi
      ln -s $file  ./${field}.${ext}
    fi

    if [ $nf -ge 1 ] ; then
      if [ $rq_zoom = "Y" ] ; then
        vg6d_transform --trans-type=zoom --sub-type=index \
           --ix=$zW --iy=$zS --fx=$zE --fy=$zN \
           ${field}.${ext} ${proj}_${field}.${ext}          >> static.log 2>&1
      else
        cp ${field}.${ext} ${proj}_${field}.${ext}
      fi
  
      if [ $outfmt = "afa" ] ; then
        $grib2afa ${proj}_${field}.grb ${proj}_${field}.afa >> static.log 2>&1
      fi
      if [ $gredit -eq 2 ] ; then
        $ma_grib2_grib1 ${proj}_${field}.grib2 ${proj}_${field}.grb
      fi
    fi
  done

# Mappa orografia; campi delle coordinate di tutti i punti
  rm -f orog.png ${proj}_orog.png tmp.grb
  grib_set -s table2Version=2,centre=200,indicatorOfParameter=8 \
    ${proj}_orog.grb tmp.grb 

  $grb2grads -t 200 -b tmp                               >> static.log 2>&1
  grads -clb 'run '$plot_grid_orog' tmp '$dsproj
  mv orog.png ${proj}_orog.png

  $grib2latlon ${proj}_orog.grb ${proj}_latlon.grb       >> static.log 2>&1
  if [ $outfmt = "afa" ] ; then
    $grib2afa ${proj}_latlon.grb ${proj}_latlon.afa      >> static.log 2>&1
  fi

# Salvo i files statici nella dir di output
  for field in orog levels layers latlon ; do
    if [ $outfmt = "afa" ] ; then
      cp ${proj}_${field}.afa $out_dir
    else
      cp ${proj}_${field}.grb $out_dir
    fi
  done
fi

# Se richiesto, creo nella dir di output i files richiesti da grib23ddat
if [ $outfmt = "3dd" ] ; then
  for file in ${proj}_orog.grb ${proj}_layers.grb grib23ddat.inp ; do
    if [ ! -f $file ] ; then
      echo "Non trovo files di input richiesti dal formato 3d.dat "$file
      exit
    fi
  done
  cp grib23ddat.inp $out_dir
  cp ${proj}_orog.grb ${out_dir}/${proj}_static.grb 
  cat ${proj}_layers.grb >> ${out_dir}/${proj}_static.grb 
fi

###########################################################################
# 3) Estrazione

#--------------------------------------------------------------------------
# 3.1 Costruisco lo script per arki-xargs

cat <<EOF1 > xargs.ksh
set -x
cd ${work_dir}
rm -f tmp?.${ext} tmp3.afa
datac=\`grib_get -p dataDate \$1 | head -n 1\`
EOF1

if [ $rq_fop = "Y" ] ; then
  echo $grib_s2f \$1 tmp1.${ext}                               >> xargs.ksh
else
  echo ln -s \$1 tmp1.${ext}                                   >> xargs.ksh
fi

if [ $rq_zoom = "Y" ] ; then
  echo vg6d_subarea --trans-type=zoom --sub-type=index \
    --ix=$zW --iy=$zS --fx=$zE --fy=$zN tmp1.${ext} tmp2.${ext}>> xargs.ksh
else
  echo ln -s tmp1.${ext} tmp2.${ext}                           >> xargs.ksh
fi

if [ $rq_destag = "Y" ] ; then
  echo vg6d_transform --a-grid tmp2.${ext} tmp3.${ext}         >> xargs.ksh
else
  echo ln -s tmp2.${ext} tmp3.${ext}                           >> xargs.ksh
fi

echo cat tmp3.${ext} \>\> ${out_dir}/${proj}_\${datac}.${ext}  >> xargs.ksh

chmod +x xargs.ksh

#--------------------------------------------------------------------------
# 3.2 Costriusco il config

unset http_proxy
rm -f ${dataset}.conf
nsep=`echo $dataset | awk '{print gsub("%","###",$1)}'`
cntds=0
rm -f ${dataset}.conf
while [ $cntds -le $nsep ] ; do
  cntds=`expr $cntds + 1`
  dsc=`echo $dataset | cut -d % -f $cntds`
  arki-mergeconf ${akurl}/dataset/${dsc} >> ${dataset}.conf 2>/dev/null
  if [ $? -ne 0 ] ; then
    echo "Errore nell'accesso al dataset: ${akurl}/dataset/${dsc}"
    exit 3
  fi
done

#--------------------------------------------------------------------------
# 3.3 Elaboro il file con la query ($proj.akq)

# Filtro commenti e righe vuote
rm -f ${proj}.akq.proc ${prog}.query.*
grep -Ev "^ *$" ${proj}.akq | grep -v ^! > ${proj}.akq.proc

# Filtro i separatori e scrivo le query in files separati
cntq=1
while read line ; do
  echo $line | grep \# > /dev/null
  if [ $? -eq 0 ] ; then                 # trovato separatore: nuova query
    cntq=`expr $cntq + 1`
  else                                   # mantengo query corrente
    echo $line >> ${proj}.query.${cntq}
  fi
done < ${proj}.akq.proc

echo "Pronto per estrarre, lancero' "$cntq" query"

#--------------------------------------------------------------------------
# 3.4 Estrazione

cnt=1
while [ $cnt -le $cntq ] ; do
  echo "Elaboro query "$cnt"      "`date "+%Y%m%d %H:%M:%S"`
  arki-query --config=${dataset}.conf --file=${proj}.query.${cnt} --inline | \
    arki-xargs $xargs_opt xargs.ksh > arki.log.${cnt} 2>&1
  cnt=`expr $cnt + 1`
done

###########################################################################
# 4) Post-processing e archiviazione

cd $out_dir

# Eventuale sorting
if [ $sort = "Y" ] ; then
  echo "##### Inizio sorting dei files di output "`date "+%Y%m%d %H:%M:%S"`
  for file in `ls ${proj}_????????.${ext}` ; do
    mv $file tmp.${ext}
    arki-scan --data --sort=day:reftime,timerange,product,level \
      grib:tmp.${ext} > $file
    rm -f tmp.${ext}
  done
fi

# Eventuale conversione a formato 3D.DAT oppure AFA
if [ $outfmt = "3dd" ] ; then
  echo "##### Inizio conversione 3D.DAT dei files di output "`date "+%Y%m%d %H:%M:%S"`
  for file in `ls ${proj}_????????.${ext}` ; do
    file2=`basename $file $ext`"3dd"
    $grib23ddat ${proj}_static.grb $file $file2
    gzip $file2
  done
elif [ $outfmt = "afa" ] ; then
  echo "##### Inizio conversione AFA dei files di output "`date "+%Y%m%d %H:%M:%S"`
  for file in `ls ${proj}_????????.${ext}` ; do
    file2=`basename $file $ext`"afa"
    $grib2afa $file $file2
    gzip $file2
  done
fi

echo "##### Elaborazioni terminate "`date "+%Y%m%d %H:%M:%S"`

# Eventuale archiviazione dei files statici
cd $work_dir
echo "Archivio i files statici? (Y/N)"
read yn
if [ $yn = "y" -o $yn = "Y" ] ; then

  if [ -d $arc_dir ] ; then
    echo "Il progetto "$proj" e gia stato archviato: "
    echo "(C)ancello files esistenti e sovrascrivo, (S)ovrascrivo, (T)ermino"
    read yn
    if [ $yn = "C" -o $yn = "c" ] ; then
      rm -f $arc_dir/*
    elif [ $yn != "S" -a $yn != "s" ] ; then
      exit
    fi
  else
    mkdir $arc_dir
  fi

  for file in ${proj}.akq ${proj}.zoom ${proj}_orog.grb ${proj}_orog.png \
    ${proj}_layers.grb ${proj}_levels.grb ${proj}_latlon.grb ; do
    if [ -s $file ] ; then
      cp -v $file $arc_dir
    else
      echo "file "$file" non prodotto!"
    fi
  done
fi

# Se ho creato un nuovo progetto, modifico l'elenco dei progetti
if [ $new = "Y" ] ; then
  $editor $doc_file
fi

exit
