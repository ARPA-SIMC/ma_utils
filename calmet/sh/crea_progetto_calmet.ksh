#!/bin/ksh
###########################################################################
# crea_progetto_calmet.ksh
#
# Procedura interattiva per costurire un nuovo progetto per run Calmet
# Puo' essere lanciata per creare da zero un nuovo progetto o per 
# modificare un progetto esistente
#
#                                                 V5.1.0, Enrico 24/09/2013
###########################################################################

#set -x

###########################################################################
# 1) Elaborazioni preliminari

# 1.1) Path, utility, files di appoggio

# Path di calmet 
root_dir=${HOME_MINGUZZI}/calmet/progetti      # root progetti calmet
run_root=${SCRATCH_MINGUZZI}/calmet/           # root dir di lavoro calmet
dat_dir=${HOME_MINGUZZI}/calmet/dat/           # templates Calmet
exe_dir=${HOME_MINGUZZI}/calmet/bin/           # utilities Calmet

# Utility
if [ -z $MA_UTILS_SVN ] ; then
  sel_punti_grid=/usr/libexec/ma_utils/sel_punti_grid.exe
  proc_fisiog=/usr/libexec/ma_utils/proc_fisiog.exe
  llgrib=/usr/libexec/ma_utils/llgrib.exe
  zoom_grib=/usr/libexec/ma_utils/zoom_grib.exe
else
  echo "(crea_progetto_calmet.ksh) Eseguibili ma_utils: copia di lavoro in "$MA_UTILS_SVN
  sel_punti_grid=${MA_UTILS_SVN}/arkimet/src/sel_punti_grid.exe
  proc_fisiog=${MA_UTILS_SVN}/fisiog/src/proc_fisiog.exe
  llgrib=${MA_UTILS_SVN}/util/grib/src/llgrib.exe
  zoom_grib=${MA_UTILS_SVN}/util/grib/src/zoom_grib.exe
fi
grb2grads=${HOME_MINGUZZI}/util/grads/bin/grb2grads.ksh
editor=emacs

# Files di appoggio (costanti)
if [ -z $MA_UTILS_DAT ] ; then
  export MA_UTILS_DAT=/usr/share/ma_utils
else
  echo "(crea_progetto_calmet.ksh) Path tabelle ma_utils: "$MA_UTILS_DAT
fi
aree_utm=${MA_UTILS_DAT}/aree_utm.dat

ds_cori2000=${HOME_MINGUZZI}/fisiog/dataset/lu_corine2000/LAND_CLC00ita
ds_bats=${HOME_MINGUZZI}/fisiog/dataset/lu_bats/gbats2_0ll.img
ds_usgs=${HOME_MINGUZZI}/fisiog/dataset/lu_usgs/gusgs2_0ll.img
orog_lamaz=${HOME_MINGUZZI}/util/grib/lm_ope/LAMAZ_orog_20120606.grb
lay_lamaz=${HOME_MINGUZZI}/util/grib/lm_ope/LAMAZ_layers_20120606.grb

# Altre costanti
lm_area=lamaz
area="null"

# 1.2) Gestione parametri (nome progetto)
if [ $# -eq 0 ] ; then
  echo "Uso: crea_progetto_calmet.ksh PROGETTO"
  echo "Progetti gia fatti:"
  ls -1rt $root_dir
  exit
fi
if [ $1 = "-h" ] ; then
  echo "Uso: crea_progetto_calmet.ksh PROGETTO"
  echo "Progetti gia fatti:"
  ls $root_dir
  exit
fi

proj=$1
proj_dir=${root_dir}/${proj}

###########################################################################
# 2) Esecuzione: costurisco i files richiesti dal pre-processing Calmet

#--------------------------------------------------------------------------
# 2.0) costruzione directories
echo "Prgetto "$proj
echo "Costruisco le directories? (Y/N) [progetto, esecuzione, ...]"
read yn
if [ $yn = "y" -o $yn = "Y" ] ; then

  if [ ! -d $run_root/$proj ] ; then
    mkdir $run_root/$proj
  fi
  cd $run_root/$proj
  for subdir in run log out oss ; do
    if [ ! -d $subdir ] ; then
    mkdir $subdir
    fi
  done

  if [ ! -d $proj_dir ] ; then
    mkdir $proj_dir
  fi

  echo "Il progetto deve essere scrivibile dal gruppo ambiente? (Y/N)"
  read yn
  if [ $yn = "y" -o $yn = "Y" ] ; then
    chgrp -R ambiente $run_root/$proj
    chmod g+s $run_root/$proj $run_root/$proj/*
    chgrp ambiente $proj_dir
    chmod g+s $proj_dir
  fi

else

  if [ ! -d $proj_dir ] ; then
    echo "directory "$proj_dir" non torvata!"
    exit
  fi

fi
cd $proj_dir

#--------------------------------------------------------------------------
# 2.1) Costruzione elenco stazioni (in quota e sup.) e parametri richiesti 

echo "Costruisco liste stazioni in quota e al suolo? (Y/N)"
echo "NB:"
echo "- i files srq* prq_surf verranno creati automaticamente e corretti manualmente"
echo "- rispondere N se Calmet sara' alimentato solo da dati LAMA."
read yn
if [ $yn = "y" -o $yn = "Y" ] ; then

  new="Y"
  if [ -f srq_temp_${proj}.lst -o -f srq_surf_${proj}.lst -o \
       -f prq_surf_${proj}.lst ] ; then
    echo "files .lst esistenti; sovrascrivo? (Y/N)"  
    read yn
    if [ $yn != "y" -a $yn != "Y" ] ; then
      new="N"
    fi
  fi

  if [ $new = "Y" ] ; then
    if [ -f pre_calmet.inp ] ; then
      area=`tail -n +9 pre_calmet.inp | head -n +1 | cut -d \  -f 1`
      echo "area: "$area
    else
      echo "Immetti nome area per il run"
      read area
    fi

    ${exe_dir}/sel_staz_calmet.exe -c
    $editor sel_staz_calmet.inp

#   lista stazioni superficiali da escludere
    excl=`tail -n +6 sel_staz_calmet.inp | head -n +1 | cut -d \  -f 1`
    if [ $excl -eq 1 ] ; then
      file2="staz_excl.lst"
      new2="Y"
      if [ -f $file2 ] ; then
        echo $file2" esiste; sovrascrivo? (Y/N)"  
        read yn
        if [ $yn != "y" -a $yn != "Y" ] ; then
          new2="N"
        fi
      fi

      if [ $new2 = "Y" ] ; then
        echo "Escludo stazioni doppie e off"
        cp ${dat_dir}/staz_excl.lst.doppie ./$file2
        cat ${dat_dir}/staz_excl.lst.off >> ./$file2
      fi

      $editor $file2
    fi

#   Selezione stazioni (automatica)
    ${exe_dir}/sel_staz_calmet.exe $area
    if [ $area != $proj ] ; then
      mv srq_temp_${area}.lst srq_temp_${proj}.lst
      mv srq_surf_${area}.lst srq_surf_${proj}.lst
    fi
  fi

# Correzione manuale dei files prodotti
  echo "NB: la 1a staz. TEMP dovrebbe essere quella maggiormente rappresentativa"
  $editor srq_temp_${proj}.lst
  $editor srq_surf_${proj}.lst

  ${exe_dir}/crea_prq_surf.exe srq_surf_${proj}.lst prq_surf_${proj}.lst
  $editor prq_surf_${proj}.lst

fi

#--------------------------------------------------------------------------
# 2.2) Costruzione elenco punti LAMAZ

echo "Costruisco i files di appoggio per usare dati LAMAZ come osservazioni? (Y/N)"
read yn_LM
if [ $yn_LM = "y" -o $yn_LM = "Y" ] ; then

# Trovo i punti LAMAZ utili per il domino calmet
file=${lm_area}_${proj}.pts.csv
new="Y"
if [ -f $file ] ; then
  echo $file" esiste; sovrascrivo? (Y/N)"  
  read yn
  if [ $yn != "y" -a $yn != "Y" ] ; then
    new="N"
  fi
fi

if [ $new = "Y" ] ; then
  if [ $area = "null" ] ; then
    echo "Immetti nome area Calmet (deve essere inclusa in aree_utm.dat)"
    read area
    area_desc=`grep -i $area $aree_utm`
    if [ $? -ne 0 ] ; then
      echo "Area "$area" non trovata in aree_utm.dat"
      exit
    fi
  fi

  echo "Numero massimo di punti LAMA da usare (cosigliato 50, max 150)"
  read mxp

  $sel_punti_grid $lm_area geo $area utm -buffer 1. -mxp $mxp
  mv sel_punti_grid.pts.csv $file
fi

# Verifico che i punti non siano troppi
idum=`wc -l $file | awk '{print $1}'`
np=`expr $idum - 1`
echo "Selezionati "$np" punti"

# Aggiungo a srq_surf e srq_temp i punti LAMAZ
if [ ! -f srq_surf_${proj}.lst ] ; then
  echo "NtUsrid Nome                      Lat     Long Quot" > srq_surf_${proj}.lst
else
  echo "file "srq_surf_${proj}.lst" esiste, (A)ppendo o (S)ovrascrivo?"
  read yn
  if [ $yn = "S" -o $yn = "s" ] ; then
    rm srq_surf_${proj}.lst
    echo "NtUsrid Nome                      Lat     Long Quot" > srq_surf_${proj}.lst
  fi
fi
if [ ! -f srq_temp_${proj}.lst ] ; then
  echo "NtUsrid Nome                      Lat     Long Quot" > srq_temp_${proj}.lst
else
  echo "file "srq_temp_${proj}.lst" esiste, (A)ppendo o (S)ovrascrivo?"
  read yn
  if [ $yn = "S" -o $yn = "s" ] ; then
    rm srq_temp_${proj}.lst
    echo "NtUsrid Nome                      Lat     Long Quot" > srq_temp_${proj}.lst
  fi
fi

tail -n +2 $file > ${file}.tmp
while read line ; do
  lon=`echo $line | cut -d \, -f 1`
  lat=`echo $line | cut -d \, -f 2`
  ii=`echo $line  | cut -d \, -f 4`
  jj=`echo $line  | cut -d \, -f 5`
  kk=`echo $line  | cut -d \, -f 6`
  orog=`$llgrib $orog_lamaz -pi $kk 2>/dev/null | tail -n 1 | awk '{print $13}'`
  if [ $ii -lt 10 ] ; then
    idi="00"$ii
  elif [ $ii -lt 100 ] ; then
    idi="0"$ii
  else
    idi=$ii
  fi
  if [ $jj -lt 10 ] ; then
    idj="00"$jj
  elif [ $jj -lt 100 ] ; then
    idj="0"$jj
  else
    idj=$jj
  fi
  if [ $kk -lt 10 ] ; then
    idk="0000"$kk  
  elif [ $kk -lt 100 ] ; then
    idk="000"$kk  
  elif [ $kk -lt 1000 ] ; then
    idk="00"$kk  
  elif [ $kk -lt 10000 ] ; then
    idk="0"$kk  
  else
    idk=$kk  
  fi

  echo "00"${idk}" Punto_LM_"${idi}"_"${idj}"       "$lat"    "$lon" "$orog >> srq_surf_${proj}.lst
  echo "00"${idk}" Punto_LM_"${idi}"_"${idj}"       "$lat"    "$lon" "$orog >> srq_temp_${proj}.lst
done < ${file}.tmp

# Copio l'elenco ordinato dei prametri LAMAZ da estrarre per costruire le 
# pseudo-osservaazioni superficiali 
cp ${dat_dir}/cachi.sup.varliv.csv ./sup.varliv.csv

fi

#--------------------------------------------------------------------------
# 2.3) Costruzione files per uso LAMAZ com campo di background

echo "Costruisco i files di appoggio per usare dati LAMAZ come background? (Y/N)"
read yn_LM2
if [ $yn_LM2 = "y" -o $yn_LM2 = "Y" ] ; then

  new="Y"
  if [ -f grib23ddat_${proj}.inp.40lay -o -f grib23ddat_${proj}.inp.35lay -o \
       -f static_${proj}.grb -o -f ${proj}.zoom ] ; then
    echo "files esistenti; sovrascrivo? (Y/N)"  
    read yn
    if [ $yn != "y" -a $yn != "Y" ] ; then
      new="N"
    fi
  fi

  if [ $new = "Y" ] ; then
    cp ${dat_dir}/grib23ddat.inp.40lay ./grib23ddat_${proj}.inp.40lay 
    cp ${dat_dir}/grib23ddat.inp.35lay ./grib23ddat_${proj}.inp.35lay 

#   Seleziono sottoarea LAMAZ (costruisco file ${proj}.zoom)
    if [ $area = "null" ] ; then
      echo "Immetti nome area Calmet (deve essere inclusa in aree_utm.dat)"
      read area
      area_desc=`grep -i $area $aree_utm`
      if [ $? -ne 0 ] ; then
        echo "Area "$area" non trovata in aree_utm.dat"
        exit
      fi
    fi

    $sel_punti_grid $lm_area geo $area utm -buffer 1.
    mv sel_punti_grid.zoom ${proj}.zoom
    zoom_str=`head -n 2 ${proj}.zoom | tail -n 1`
    
#   Controlli sulla sottoarea selezionata
    zW=`echo $zoom_str | awk '{print $1}'`
    zS=`echo $zoom_str | awk '{print $2}'`
    zE=`echo $zoom_str | awk '{print $3}'`
    zN=`echo $zoom_str | awk '{print $4}'`
    if [ -z $zW -o -z $zS -o -z $zE -o -z $zN ] ; then
      echo "Estremi sottoarea illegali "$zoom_str
      exit
    elif [ $zW -lt 1 -o $zS -lt 1 -o $zE -lt 1 -o $zN -lt 1 -o \
           $zE -lt $zW -o $zN -lt $zS ] ; then
      echo "Estremi sottoarea illegali "$zoom_str
      exit
    fi
    if [ $zE -gt 169 -o $zN -gt 181 ] ; then
      echo "Estremo richiesti fuori dall'area LAMAZ"
      exit
    fi
    echo "Estremi sottoarea LAMAZ: "$zoom_str

#   Costruisco file static (orografia e quote model layers)
    rm tmp1.grb tmp2.grb 
    $zoom_grib $orog_lamaz tmp1.grb -pts $zoom_str > /dev/null
    $zoom_grib $lay_lamaz tmp2.grb -pts $zoom_str > /dev/null
    cp tmp1.grb static_${proj}.grb
    cat tmp2.grb >> static_${proj}.grb
  fi
fi

#--------------------------------------------------------------------------
# 2.4) Costruzione pre_calmet.inp

file="pre_calmet.inp"
echo "Costruisco "$file"? (Y/N) [opzioni pre-processing, gestione run]"
read yn
if [ $yn = "y" -o $yn = "Y" ] ; then

  new="Y"
  if [ -f $file ] ; then
    echo $file" esiste; sovrascrivo? (Y/N)"  
    read yn
    if [ $yn != "y" -a $yn != "Y" ] ; then
      new="N"
    fi
  fi

  if [ $new = "Y" ] ; then
    if [ $yn_LM2 = "y" -o $yn_LM2 = "Y" -o $yn_LM = "y" -o $yn_LM = "Y" ] ; then
      cp ${dat_dir}/pre_calmet.inp.template.lama ./${file}
    else
      cp ${dat_dir}/pre_calmet.inp.template.oss ./${file}
    fi
  fi

  $editor $file

fi

#--------------------------------------------------------------------------
# 2.5) Costruzione calmet_PROJ.inp

file=calmet_"$proj".inp
echo "Costruisco "$file"? (Y/N) [opzioni per run calmet]"
read yn
if [ $yn = "y" -o $yn = "Y" ] ; then

  new="Y"
  if [ -f $file ] ; then
    echo $file" esiste; sovrascrivo? (Y/N)"  
    read yn
    if [ $yn != "y" -a $yn != "Y" ] ; then
      new="N"
    fi
  fi

  if [ $new = "Y" ] ; then
    if [ $yn_LM = "y" -o $yn_LM = "Y" ] ; then
      cp ${dat_dir}/calmet.inp.template.lama ./${file}
    else
      cp ${dat_dir}/calmet.inp.template.oss ./${file}
    fi
  fi

  $editor $file

fi

#--------------------------------------------------------------------------
# 2.6) Costruzione GEO.DAT

file=geo_"$proj"_yea.dat
echo "Costruisco "$file"? (Y/N) [orografia e parametri fisiografici]"
read yn
if [ $yn = "y" -o $yn = "Y" ] ; then

  new="Y"
  if [ -f $file ] ; then
    echo $file" esiste; sovrascrivo? (Y/N)"  
    read yn
    if [ $yn != "y" -a $yn != "Y" ] ; then
      new="N"
    fi
  fi

  if [ $new = "Y" ] ; then
    echo "Immetti nome area (deve essere inclusa in aree_utm.dat)"
    read area
    area_desc=`grep -i $area $aree_utm`
    if [ $? -ne 0 ] ; then
      echo "Area "$area" non trovata in aree_utm.dat"
      exit
    fi
    nx=`echo $area_desc | awk '{print $2}'`
    ny=`echo $area_desc | awk '{print $3}'`
    x1=`echo $area_desc | awk '{print $4}'`
    y1=`echo $area_desc | awk '{print $5}'`
    x2=`echo $area_desc | awk '{print $6}'`
    y2=`echo $area_desc | awk '{print $7}'`
    
    rm -f proc_fisiog.inp tmp.inp
    $proc_fisiog -c
    tail -n +13 proc_fisiog.inp > tmp.inp

    cat <<EOF > proc_fisiog.inp
!Parametri griglia richiesta (UTM)
UTM
$nx
$ny
$x1
$y1
$x2
$y2
32            ! UTMZ
0.            ! Xrot
0.            ! Yrot
$area
EOF
    cat tmp.inp >> proc_fisiog.inp
    rm tmp.inp
    $editor proc_fisiog.inp

#   Se necessario, un-zippo i dataset richiesti
    grep 'Corine 2000' proc_fisiog.inp | head -n 1 | grep ^1 > /dev/null
    if [ $? -eq 0 ] ; then
      if [ ! -f $ds_cori2000 ] ; then
        echo "Scompatto "$ds_cori2000", ricordarsi di ricompattarlo!! "
        gunzip ${ds_cori2000}.gz
      fi
    fi

    grep 'BATS' proc_fisiog.inp | head -n 1 | grep ^1 > /dev/null
    if [ $? -eq 0 ] ; then
      if [ ! -f $ds_bats ] ; then
        echo "Scompatto "$ds_bats", ricordarsi di ricompattarlo!! "
        gunzip ${ds_bats}.gz
      fi
    fi

    grep 'USGS' proc_fisiog.inp | head -n 1 | grep ^1 > /dev/null
    if [ $? -eq 0 ] ; then
      if [ ! -f $ds_usgs ] ; then
        echo "Scompatto "$ds_usgs", ricordarsi di ricompattarlo!! "
        gunzip ${ds_usgs}.gz
      fi
    fi

#   Costruisco geo.dat
    $proc_fisiog
    mv geo_${area}_yea.grb geo_${proj}_yea.grb
    mv geo_${area}_yea.dat geo_${proj}_yea.dat

#   Costruisco le mappe
    cp ${HOME_MINGUZZI}/fisiog/bin/plot_fisiog_calmet.gs ./
    $grb2grads -t 200 -b geo_${proj}_yea
    grads -clb 'run plot_fisiog_calmet 'geo_${proj}_yea' '$proj

  fi
fi


exit
