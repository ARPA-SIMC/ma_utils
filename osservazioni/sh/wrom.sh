#!/bin/bash
#set -x

#-------------------------------------------------------------------------------
# Script che fa le windrose (con windrose.ksh) e
# le mette su una mappa (con overlay.gs)
#
#                                     Versione 2.2.3, Giovanni&Enrico 28/01/2020
#-------------------------------------------------------------------------------

# help
function write_help
{
 echo " uso: wrom.ksh [-b] [-m] [-s] [-h] [-os opts1] [-om opts2]"
 echo ""
 echo "      -b   batch, vuole wrom.lst ed eventualmente wrtmp.gs nella cartella di lavoro"
 echo "           wrom.lst: una riga per stazione, 3 campi: nome_file lon lat (separti da spazi)"
 echo "      -m   fa solo la mappa, non anche le w.rose separate"
 echo "      -s   input da seriet (default e' estra_orari)"
 echo "      -h   scrive questo help"
 echo "      opts1 opzioni da passare a windrose.sh (tra apici)"
 echo "      opts2 opzioni da passare a wrom.gs (tra apici)"
 echo ""
 return 
}

# Assegno l'ambiente ma_utils
if [ -z $MA_UTILS_SVN ] ; then
  windrose=/usr/libexec/ma_utils/windrose.sh
  wrom_gs=/usr/libexec/ma_utils/wrom.gs
  overlay_gs=/usr/libexec/ma_utils/overlay.gs
  draw_orogy_gs=/usr/libexec/ma_utils/draw_orog.gs
  geo_file=/usr/share/ma_utils/orog_eur_20km
else 
  echo "(ak_seriet.ksh) Eseguibili ma_utils: copia di lavoro in "$MA_UTILS_SVN
  windrose=${MA_UTILS_SVN}/osservazioni/sh/windrose.sh
  wrom_gs=${MA_UTILS_SVN}/osservazioni/sh/wrom.gs
  overlay_gs=${MA_UTILS_SVN}/util/grads/sh/overlay.gs
  draw_orog_gs=${MA_UTILS_SVN}/util/grads/sh/draw_orog.gs
  geo_file=${MA_UTILS_SVN}/data/orog_eur_20km
fi

cp $overlay_gs $draw_orog_gs $geo_file.* ./

# opzioni
interactive=1
separate=1
type="-o"
opts1=""
opts2="-a 2"
while [ $# -ge 1 ] ; do
  if [ $1 = "-h" ] ; then
    write_help
    exit
  elif [ $1 = "-b" ] ; then
    interactive=0
    shift
  elif [ $1 = "-m" ] ; then
    separate=0
    shift
  elif [ $1 = "-s" ] ; then
    type="-s"
    shift
  elif [ $1 = "-os" ] ; then
    shift
    opts1=$1
    shift
  elif [ $1 = "-om" ] ; then
    shift
    opts2=$1
    shift
  fi
done

# fa la lista
echo "wrom.sh: inizio elaborazioni"
if [ $interactive -eq 1 ] ;then
  if [ ! -s wrom.lst ] ; then
  cat > wrom.lst <<EOF1
nomefile1  lon1 lat1
nomefile2  lon2 lat2
nomefile3  lon3 lat3
...        ...  ... 
EOF1
  fi
  echo "Edita la lista dei punti: (nomefile lon lat)"
  emacs wrom.lst
else
  if [ ! -s wrom.lst ] ; then
    echo "Cannot find file wrom.lst"
    exit
  fi
fi 

# fa le windrose
rm -f wrose_*.png windrose.log
j=0
legend=0
if [ $separate -eq 1 ] ; then mkdir -p wrs ; fi
while read line ; do
  j=`expr $j + 1`
  nwr=$j
  file=`echo $line | awk '{print $1}'`
  nrows=`wc $file | awk '{print $1}'`
  nrows=`expr $nrows - 3`
  lim=`expr $nrows / 3`
  dum=$(basename $file)         # tolgo path
  fileout=${dum%.*}".png"    # sostituisco estensione con .png
  echo "wrom.sh: plot windrose "$file
# $windrose $type -m -L minimal -T bars -c transparent -p light -l $lim $file 
  $windrose $type -m -L minimal -T bars -c transparent -p light $opts1 $file \
    >> windrose.log 2>&1
  mv -f wrose_*.png $j.png   
  if [ $separate -eq 1 ] ; then
    $windrose $type -m -L full -T bars -c white -p classic $opts1 $file \
      >> windrose.log 2>&1
    mv -f wrose_*.png wrs/
  fi
  if [ $legend -eq 0 -a -s $j.png ] ; then
    $windrose $type -L onlylegend -c transparent -p light $opts1 $file \
      >> windrose.log 2>&1
    mv -f wrose_*.png legend.png
    legend=1
  fi
done < wrom.lst

# mette le windrose su una mappa
echo "wrom.sh: metto le wroses su mappa"
if [ $interactive -eq 1 ] ;then
  if [ ! -s wrtmp.gs ] ; then
    cat > wrtmp.gs <<EOF
'define_colors'
'set line 87'
'draw_shape regita'
'set line 0'
'draw_marks nord'
EOF
  fi
  echo "Modifica i dettagli della mappa:"
  emacs wrtmp.gs
fi
set -x
grads -clb $wrom_gs' '"$opts2"
set +x

# elimina le windrose "di lavoro"
rm legend.png
j=0
while [ $j -lt $nwr ] ; do
  j=`expr $j + 1`
#  rm $j.png   
done
