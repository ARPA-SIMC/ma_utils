#!/bin/ksh
#--------------------------------------------------------------------------
# Script per generare rose dei venti
# a partire da dati prodotti con estra_orari o trasp_seriet
#
# (usa R, funziona su annapurna)
#
#                                       V2.1.0 Johnny & Enrico, 07/06/2018
#-------------------------------------------------------------------------
function write_help
{
#       123456789012345678901234567890123456789012345678901234567890123456789012345
 echo " uso: windrose.ksh [-s] [-less] [-t title] [-l lim]   "
 echo "                   [-c col] [-L layout] [-T type]     "
 echo "                   [-p palette] [-M months] [-H hours]"
 echo "                   [-lev level] [-f fileout] filein   "
 echo ""
 echo "      filein = file ASCII nel formato estra_orari"
 echo "      -s       per usare i dati in formato seriet"
 echo "      -less    per usare 8 settori anziche' 16"
 echo "      title  = eventuale titolo del plot (senza spazi)"
 echo "      lim    = limite superiore dell'asse radiale"
 echo "      col    = colore dello sfondo (default white)"
 echo "      layout = layout della windrose (full, light, "
 echo "               minimal, onlylegend)"
 echo "               (default = full)"
 echo "      type   = tipo di rosa (lines,bars)"
 echo "               (default = bars)"
 echo "      palette= colori per la rosa (classic,light)"
 echo "               (default = classic)"
 echo "      months = mesi da considerare, separati da virgole,"
 echo "               senza spazi (p.es. 1,2,12)"
 echo "      hours  = ore da considerare, separate da virgole,"
 echo "               senza spazi (p.es. 0,1,2,3,4,5,22,23)"
 echo "      level  = livello del modello (default 10)"
 echo "      fileout= nome del fine di output"
 echo ""
 return 
}
#-------------------------------------------------------------------------

# 1.1.1 Assegno l'ambiente ma_utils
if [ -z $MA_UTILS_SVN ] ; then
  wroseR=/usr/libexec/ma_utils/windrose.r
  polarplotR=/usr/libexec/ma_utils/polarplot.r
else
  wroseR=${MA_UTILS_SVN}/osservazioni/sh/windrose.r
  polarplotR=${MA_UTILS_SVN}/osservazioni/sh/polarplot.r
fi
export polarplotR

# gestisce l'help
if [ $# -eq 0 ] ; then
 write_help
 exit
fi

#gestisce le opzioni
seriet="n"
nsett="s16"
title="default"
color="white"
layout="full"
wrtype="bars"
palette="classic"
months="1,2,3,4,5,6,7,8,9,10,11,12"
hours="0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23"
level="10"
limit=-9999
fileout="nil"
while [ $# -ge 1 ] ; do
    if [ $1 = '-h' ] ; then
	write_help
	exit
    elif [ $1 = '-s' ] ; then
        seriet="y"
    elif [ $1 = '-l' ] ; then
        shift
        limit=$1
    elif [ $1 = '-t' ] ; then
        shift
        title=$1
    elif [ $1 = '-c' ] ; then
        shift
        color=$1
    elif [ $1 = '-L' ] ; then
        shift
        layout=$1
    elif [ $1 = '-T' ] ; then
        shift
        wrtype=$1
    elif [ $1 = '-M' ] ; then
        shift
        months=$1
    elif [ $1 = '-H' ] ; then
        shift
        hours=$1
    elif [ $1 = '-p' ] ; then
        shift
        palette=$1
    elif [ $1 = '-less' ] ; then
        nsett="s8"
    elif [ $1 = '-m' ] ; then      # per compatibilita' con vers.1.2
        nsett="s16"
    elif [ $1 = '-lev' ] ; then
        shift
        level=$1
    elif [ $1 = '-f' ] ; then
        shift
        fileout=$1
    else
	filein=$1
    fi
    shift
done

# controlla che ci sia il file di input
if [ -s $filein ] ; then
  echo "leggo file "$filein
else
  echo "ATTENZIONE!"
  echo "non trovo "$filein
  exit
fi

# controlla che ci siano direzione e intensita' 
if [ $seriet = "n" ] ; then
 nv=`head -n 3 $filein | tail -n 1 | wc -c`
 nv=`expr $nv - 14`
 nv=`expr $nv / 11`
 echo $filein" contiene "$nv" variabili"
 iv=0
 fld=0
 flf=0
 while [ $iv -lt $nv ] ; do
  iv=` expr $iv + 1 `
  dum=` expr $iv - 1 `
  dum=` expr $dum \* 11 `
  range=`expr 19 + $dum`"-"`expr 20 + $dum`
  nomv=`head -n 3 $filein | tail -n 1 | cut -c $range`
  if [ $nomv ] ; then
   if [ $nomv = "FF" ] ; then
    flf=`expr $flf + 1`
   fi
   if [ $nomv = "DD" ] ; then
    fld=`expr $fld + 1`
   fi
  fi
 done
 if [ $flf -eq 1 ] ; then
  if [ $fld -eq 1 ] ; then
   check="ok"
  else
   check="ko"
   echo "non trovo la direzione!"
  fi
 else
  check="ko"
  echo "non trovo l'intensita'!"
 fi
elif [ $seriet = "y" ] ; then
 nv=`head -n 6 $filein | tail -n 1 | wc -c`
 nv=`expr $nv - 18`
 nv=`expr $nv / 11`
 echo $filein" contiene "$nv" variabili"
 iv=0
 fld=0
 flf=0
 while [ $iv -lt $nv ] ; do
  iv=`expr $iv + 1`
  dum=`expr $iv - 1`
  dum=`expr $dum \* 11`
  range=`expr 21 + $dum`"-"`expr 23 + $dum`
  nomv=`head -n 6 $filein | tail -n 1 | cut -c $range`
  if [ $nomv ] ; then
   if [ $nomv = "Mod" ] ; then
    flf=`expr $flf + 1`
   fi
   if [ $nomv = "Dir" ] ; then
    fld=`expr $fld + 1`
   fi
  fi
 done
 if [ $flf -eq 1 ] ; then
  if [ $fld -eq 1 ] ; then
   check="ok"
  elif [ $fld -eq 0 ] ; then
   check="ko"
   echo "non trovo la direzione!"
  elif [ $fld -gt 1 ] ; then
   check="ok"
   echo "gestisco il profilo"
  fi
 elif [ $flf -eq 0 ] ; then
  check="ko"
  echo "non trovo l'intensita'!"
 elif [ $flf -gt 1 ] ; then
  check="ok"
  echo "gestisco il profilo"
 fi
fi


#-------------------------------------------------------------------------
# Costruisco il file windrose.inp e chiamo R per disegnare la rosa dei 20
# - nome file dati
# - codice punto (per nome file output)
# - nome punto (per legenda)
# - codice formato file (ossmeteo o seriet)

if [ $check = "ok" ] ; then
 if [ $seriet = "n" ] ; then
  nbars=`echo $filein | awk '{print gsub("/","#",$1)}'`
  nbarsp1=`expr $nbars + 1`
  cod="stz_"`echo $filein | cut -d / -f $nbarsp1 | cut -c 1-7`

  if [ $title = "default" ] ; then
   nome="station_"`echo $filein | cut -d / -f $nbarsp1 | cut -c 1-7`
  else
   nome=$title
  fi
  echo $filein > windrose.inp
  echo $cod >> windrose.inp
  echo $nome >> windrose.inp
  echo "ossmeteo" >> windrose.inp

 else
  id_pt=`head -n 1 $filein | cut -d "!" -f 2 | sed 's/ /_/g'`
  model=`head -n 3 $filein | tail -n 1 | cut -c 18-27 | sed 's/_*$//g' | sed 's/\%//g'`
  if [ $title = "default" ] ; then
   nome=$model"_"`echo $id_pt `
  else
   nome=$title
  fi
  cod=$model"_"`echo $id_pt | awk '{print substr($1,1,8)}'`
  echo $filein > windrose.inp
  echo $cod >> windrose.inp
  echo $nome >> windrose.inp
  echo "seriet" >> windrose.inp

 fi
 echo $nsett >> windrose.inp
 echo $limit >> windrose.inp
 echo $color >> windrose.inp
 echo $layout >> windrose.inp
 echo $wrtype >> windrose.inp
 echo $palette >> windrose.inp
 echo $months >> windrose.inp
 echo $hours >> windrose.inp
 echo $level >> windrose.inp
 echo $fileout >> windrose.inp

 R --vanilla --slave < $wroseR || exit 2

else
 echo "Errore wrose: "$filein" deve contenere una direzione e una velocita'"
 exit 1
fi
