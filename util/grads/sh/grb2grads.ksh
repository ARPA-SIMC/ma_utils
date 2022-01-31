#! /bin/ksh
#--------------------------------------------------------------------------
# Procedura per importare in GRADS un GRIB1 SIMC
#
# In tutti i casi, importa in grads un solo file, eventualmente salvando
# in files separati i campi che non possono coesistere nello stesso dataset
# GRADS.
#
# Operazioni compiute:
# - eventuale scorporo dei parametri di tabella 201 & 203 e della vel.vert.
# - eventuale interpolazione su griglia geografica regolare
# - costruzione .ctl ed eventuale aggiustmento secondo la tabella richiesta
# - costruzione .idx ed eventuale apertura finestra interattiva GRADS
#
#
# Updates: 
# modifcare le opzioni che escludono variabili sull'esempio di -scale, ie.
# non copiando il file originale, ma importandone uno con estenzione diversa
#
#                               Versione 7.0.0, Enrico & Jhonny 31/01/2022
#--------------------------------------------------------------------------
#set -x

#--------------------------------------------------------------------------
# 0) Funzione che scrive a schermo l'help della procedura
function write_help
{
#       123456789012345678901234567890123456789012345678901234567890123456789012345
  echo "Uso: grb2grads file [-h][-i][-b] [-forc] [-tsz N][-tst STEP] [-utmHR]"
  echo "     [-scale FNORM OFFSET] [-t N] [-lsort][-lsup/-lgrp]"
  echo ""
  echo "Opzioni generali"
  echo "  file   input file (grib), senza l'estensione .grb o .grib"
  echo "  -h     visualizza questo help"
  echo "  -i     interpola i grib su griglia regolare"
  echo "  -b     non apre la finestra interattiva grads"
  echo "  -forc  importa grib relativi a previsioni"
  echo "  -tsz N forza a N il numero di istanti nel .ctl (record tdef)"
  echo "  -tst STEP forza a STEP (stringa) il passo temporale nel .ctl (record tdef)"
  echo "  -utmHR toglie 4000000 alle coordiante Y (utile per campi UTM grib1 ad "
  echo "         alta risoluzione (rinomina il file .grb)"
  echo "  -scale moltiplica per FNORM le coordinate della griglia e somma OFFSET"
  echo "         alle coordinate Y (rinomina il file .grb)"
  echo ""
  echo "Opzioni per modificare i nomi di livelli e varibili"
  echo "  -t NNN aggiusta i nomi delle variabili usando la tabella NNN"
  echo "  -lsort ordina i livelli in senso crescente (Chimere, Calmet)"
  echo "  -lsup  mantiene i nomi delle var.sup. distinti da quelle in quota [LM]"
  echo "  -lgrp  raggruppa le var.sup. a quelle in quota [Calmet]"
  echo ""
  echo "Per usare versioni di lavoro di programmi e tabelle, esportare le variabili:"
  echo "          MA_UTILS_DAT, MA_UTILS_SVN, LIBSIM_DATA, DBA_TABLES, LIBSIM_SVN"
#       123456789012345678901234567890123456789012345678901234567890123456789012345
  return
}

#---------------------------------------------------------------------------
# 1) Preliminari

# 1.1) Assegno l'ambiente

if [ -z $MA_UTILS_SVN ] ; then
  interp_grib=/usr/libexec/ma_utils/interp_grib.exe
  grib_utm_scale=/usr/libexec/ma_utils/grib_utm_scale.exe
  adjust_ctl=/usr/libexec/ma_utils/adjust_ctl.exe
  grib2ctl=/usr/libexec/ma_utils/grib2ctl.pl
else 
  echo "(grb2grads.ksh) Eseguibili ma_utils: copia di lavoro in "$MA_UTILS_SVN
  interp_grib=${MA_UTILS_SVN}/util/grib/src/interp_grib.exe
  grib_utm_scale=${MA_UTILS_SVN}/util/grads/src/grib_utm_scale.exe
  adjust_ctl=${MA_UTILS_SVN}/util/grads/src/adjust_ctl.exe
  grib2ctl=${MA_UTILS_SVN}/util/grads/sh/grib2ctl.pl
fi

if [ -z $MA_UTILS_DAT ] ; then
  gribtab_dir=/usr/share/ma_utils
else
  echo "(grb2grads.ksh) Tabelle ma_utils: copia di lavoro in "$MA_UTILS_DAT
  gribtab_dir=$MA_UTILS_DAT
fi

# 1.2) Gestione parametri
interp="NO"
batch="NO"
forc="NO"
tsz="NO"
tst="NO"
utm_scale="NO"
adj_var="NO"
adj_lev="0"
sort_lev="NO"

if [ $# -eq 0 ] ; then
  write_help
  exit
fi

while [ $# -ge 1 ] ; do

  if [ $1 = '-h' ] ; then
    write_help
    exit
  elif [ $1 = '-i' ] ; then
    interp="YES"
    shift
  elif [ $1 = '-b' ] ; then
    batch="YES"
    shift
  elif [ $1 = '-forc' ] ; then
    forc="YES"
    shift
  elif [ $1 = '-tsz' ] ; then
    tsz="YES"
    shift
    ntimes=$1
    shift
  elif [ $1 = '-tst' ] ; then
    tst="YES"
    shift
    tstep=$1
    shift
  elif [ $1 = '-utmHR' ] ; then
    utm_scale="YES"
    fnorm="1"
    offset="-4000000"
    shift
  elif [ $1 = '-scale' ] ; then
    utm_scale="YES"
    shift
    fnorm=$1
    shift
    offset=$1
    shift
  elif [ $1 = '-t' ] ; then
    shift
    if [ $1 -lt 10 ] ; then
      adj_var="00"$1
    elif [ $1 -lt 100 ] ; then
      adj_var="0"$1
    else
      adj_var=$1
    fi
    shift
  elif [ $1 = '-lsort' ] ; then
    sort_lev="YES"
    shift
  elif [ $1 = '-lgrp' ] ; then
    adj_lev="2"
    shift
  elif [ $1 = '-lsup' ] ; then
    adj_lev="1"
    shift

  else
    prog=$1
    shift

  fi
done

# 1.3) Controllo se esiste il file grib e se le opzioni sono legali
if [ -f $prog.grb ] ; then
  ext='grb'
elif [ -f $prog.grib ] ; then
  ext='grib'
else
  echo "Errore, file ${prog}.grb non trovato"
  exit
fi

#---------------------------------------------------------------------------
# 2) Modifiche al grib (se richieste)

# 2.1) Interpolo su griglia regolare
if [ $interp = "YES" ] ; then
  echo "***** Interpolo su griglia regolare *****" 
  rm -f ${prog}.${ext}.int 
  $interp_grib ${prog}.${ext} ${prog}.${ext}.int > /dev/null
  ext=${ext}.int

  if [ ! -f ${prog}.${ext} ] ; then
    echo "errore, grib non interpolati"
    exit
  fi
fi

# 2.2) Riscalo (ie. divido per 100) estremi e passo delle griglie UTM
if [ $utm_scale = "YES" ] ; then
  echo "***** Riscalo estremi griglia *****" 
  rm -f ${prog}.${ext}.scaled
  $grib_utm_scale ${prog}.${ext} ${prog}.${ext}.scaled \
    $fnorm $offset
  ext=${ext}.scaled
fi

#---------------------------------------------------------------------------
# 3) Importazione GRADS

# 3.1) Creo il .ctl in modo automatico
echo "***** Creo il .ctl *****" 
rm -f ${prog}.ctl
if [ $forc = "YES" ] ; then
  perl $grib2ctl ${prog}.${ext} ${prog}.idx -verf > ${prog}.ctl
else
  perl $grib2ctl ${prog}.${ext} ${prog}.idx > ${prog}.ctl
fi

if [ ! -s ${prog}.ctl ] ; then
  echo "Errore, ctl non creato"
  exit
fi

# 3.2) Se richiesto, aggiusto il .ctl
adjp=""
adj="NO"

if [ $tsz != "NO" ] ; then
  adjp=${adjp}" "-tsz" "${ntimes}
  adj="YES"
fi

if [ $tst != "NO" ] ; then
  adjp=${adjp}" "-tst" "${tstep}
  adj="YES"
fi

if [ $adj_var != "NO" ] ; then
  adjp=${adjp}" "${gribtab_dir}/tabella_${adj_var}.txt
  adj="YES"
fi

if [ $sort_lev = "YES" ] ; then
  adjp=${adjp}" -lsort"
  adj="YES"
fi

if [ $adj_lev = "1" ] ; then
  adjp=${adjp}" -lsup"
  adj="YES"
elif [ $adj_lev = "2" ] ; then
  adjp=${adjp}" -lgrp"
  adj="YES"
fi

if [ $adj = "YES" ] ; then
  echo "***** Aggiusto il .ctl *****"
  cp ${prog}.ctl ${prog}.ctl.org
  mv ${prog}.ctl ${prog}.ctl.tmp
  $adjust_ctl ${prog}.ctl.tmp ${prog}.ctl ${adjp}
  rm -f ${prog}.ctl.tmp
fi

# 3.3) Creo il .idx
echo "***** Creo il .idx *****"
gribmap -i ${prog}.ctl

# 3.4) Se richiesto, lancio grads
if [ $batch = "NO" ] ; then 
  grads -lc 'open '$prog'.ctl'
fi

exit



