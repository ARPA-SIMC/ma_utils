#! /bin/ksh
#--------------------------------------------------------------------------
# Procedura per importare in GRADS un GRIB generico
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
#                               Versione 6.10.0, Enrico & Jhonny 20/07/2015
#--------------------------------------------------------------------------
#set -x

#--------------------------------------------------------------------------
# 0) Funzione che scrive a schermo l'help della procedura
function write_help
{
#       123456789012345678901234567890123456789012345678901234567890123456789012345
  echo "Uso: grb2grads file [-h][-i][-b] [-forc] [-tsz N][-tst STEP] [-utmHR]"
  echo "     [-scale FNORM OFFSET] [-t N] [-lsort][-lsup/-lgrp]"
  echo "     [-tsp][-tspc][-w][-tke] [-cal/-lm]"
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
  echo "Opzioni per escludere alcune variabili (rinominano il file .grb)"
  echo "  -tsp   sposta in 2 files a parte le variabili delle tab. 201 e 203 [LM]"
  echo "  -tspc  sposta in un file a parte le variabili della tabella 199 [Chimere]"
  echo "  -w     sposta in un file a parte la variabile 40 (velocita verticale)"
  echo "  -tke   sposta in un file a parte la variabile 152 (TKE)"
  echo "  -uv    sposta in un file a parte il vento orizzontale LM"
  echo ""
  echo "Opzioni complessive:"
  echo "  -lev   equivale a: -lsort -lgrp"
  echo "  -cal   equivale a: -t 200 -lsort -lgrp"
  echo "  -lm    equivale a: -t 2 -tsp"
  echo ""
  echo "Per usare versioni di lavoro di programmi e tabelle, esportare le variabili:"
  echo "          MA_UTILS_DAT, MA_UTILS_SVN, LIBSIM_DATA, DBA_TABLES, LIBSIM_SVN"
#       123456789012345678901234567890123456789012345678901234567890123456789012345
  return
}

#---------------------------------------------------------------------------
# 1) Preliminari

# 1.1) Assegno l'ambiente

cong=/usr/bin/cong
grib2ctl=/usr/local/bin/grib2ctl.pl

if [ -z $MA_UTILS_SVN ] ; then
  interp_grib=/usr/libexec/ma_utils/interp_grib.exe
  grib_utm_scale=/usr/libexec/ma_utils/grib_utm_scale.exe
  adjust_ctl=/usr/libexec/ma_utils/adjust_ctl.exe
else 
  echo "(grb2grads.ksh) Eseguibili ma_utils: copia di lavoro in "$MA_UTILS_SVN
  interp_grib=${MA_UTILS_SVN}/util/grib/src/interp_grib.exe
  grib_utm_scale=${MA_UTILS_SVN}/util/grads/src/grib_utm_scale.exe
  adjust_ctl=${MA_UTILS_SVN}/util/grads/src/adjust_ctl.exe
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
tabsplit="NO"
tabsplitc="NO"
noww="NO"
notke="NO"
nouv="NO"

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

  elif [ $1 = '-tsp' ] ; then
    tabsplit="YES"
    shift
  elif [ $1 = '-tspc' ] ; then
    tabsplitc="YES"
    shift
  elif [ $1 = '-w' ] ; then
    noww="YES"
    shift
  elif [ $1 = '-tke' ] ; then
    notke="YES"
    shift
  elif [ $1 = '-uv' ] ; then
    nouv="YES"
    shift

  elif [ $1 = '-lev' ] ; then
    sort_lev="YES"
    adj_lev="2"
    shift
  elif [ $1 = '-cal' ] ; then
    adj_var="200"
    sort_lev="YES"
    adj_lev="2"
    shift
  elif [ $1 = '-lm' ] ; then
    adj_var="002"
    tabsplit="YES"
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

if [ $noww = "YES" -o $notke = "YES" -o $tabsplit = "YES" \
     -o $tabsplitc = "YES" ] ; then
  echo "Salvo il file originale in "$prog.$ext".org"
  cp ${prog}.${ext} ${prog}.${ext}.org
fi

# 2.1) Metto i parametri delle tabelle 201 e 203 (LAMI) in files a parte
if [ $tabsplit = "YES" ] ; then
  rm -f %tmp.grb%
  echo "***** Scorporo eventuali parametri delle tabelle 201 e 203 *****" 
  $cong ${prog}.${ext} %tmp.grb% \
    +"setkey var:-1,2,-1"
  $cong ${prog}.${ext} ${prog}_tab201.${ext} \
    +"setkey var:-1,201,-1"
  $cong ${prog}.${ext} ${prog}_tab203.${ext} \
    +"setkey var:-1,203,-1"
  mv %tmp.grb% ${prog}.${ext}
fi

# 2.2) Metto i parametri della tabella 199 (Chimere aero) in un file a parte
if [ $tabsplitc = "YES" ] ; then
  rm -f %tmp.grb%
  echo "***** Scorporo eventuali parametri della tabella 199 *****" 
  $cong ${prog}.${ext} %tmp.grb% \
    +"setkey var:-1,200,-1"
  $cong ${prog}.${ext} ${prog}_tab199.${ext} \
    +"setkey var:-1,199,-1"
  mv %tmp.grb% ${prog}.${ext}
fi

# 2.3) Metto la velocita' verticale in un file a parte
if [ $noww = "YES" ] ; then
  rm -f %tmp.grb%
  echo "***** Scorporo la velocita verticale *****" 
  $cong ${prog}.${ext} ${prog}_ww.${ext} \
    +"setkey var:-1,-1,40"
  $cong ${prog}.${ext} %tmp.grb% \
    -"setkey var:-1,-1,40"
  mv %tmp.grb% ${prog}.${ext}
fi

# 2.4) Metto la TKE in un file a parte
if [ $notke = "YES" ] ; then
  rm -f %tmp.grb%
  echo "***** Scorporo la TKE *****" 
  $cong ${prog}.${ext} ${prog}_tke.${ext} \
    +"setkey var:-1,-1,152"
  $cong ${prog}.${ext} %tmp.grb% \
    -"setkey var:-1,-1,152"
  mv %tmp.grb% ${prog}.${ext}
fi

# 2.5) Metto il vento orizzontale in un file a parte
if [ $nouv = "YES" ] ; then
  echo "***** Scorporo il vento orizzontale *****" 

  rm -f %tmp.grb%
  echo "+setkey var:-1,2,33 liv:-1,-1,-1" > %tmp.lst%
  echo "+setkey var:-1,2,34 liv:-1,-1,-1" >> %tmp.lst%
  $cong ${prog}.${ext} ${prog}_uv.${ext} -f%tmp.lst%

  echo "-setkey var:-1,2,33 liv:-1,-1,-1" > %tmp.lst%
  echo "-setkey var:-1,2,34 liv:-1,-1,-1" >> %tmp.lst%
  $cong ${prog}.${ext} %tmp.grb% -f%tmp.lst%
  mv %tmp.grb% ${prog}.${ext}
  rm %tmp.lst%
fi

# 2.6) Interpolo su griglia regolare
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

# 2.7) Riscalo (ie. divido per 100) estremi e passo delle griglie UTM
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



