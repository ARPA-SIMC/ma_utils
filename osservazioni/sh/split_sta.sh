#/bin/ksh
#
# Dato un file .sta con serie temporale prodotto da stat_orari, lo divide in 4
# files in formato estra_qaria giornaliero (leggibili dallo stresso stat_orari)
#
#set -x

# Parametri da riga comandi
if [ $# -eq 0 ] ; then
  echo "Uso: split_sta.ksh filein"
  exit 1
fi
if [ $1 = "-h" ] ; then
  echo "Uso: split_sta.ksh filein"
  exit 1
fi

filein=$1

# Trovo le posizioni di inizio dei segmenti
nseg=`grep -E "Medie|Massimi|Minimi|Dati_validi|Rank" $filein | wc -l`
nl=`wc -l $filein | awk '{print $1}'`

p1=`grep "Medie" $filein --line-number | cut -d : -f 1`
p2=`grep Massimi $filein --line-number | cut -d : -f 1`
p3=`grep Minimi $filein --line-number | cut -d : -f 1`
p4=`grep Dati_validi $filein --line-number | cut -d : -f 1`
if [ $nseg = 5 ] ; then
  p5=`grep Rank $filein --line-number | cut -d : -f 1`
  extra=`grep Rank $filein | cut -d \  -f 1`
else
  p5=$nl
fi

# Medie
li=$p1
lf=`expr $p2 - 3`
head -n $lf $filein | tail -n +$li > ${filein}.ave

# Massimi
li=$p2
lf=`expr $p3 - 3`
head -n $lf $filein | tail -n +$li > ${filein}.max

# Minimi
li=$p3
lf=`expr $p4 - 3`
head -n $lf $filein | tail -n +$li > ${filein}.min

# Dati validi
li=$p4
lf=`expr $p5 - 2`
head -n $lf $filein | tail -n +$li > ${filein}.nok

# Extra
if [ $nseg = 5 ] ; then
  li=$p5
  lf=`expr $nl - 2`
  head -n $lf $filein | tail -n +$li > ${filein}.${extra}
fi

exit
