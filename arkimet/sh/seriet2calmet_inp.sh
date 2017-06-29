#/bin/ksh
###########################################################################
# Legge una serie di files seriet e scrive l'input per un run Calmet.
# Uso: seriet2calmet_inp.ksh PROG [-outver X.Y]
#
# Note:
# - I dati in input devono essere stati prodotti con un'estrazione dal 
#   dateset LAMAZ secondo il template: template.akq.lamaz.calpuff
# - Lavora nella dir. corrente
# - Deve essere presente il file $PROG_fisiog.csv
#
#
#                                          Versione 1.1.1 Enrico 06/04/2017
###########################################################################
#set -x
set +e

function intfill
{
#----------------------------------------------------------------------
# USO: intfill str_in len_req
# Ritorna la variabile d'ambiente str_out, che contiene len_req
# caratteri, di cui i primi sono "0" e gli ulitmi conicidono con str_in
#----------------------------------------------------------------------

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

#-------------------------------------------------------------------------
# Scrive a schermo l'help della procedura
function write_help
{
  echo "Uso: seriet2calmet_inp.ksh PROG [-outver X.Y]"
  echo "Riscrive come input per Calmet i risultati di un'estrazione seriet per Capluff"
  echo "Lavora nella dir. corrente, dove devono esitere i files $PROG_punti_???.txt e $PROG_fisiog.csv"
  echo "-outver: scrive i files UP e SURF nel formato X.Y "
  echo "         Valori gestiti: 2.1 (calmet 6.3); default calmet <= 5.5"
  return
}

###########################################################################
# 1) Elaborazioni preliminari

#--------------------------------------------------------------------------
# 1.1 Parametri da riga comando

out_ver="1"
out_ver_opt=""
if [ $# -eq 0 ] ; then
  write_help
  exit
fi

while [ $# -ge 1 ] ; do
  if [ $1 = '-h' ] ; then
    write_help
    exit
  elif [ $1 = "-outver" ] ; then
    shift
    out_ver=$1
    out_ver_opt="-outver "$1
    shift
  else
    proj=$1
    shift
  fi
done

#--------------------------------------------------------------------------
# Path, utility, costanti

# parametri derivati
filefis=${proj}_fisiog.csv

# utility
if [ -z $MA_UTILS_SVN ] ; then
  echo "Ambiente ma_utils: produzione"
  MA_UTILS_BIN=/usr/libexec/ma_utils
  tfill_seriet=${MA_UTILS_BIN}/tfill_seriet.exe
  proc_prf=${MA_UTILS_BIN}/proc_seriet_prf.exe
  proc_surf=${MA_UTILS_BIN}/proc_seriet_surf.exe
  crea_surf=${MA_UTILS_BIN}/crea_surf_dat.exe
else 
  echo "Ambiente ma_utils: sviluppo"
  tfill_seriet=${MA_UTILS_SVN}/arkimet/src/tfill_seriet.exe
  proc_prf=${MA_UTILS_SVN}/calmet/src/proc_seriet_prf.exe
  proc_surf=${MA_UTILS_SVN}/calmet/src/proc_seriet_surf.exe
  crea_surf=${MA_UTILS_SVN}/calmet/src/crea_surf_dat.exe
fi

#--------------------------------------------------------------------------
# Controlli

if [ ! -s $filefis ] ; then
  echo "File non trovato: "$filefis
  exit 1
fi

nf=`ls ${proj}_punti_???.txt  2>/dev/null | wc -l`
if [ $nf -eq 0 ] ; then
  echo "Non ho trovato nessun file di input, termino"
  exit 1
else
  echo "Elaboro "$nf" files"
fi

###########################################################################
# 2) Elaborazioni 

rm -f up*.dat *.st2 surf.dat surf_req.dat date_calmet.inp
echo "" > surf_req.dat
cnt=0
for file in `ls ${proj}_punti_???.txt` ; do
  cnt=`expr $cnt + 1`
  fid=`echo ${file##*_} | cut -d . -f 1`
  rm -f tmp.txt tmp.head tmp.fis

# Header, label, orografia
  head -n 1 $file > tmp.head
  head -n `expr $cnt + 1` $filefis | tail -n 1 > tmp.fis
  lab_ser=`cat tmp.head | cut -d ! -f 2 | sed 's/^ *//g;s/ *$//g'| sed 's/ /_/g'`
  lab_fis=`cat tmp.fis | cut -d , -f 3 | sed 's/^ *//g;s/ *$//g' | sed 's/ /_/g'`
  if [ $lab_ser != $lab_fis ] ; then
    echo "Warning: labels inconsistenti nel punto "$cnt
    echo $lab_ser" in "$file
    echo $lab_fis" in "$filefis
  fi
  orog=`cat tmp.fis | cut -d , -f 4 `

# Elaboro i dati
  $tfill_seriet $file tmp.txt
  $proc_prf tmp.txt up${cnt}.dat $fid $orog $out_ver_opt | tail -n 1
  $proc_surf tmp.txt "000_00"${fid}".st2"

# Incremento il file surf_req.dat
  echo "00"${fid} >> surf_req.dat

done

# Costruisco il file date_calmet.inp
rec1=`head -n 7 ${proj}_punti_001.txt | tail -n 1`
datah1=`echo $rec1 | awk '{print substr($1,7,4) substr($1,4,2) substr($1,1,2) $2}'`
rec2=`tail -n 1 ${proj}_punti_001.txt`
datah2=`echo $rec2 | awk '{print substr($1,7,4) substr($1,4,2) substr($1,1,2) $2}'`
cat <<EOF > date_calmet.inp



$datah1
$datah2
EOF

# Costruisco surf.dat
$crea_surf date_calmet.inp surf_req.dat $out_ver_opt
