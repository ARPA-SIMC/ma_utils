#/bin/bash
#-------------------------------------------------------------------------------
# Interroga arkioss e ritorna la lista delle stazioni che misurano uno specifico 
# parametro.
#
# TODO:
# - aggiugere gestione di poligoni specifici per la ricerca delle stazioni: EMR,
#   EMR_pinaura, BPA_pianura
#
# NOTE:
# - Causa bug arkimet, le query con intervallo di date ritornano meno dati di 
#   quelle sull'intero dataset (non e' chiaro quale sia quella giusta...)
#
#                                              Versione 3.0.1, Enrico 02/09/2019
#-------------------------------------------------------------------------------
#set -x

#===============================================================================
function write_help
{
#       123456789012345678901234567890123456789012345678901234567890123456789012345678
  echo "Interroga arkioss e ritorna la lista delle stazioni che misurano uno specifico"
  echo "parametro"
  echo "Uso: get_var_staz.sh id_var [-z xmin ymin xmax ymax] [-d data_ini data_fin]"
  echo "     [-url URL]"
  echo ""
  echo "id_var: id oracle del parametro (vedi files param_arkioss.csv e "
  echo "        param_shortnames.csv, in /usr/share/ma_utils)"
  echo "xmin ymin xmax ymax: estremi dell'aera geografica di ricerca (def: ovunque)"
  echo "data_ini, data_fin: intervallo di date in cui cercare (def: qualsiasi data)"
  echo "URL: indirizzo del server arkioss (def: http://arkioss.metarpa:8090)"
}

#===============================================================================
function parse_area
#
# Riformatta (da yaml a csv) la stringa "Area" estratta da arkioss e relativa
# a una stazione.
#
# Note:
# - Funzione usata anche da get_staz_var.sh
# - Se il record di anagrafica e' incompleto, i campi mancanti sono messi a ""

{
line2=$(echo $line | sed 's/"//g')
id_staz=""
lon=""
lat=""
quo=""
nome=""

# id_staz (Oracle)
pp=$(echo "" | awk '{print index("'"${line2}"'","(")}')
if [ $pp -gt 0 ] ; then
  id_staz_row=$(echo $line2 | cut -c $[pp+1]- | cut -d , -f 1 | cut -d \) -f 1)
  intfill $id_staz_row 5
  id_staz="H"${str_out}
fi

# Coordinate (in archvio in gradi*10^5)
pp=$(echo "" | awk '{print index("'"${line2}"'","lon=")}')
if [ $pp -gt 0 ] ; then
  lon_raw=$(echo $line2 | cut -c $[pp+4]- | cut -d , -f 1 | cut -d \) -f 1)
  intfill $lon_raw 7
  lon=$(echo $str_out | awk '{print substr($1,1,2) "." substr($1,3,5)}')
fi

pp=$(echo "" | awk '{print index("'"${line2}"'","lat=")}')
if [ $pp -gt 0 ] ; then
  lat_raw=$(echo $line2 | cut -c $[pp+4]- | cut -d , -f 1 | cut -d \) -f 1)
  intfill $lat_raw 7
  lat=$(echo $str_out | awk '{print substr($1,1,2) "." substr($1,3,5)}')
fi

# Cerco la stazione in anag_meteozen.dat; verifico che le coordinate 
# corrispondano; leggo quota e nome stazione.
line3=$(grep ^:${id_staz_row}, anag_meteozen.dat)
if [ $? -eq 0 ] ; then
  line4=$(echo $line3 | sed 's/"//g')

# Verifico coordinate
  pp=$(echo "" | awk '{print index("'"${line4}"'","lon:")}')
  if [ $pp -gt 0 ] ; then
    lon_mz=$(echo $line4 | cut -c $[pp+4]- | cut -d , -f 1 | cut -d \) -f 1)
  fi

  pp=$(echo "" | awk '{print index("'"${line4}"'","lat:")}')
  if [ $pp -gt 0 ] ; then
    lat_mz=$(echo $line4 | cut -c $[pp+4]- | cut -d , -f 1 | cut -d \) -f 1)
  fi
  
# Cerco nome e quota
  if [ $lon_mz = $lon_raw -a $lat_mz = $lat_raw ] ; then
    pp=$(echo "" | awk '{print index("'"${line4}"'","name:")}')
    if [ $pp -gt 0 ] ; then
      nome=$(echo $line4 | cut -c $[pp+5]- | cut -d , -f 1 | cut -d \) -f 1)
    fi
  
    pp=$(echo "" | awk '{print index("'"${line4}"'","height:")}')
    if [ $pp -gt 0 ] ; then
      quo=$(echo $line4 | cut -c $[pp+7]- | cut -d , -f 1 | cut -d \) -f 1)
    fi
    mz_ok=$[$mz_ok+1]
    
  else
    echo "Stazione $id_staz $net: coordinate inconsistenti in anag_meteozen.dat"
    echo "  attese "$lon_raw $lat_row" trovate "$lon_mz $lat_mz
  fi

else
  echo "Stazione $id_staz $net non trovata in anag_meteozen.dat"
fi

str_area_csv=${id_staz},${lat},${lon},${quo},${net},${nome}

}

#===============================================================================
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


################################################################################
# 1) Preliminari

# Parametri da riga comando
akurl="http://arkioss.metarpa:8090"
id_var=0
data_restrict="N"
area_restrict="N"

mand_par=0
req_par=1
if [ $# -eq 0 ] ; then
  write_help
  exit 1
fi
while [ $# -ge 1 ] ; do
  if [ $1 = -h ] ; then
    write_help
    exit 1
  elif [ $1 = "-z" ] ; then
    area_restrict="Y"
    shift
    xmin=$1
    shift
    ymin=$1
    shift
    xmax=$1
    shift
    ymax=$1
    shift
  elif [ $1 = "-d" ] ; then
    data_restrict="Y"
    shift
    data1=$1
    shift
    data2=$1
    shift
  elif [ $1 = "-url" ] ; then
    shift
    akurl=$1
    shift
  else
    if [ $mand_par -eq 0 ] ; then
      id_var=$1
    fi
    mand_par=$[mand_par+1]
    shift
  fi
done

if [ $mand_par != $req_par ] ; then
  write_help
  exit 1
fi

fileout=anag_var${id_var}.csv

# Costruisco la query arkioss
rm -f gsv.query
echo "product: VM2,${id_var}" > gsv.query

if [ $data_restrict = "N" ] ; then
  echo "Cerco dati con reftime qualsiasi"
else
  echo "Cerco dati con reftime tra "$data1" e "$data2
  echo "reftime: >="$(date -d $data1 +%Y-%m-%d)", <="$(date -d $data2 +%Y-%m-%d) \
    >> gsv.query
fi

if [ $area_restrict = "N" ] ; then
  echo "Cerco dati con coordinate qualsiasi"
else
  echo "Cerco dati nel rettangolo "$xmin" "$ymin" "$xmax" "$ymax
  echo "area: bbox coveredby POLYGON(($xmin $ymin, $xmax $ymin, $xmax $ymax, $xmin $ymax, $xmin $ymin))" \
    >> gsv.query
fi

#===============================================================================
# 2) Elaborazioni

#-------------------------------------------------------------------------------
# 2.0 Scarico da meteozen l'anagrafica completa

rm -f tmp.dat anag_meteozen.dat
# Alternative URL: data format is different!!
# curl http://meteozen.metarpa/rt_data/stations > tmp.dat
curl -u "ugo:Ul1ss&" "http://meteozen.metarpa/simcstations/api/v1/stations" > tmp.dat
cat tmp.dat | sed 's/{"id"/\n/g' | cut -d , -f 1,3,4,5,6,7 > anag_meteozen.dat
ns=$(wc -l anag_meteozen.dat |awk '{print $1}')
echo "Scaricata anagrafica meteozen, stazioni trovate "$ns

#-------------------------------------------------------------------------------
# 2.1 Trovo l'elenco dei dataset presenti su arkioss
rm -f tmp.ds ds.lst
unset http_proxy
curl $akurl 2>/dev/null > tmp.ds
grep /dataset/ tmp.ds | cut -d / -f 3 | cut -d \' -f 1 | grep -v error > ds.lst

#-------------------------------------------------------------------------------
# 2.2 Per ciasun dataset, trovo le stazioni che misurano il parametro richiesto

rm -f $fileout
echo "id_staz,lat,lon,quota,dataset,nome" > $fileout

mz_ok=0
cnt_net=0
while read net ; do
  rm -f tmp1.yml tmp2.yml
  arki-query --summary --yaml --file=gsv.query ${akurl}/dataset/${net} > tmp1.yml
  nc=$(wc -m tmp1.yml | awk '{print $1}')
  if [ $nc -gt 1 ] ; then
    echo "Trovati dati nel dataset "$net
    cnt_net=$[$cnt_net+1]
    grep Area tmp1.yml | sort -u > tmp2.yml
    while read line ; do
      parse_area $line
      echo $str_area_csv >> $fileout
    done < tmp2.yml
  fi
done < ds.lst

nl=$(wc -l $fileout | awk '{print $1}')
echo "Totale stazioni trovate: "$[$nl-1]" (in anagrafica meteozen "$mz_ok"), in "$cnt_net" reti"
