#!/bin/ksh
###########################################################################
# estra_qaria.ksh
#
# Script per l'estrazione di dati dal DB di qualita' dell'aria
# Appartiene alla catena estra_qaria
#
#                                        V3.1.0, Johnny & Enrico 15/07/2015
###########################################################################
# set -x 

#--------------------------------------------------------------------------
# 0) Funzioni 

# 0.1) Help
function write_help
{
  echo "Uso: estra_qaria.ksh [-h] [-c/-l/-b/] [-nograds]"
  echo "  -h  scrive questo help"
  echo "  -c  crea estra_qaria.inp di esempio"
  echo "  -l  crea estra_qaria.inp di esempio con la lista di tutte le stazioni"
  echo "  -b  lancia l'estrazione usando le opzioni di estra_qaria.inp"
  echo "  -nograds: costruisce solo gli output .asc"
  echo ""
  echo "NB: gli orari in ouptut sono LST; i dati orari sono concentrazioni medie"
  echo "    nell'ora precedetne"
}

#--------------------------------------------------------------------------
# 1) Elaborazioni preliminari

# 1.1 Path e utility
if [ -z $MA_UTILS_SVN ] ; then
  scrive_dati_sql=/usr/libexec/ma_utils/scrive_dati_sql.exe
  ordina_dati_tmp=/usr/libexec/ma_utils/ordina_dati_tmp.exe
  scrive_stzqa_sql=/usr/libexec/ma_utils/scrive_stzqa_sql.exe
else 
  echo "(ak_seriet.ksh) Eseguibili ma_utils: copia di lavoro in "$MA_UTILS_SVN
  scrive_dati_sql=${MA_UTILS_SVN}/osservazioni/src/scrive_dati_sql.exe
  ordina_dati_tmp=${MA_UTILS_SVN}/osservazioni/src/ordina_dati_tmp.exe
  scrive_stzqa_sql=${MA_UTILS_SVN}/osservazioni/src/scrive_stzqa_sql.exe
fi

##dbqa='/usr/bin/sqlplus guest_smr/smr@qa-sia.arpa.emr.net'
dbqa="/usr/bin/sqlplus AA_USER/USER_AA@RMQAC_NEWARIA"
stnmap=/usr/bin/stnmap

# 1.2) Gestione parametri
fileopt="NO"
estra="NO"
tab="QA_DATI"
lgrads="YES"
if [ $# -eq 0 ] ; then
    write_help
    exit
fi
while [ $# -ge 1 ] ; do
    if [ $1 = '-h' ] ; then
	write_help
	exit
    elif [ $1 = '-c' ] ; then
	estra="NO"
	fileopt="MIN"
    elif [ $1 = '-l' ] ; then
	estra="NO"
	fileopt="MAX"
    elif [ $1 = '-b' ] ; then
	estra="YES"
        tab="QA_DATI"
    elif [ $1 = '-r' ] ; then
	estra="YES"
        tab="QA_DATI"
    elif [ $1 = '-nograds' ] ; then
        lgrads="NO"
    fi
    shift
done

# 1.3) Scrittura file opzioni
if [ $fileopt != 'NO'  ] ; then
    cat <<EOF1 >  estra_qaria.inp
20100218        ! data iniziale (aaaammgg)
20100221        ! data finale (aaaammgg)
PM10            ! inquinante (disponibili PM10,O3,NO2,NOX,NO,SO2,CO,BZ,PM2.5)
EOF1
    
    if [ $fileopt = 'MIN' ] ; then
	cat <<EOF2 >> estra_qaria.inp
3               ! n.o di stazioni, 0 = tutte
2000003         ! codici staz. (uno per riga; cancellare queste righe se 
3000004         !   n.staz. = 0; usare opzione -l per visualizzare la 
5000013         !   lista completa delle stazioni)
EOF2
	
# se si vuole che il file di opzioni contenga
# la lista di tutte le stazioni, costruisce la 
# query, ...
    elif [ $fileopt = 'MAX' ] ; then
	cat <<EOF21 >  lista_stzqa.sql
set lines 70 
set pages 10000
col NOME   format A21
col COMUNE format A23
col PR     format A8
spool lista_stzqa.tmp;
SELECT
  ID_STAZIONE   , 
  '    ! '||substr(max(NOME_STAZIONE),1,15) as NOME,      
  '(com.'||substr(max(COMUNE       ),1,15) as COMUNE,  
  'prov.'||substr(max(PROVINCIA    ),1,2)||')' as PR   
FROM
 ANG_CONFIG_SENSORI staz
WHERE
 ((staz.FLG_MOBILE=0) OR (staz.FLG_MOBILE IS NULL))
GROUP BY
 ID_STAZIONE    
HAVING 
  sum(decode(ID_PARAMETRO, 5,1,0)) +
  sum(decode(ID_PARAMETRO, 7,1,0)) +
  sum(decode(ID_PARAMETRO, 8,1,0)) +
  sum(decode(ID_PARAMETRO,38,1,0)) +
  sum(decode(ID_PARAMETRO, 9,1,0)) +
  sum(decode(ID_PARAMETRO, 1,1,0)) +
  sum(decode(ID_PARAMETRO,10,1,0)) +
  sum(decode(ID_PARAMETRO,20,1,0))
 > 0
ORDER BY 
  PR,
  COMUNE
;
spool off;
EOF21

# ...la lancia...
	echo "creo la lista delle stazioni disponibili..."
	$dbqa <<EOF22 1>/dev/null
start lista_stzqa
EOF22

# ...e inserisce la lista nel file di opzioni
	nlines=`wc lista_stzqa.tmp | awk '{print $1}'`
	echo ` expr $nlines - 6 `"             ! n.o di stazioni, 0 = tutte" >> estra_qaria.inp
	cat lista_stzqa.tmp | head -n ` expr $nlines - 3 ` | tail -n ` expr $nlines - 6 ` >> estra_qaria.inp
    fi
    
    cat <<EOF3 >> estra_qaria.inp
9               ! n.o di province, 0 = tutte
PC              ! codici prov. (una per riga; cancellare queste righe se 
PR              !   n.prov. = 0)
RE
MO
BO
FE
RA
FC
RN
1               ! unita' di misura (1=ug/m3)
2               ! 1=med.orarie, 2=med.giorn., 3=max.giorn.
1               ! validazione richiesta (vedi nota)
80.             ! % dati orari validi necessari per statistica giornaliera

-------------------------------------------------------
NOTE:
- validazione richiesta:
  0 tiene tutti i dati
  1 esclude i dati invalidati
  2 tiene solo i dati validati
  3 tiene solo i dati validati da operatore

- i files prodotti hanno orari LST; i valori orari si riferiscono alle
  concentrazioni medie nell'ora precedente (per ogni giornata sono quindi
  estratti i valori dalle 01 alle 24)
EOF3
fi

#--------------------------------------------------------------------------
# 2) Estrazione dati

# 2.1) Estrazione della lista di codici di configurazione
#      dei sensori (ID_CONFIG_SENSORE)
if [ $estra = "YES" ] ; then
    if [ ! -s estra_qaria.inp ] ; then
	echo
	echo "Attenzione:"
	echo "estra_qaria.inp non esiste o non e' valido"
	exit
    fi
    
    $scrive_stzqa_sql
    echo "creo l'anagrafica d'appoggio..."
    $dbqa <<EOF4 1>/dev/null
start stzqa
EOF4
    
    if [ ! -s stzqa.tmp ] ; then
	echo
	echo "Attenzione:"
	echo "problemi nell'estrazione dal DB"
	exit
    fi
    if [ `wc stzqa.tmp | awk '{print $1}'` -lt 4 ] ; then
	echo
	echo "Attenzione:"
	echo "nessuna stazione corrispondente alla richiesta"
	exit
    fi
    
# 2.2) Estrazione dei dati
    $scrive_dati_sql $tab
    echo "estraggo i dati dal DB, tabella "$tab" ..."
    $dbqa <<EOF5 1>/dev/null
start dati
EOF5
fi


#--------------------------------------------------------------------------
# 3) Riscrittura dati

if [ $estra = "YES" ] ; then
    if [ ! -s dati.tmp ] ; then
	echo
	echo "Attenzione:"
	echo "problemi nell'estrazione dal DB"
	exit
    fi
    if [ `wc dati.tmp | awk '{print $1}'` -lt 4 ] ; then
	echo
	echo "Attenzione:"
	echo "nessun dato nel DB"
	exit
    fi
    
    echo "preparo i file di output..."
    rm -f _nomectl.tmp
    cat dati.tmp | sed s/\,/\./g > dati.tmp.tmp
    mv dati.tmp.tmp dati.tmp

    if [ $lgrads = "YES" ] ; then
      $ordina_dati_tmp
      $stnmap -q -i `cat _nomectl.tmp`
      rm _nomectl.tmp
    else
      $ordina_dati_tmp -nograds
    fi
fi
